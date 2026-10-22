#' Append Copernicus Marine values to your data
#'
#' For each row of `data`, looks up the downloaded files for every dataset in
#' `request` and adds a column per variable with the value from the grid cell
#' containing the row's position, at the matching time step.
#'
#' **Time matching.** With `time_match = "period"` (the default), daily data
#' are matched on the calendar day (UTC), monthly data on the calendar month
#' and yearly data on the year, so an observation at 15:00 on 10 March gets
#' that day's (or that month's) value. Other resolutions (hourly, 3-hourly,
#' 8-day...) use the nearest time step. `time_match = "nearest"` always uses
#' the nearest time step. Matches more than one time step away are rejected.
#'
#' **Space and depth.** Values come from the nearest grid cell; rows outside
#' the downloaded region get `NA`. For datasets with a depth axis, rows are
#' matched to the nearest depth level when `depth` names a column, and to the
#' shallowest downloaded level otherwise.
#'
#' Rows whose time falls outside the downloaded period, or on land, get `NA`.
#'
#' @param data A data frame with longitude, latitude and time columns.
#' @param request The [cmems_request()] the files were downloaded with.
#' @param lon,lat,time Names of the longitude, latitude and time columns. If
#'   `NULL`, common names (`lon`, `longitude`, `lat`, `date`, `datetime`...)
#'   are tried. Times can be `Date`, `POSIXct` or text such as
#'   `"2025-07-15 13:00"`; text and `Date` values are read as UTC.
#' @param depth Optional name of a depth column (metres, positive down).
#' @param time_match `"period"` or `"nearest"`; see Details.
#' @param keep_matched_time If `TRUE`, also add a column per dataset with the
#'   dataset time step each row was matched to.
#' @return `data` with the new columns. The attribute `"cmems_sources"` lists
#'   the dataset, variable and units behind each new column (see
#'   [cmems_sources()]), and the request is kept in `"cmems_request"`, so
#'   [cmems_getreadme()] can list the products to cite.
#' @export
cmems_extract <- function(data, request, lon = NULL, lat = NULL, time = NULL,
                          depth = NULL, time_match = c("period", "nearest"),
                          keep_matched_time = FALSE) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!inherits(request, "cmems_request")) {
    stop("`request` must be created with cmems_request() or cmems_wizard().", call. = FALSE)
  }
  time_match <- match.arg(time_match)
  cols <- .data_columns(data, lon, lat, time)
  lon_v <- as.numeric(data[[cols$lon]])
  lat_v <- as.numeric(data[[cols$lat]])
  time_ms <- .as_utc_ms(data[[cols$time]])
  depth_v <- if (is.null(depth)) NULL else as.numeric(data[[depth]])

  sources <- attr(data, "cmems_sources")
  for (e in request$datasets) {
    dir <- .dataset_dir(request, e)
    files <- list.files(dir, pattern = "\\.nc$", full.names = TRUE)
    files <- files[!startsWith(basename(files), "tmp_")]
    if (length(files) == 0) {
      warning("No downloaded files for ", e$dataset_id, "; run cmems_download(request) first.",
              call. = FALSE)
      next
    }
    res <- .helpers()$extract_points(
      as.list(files), as.list(e$variables), lon_v, lat_v, time_ms,
      depth_v, e$time_step_hours, time_match
    )
    res <- lapply(res, .as_numeric_na)
    first_col <- NULL
    for (v in e$variables) {
      col <- v
      if (col %in% names(data)) col <- paste0(v, "_", .dataset_tag(e$dataset_id))
      data[[col]] <- res[[v]]
      if (is.null(first_col)) first_col <- col
      unit <- if (is.null(e$units)) NA_character_ else unname(e$units[v])
      sources <- rbind(sources, data.frame(
        column = col, dataset_id = e$dataset_id, variable = v, units = unit,
        stringsAsFactors = FALSE
      ))
    }
    if (keep_matched_time) {
      data[[paste0(first_col, "_matched_time")]] <-
        as.POSIXct(res$matched_time_ms / 1000, origin = "1970-01-01", tz = "UTC")
    }
    n_ok <- sum(!is.na(res[[e$variables[1]]]))
    n_time <- sum(!is.na(res$matched_time_ms))
    message(sprintf("%s: values for %d of %d rows%s", e$dataset_id, n_ok, nrow(data),
                    if (n_time < nrow(data))
                      sprintf(" (%d outside the downloaded period)", nrow(data) - n_time)
                    else ""))
  }
  rownames(sources) <- NULL
  attr(data, "cmems_sources") <- sources
  attr(data, "cmems_request") <- request
  data
}

.column_candidates <- list(
  lon = c("lon", "long", "longitude", "decimallongitude", "lon_dd", "x"),
  lat = c("lat", "latitude", "decimallatitude", "lat_dd", "y"),
  time = c("time", "datetime", "date_time", "date", "timestamp", "datetime_utc",
           "time_utc", "date_utc")
)

.guess_column <- function(nms, what) {
  hit <- match(.column_candidates[[what]], tolower(nms))
  hit <- hit[!is.na(hit)]
  if (length(hit) == 0) NULL else nms[hit[1]]
}

.data_columns <- function(data, lon, lat, time) {
  out <- list(lon = lon, lat = lat, time = time)
  for (what in names(out)) {
    if (is.null(out[[what]])) out[[what]] <- .guess_column(names(data), what)
    if (is.null(out[[what]]) || !out[[what]] %in% names(data)) {
      stop(sprintf("Could not find the %s column; name it with `%s = \"...\"`.",
                   what, what), call. = FALSE)
    }
  }
  out
}

.as_utc_ms <- function(x) {
  if (inherits(x, "POSIXt")) return(as.numeric(as.POSIXct(x)) * 1000)
  if (inherits(x, "Date")) return(as.numeric(x) * 86400000)
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    t <- as.POSIXct(x, tz = "UTC", tryFormats = c(
      "%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OS", "%Y/%m/%d %H:%M:%OS",
      "%Y-%m-%d %H:%M", "%Y-%m-%dT%H:%M", "%Y/%m/%d %H:%M", "%Y-%m-%d", "%Y/%m/%d"
    ))
    return(as.numeric(t) * 1000)
  }
  stop("The time column must be Date, POSIXct or text like \"2025-07-15 13:00\".",
       call. = FALSE)
}

.as_numeric_na <- function(x) {
  x <- as.numeric(unlist(x))
  x[is.nan(x)] <- NA
  x
}

.dataset_tag <- function(dataset_id) {
  gsub("[^A-Za-z0-9]+", "_", dataset_id)
}

#' Where each appended column came from
#'
#' @param data A data frame returned by [cmems_extract()] or [cmems_wizard()].
#' @return A data frame with the column name, dataset ID, variable and units
#'   of each appended column.
#' @export
cmems_sources <- function(data) {
  attr(data, "cmems_sources")
}
