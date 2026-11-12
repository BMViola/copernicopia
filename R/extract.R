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
#' Daily and finer products are downloaded with one extra time step at each
#' end of the period, so the nearest time step is always among the files. For
#' monthly and yearly products, which are downloaded as whole months or years,
#' `"nearest"` takes the nearest of the downloaded time steps.
#'
#' **Space and depth.** Values come from the nearest grid cell; rows outside
#' the downloaded region get `NA`. For datasets with a depth axis, rows are
#' matched to the nearest depth level when `depth` names a column, and to the
#' shallowest downloaded level otherwise.
#'
#' Rows whose time falls outside the downloaded period, or on land, get `NA`.
#'
#' **Times as text.** Text times must be written year-month-day, such as
#' `"2025-07-15"`, `"2025-07-15 13:00"` or `"2025-07-15T13:00:05"`. They are
#' read as UTC unless a time zone follows (`"2025-07-15T13:00:00+10:00"`;
#' `"Z"` means UTC). Each value is read on its own, so a column may mix dates
#' with and without a time of day. Empty text is treated as missing. Any other
#' format, such as day/month/year, stops with an error rather than being
#' guessed: convert such a column first, for example with
#' `as.Date(x, format = "%d/%m/%Y")`. `Date` values are midnight UTC.
#'
#' @param data A data frame with longitude, latitude and time columns.
#' @param request The [cmems_request()] the files were downloaded with.
#' @param lon,lat,time Names of the longitude, latitude and time columns. If
#'   `NULL`, common names (`lon`, `longitude`, `lat`, `date`, `datetime`...)
#'   are tried. Times can be `Date`, `POSIXct` or text; see Details.
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
  depth_v <- if (is.null(depth)) NULL else as.numeric(data[[.depth_column(data, depth)]])

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
      depth_v, e$time_step_hours, time_match,
      .num_or_na(e$lon_step), .num_or_na(e$lat_step)
    )
    res <- lapply(res, .as_numeric_na)
    first_col <- NULL
    for (v in e$variables) {
      col <- v
      if (col %in% names(data)) col <- paste0(v, "_", .dataset_tag(e$dataset_id))
      data[[col]] <- res[[v]]
      if (is.null(first_col)) first_col <- col
      unit <- if (is.null(e$units)) NA_character_ else unname(e$units[v])
      # A column written again (the same request appended twice) is listed once
      if (!is.null(sources)) sources <- sources[sources$column != col, , drop = FALSE]
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

# The name of the depth column, checked to be a column of `data`.
.depth_column <- function(data, depth) {
  if (!is.character(depth) || length(depth) != 1 || is.na(depth) || !depth %in% names(data)) {
    stop(sprintf("Could not find the depth column %s in `data`; `depth` must name one of its columns.",
                 paste(dQuote(as.character(depth), FALSE), collapse = ", ")), call. = FALSE)
  }
  depth
}

.as_utc_ms <- function(x) {
  if (inherits(x, "POSIXt")) return(as.numeric(as.POSIXct(x)) * 1000)
  if (inherits(x, "Date")) return(as.numeric(x) * 86400000)
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) return(.text_to_utc_ms(x))
  stop("The time column must be Date, POSIXct or text like \"2025-07-15 13:00\".",
       call. = FALSE)
}

# A date written year-month-day ("2025-07-15" or "2025/07/15"), then
# optionally a time ("13:00", "13:00:05" or "13:00:05.5", after a space or a
# "T"), then optionally a time zone ("Z", "UTC", "+10:00", "-0930", "+10").
.time_pattern <- paste0(
  "^(\\d{4})[-/](\\d{1,2})[-/](\\d{1,2})",
  "(?:[T ]\\s*(\\d{1,2}):(\\d{2})(?::(\\d{2}(?:\\.\\d+)?))?)?",
  "\\s*(Z|UTC|GMT|[+-]\\d{2}(?::?\\d{2})?)?$"
)

# Text times as milliseconds since 1970-01-01 UTC. Every value is read on its
# own against .time_pattern, so a column can mix dates with and without a time
# of day (as write.csv() writes times that fall on midnight). Times are UTC
# unless a time zone follows. NA and empty text give NA. Anything else, such
# as day/month/year dates or text after the time, is an error rather than a
# guess: a time read wrongly would be matched to the wrong values.
.text_to_utc_ms <- function(x, what = "time") {
  x <- trimws(as.character(x))
  out <- rep(NA_real_, length(x))
  has <- !is.na(x) & nzchar(x)
  if (!any(has)) return(out)
  txt <- unique(x[has])
  ms <- .parse_time_text(txt)[match(x[has], txt)]
  bad <- is.na(ms)
  if (any(bad)) {
    stop("Could not read ", sum(bad), " of the ", length(ms), " ", what, " value(s), for example ",
         dQuote(x[has][bad][1], FALSE), ". Text must be written year-month-day, like ",
         "\"2025-07-15\" or \"2025-07-15 13:00\" (a time zone such as \"Z\" or \"+10:00\" may ",
         "follow). Convert other formats first, for example with ",
         "as.Date(x, format = \"%d/%m/%Y\") or ",
         "as.POSIXct(x, format = \"%d/%m/%Y %H:%M\", tz = \"UTC\").", call. = FALSE)
  }
  out[has] <- ms
  out
}

# Milliseconds since 1970-01-01 UTC for each text value, NA where the text is
# not a time as described at .time_pattern or names a date or time that does
# not exist (30 February, 25:00).
.parse_time_text <- function(txt) {
  ms <- rep(NA_real_, length(txt))
  # The usual forms ("2025-07-15", "2025-07-15 13:00", "2025-07-15 13:00:05")
  # are read in one go each
  plain <- grepl("^\\d{4}[-/]\\d{2}[-/]\\d{2}(?:[T ]\\d{2}:\\d{2}(?::\\d{2})?)?$", txt, perl = TRUE)
  if (any(plain)) {
    p <- chartr("/T", "- ", txt[plain])
    fmt <- c("%Y-%m-%d", "%Y-%m-%d %H:%M", "%Y-%m-%d %H:%M:%S")[match(nchar(p), c(10, 16, 19))]
    val <- rep(NA_real_, length(p))
    for (f in unique(fmt)) {
      val[fmt == f] <- as.numeric(as.POSIXct(p[fmt == f], tz = "UTC", format = f))
    }
    ms[plain] <- val * 1000
  }
  # Anything else that fits the pattern (a time zone, fractions of a second,
  # single-digit months, days or hours) is taken apart piece by piece
  other <- !plain & grepl(.time_pattern, txt, perl = TRUE, ignore.case = TRUE)
  if (any(other)) {
    part <- function(i) {
      sub(.time_pattern, paste0("\\", i), txt[other], perl = TRUE, ignore.case = TRUE)
    }
    num <- function(i) {
      v <- part(i)
      v[!nzchar(v)] <- "0"
      as.numeric(v)
    }
    # as.Date() gives NA for dates that do not exist, such as 30 February
    day <- as.numeric(as.Date(paste(part(1), part(2), part(3), sep = "-"), format = "%Y-%m-%d"))
    hour <- num(4)
    minute <- num(5)
    second <- num(6)
    zone <- part(7)
    offset_min <- rep(0, length(zone))
    signed <- grepl("^[+-]", zone)
    if (any(signed)) {
      digits <- gsub(":", "", substring(zone[signed], 2), fixed = TRUE)
      mins <- as.numeric(substr(digits, 1, 2)) * 60 +
        ifelse(nchar(digits) > 2, as.numeric(substr(digits, 3, 4)), 0)
      offset_min[signed] <- ifelse(startsWith(zone[signed], "+"), 1, -1) * mins
    }
    secs <- day * 86400 + hour * 3600 + minute * 60 + second - offset_min * 60
    secs[hour > 23 | minute > 59 | second >= 61] <- NA
    ms[other] <- secs * 1000
  }
  ms
}

# Dates (UTC) from Date, POSIXct or text values; text as in .text_to_utc_ms().
.as_dates <- function(x, what = "date") {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(as.POSIXct(x), tz = "UTC"))
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    return(as.Date(floor(.text_to_utc_ms(x, what) / 86400000), origin = "1970-01-01"))
  }
  stop("Could not read the ", what, " value(s): give dates as Date values or as text like ",
       "\"2025-07-15\".", call. = FALSE)
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
