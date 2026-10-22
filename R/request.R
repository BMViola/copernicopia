#' Describe a Copernicus Marine download
#'
#' Bundles everything needed to download data and append it to your own data:
#' which variables to take from which datasets, the region, the time period
#' and (for 3D datasets) the depth range. [cmems_wizard()] builds one of these
#' for you, and printing it shows the code to recreate it, so a wizard session
#' can be rerun as a script.
#'
#' @param datasets A named list: names are dataset IDs and values are the
#'   variable names to download from each, for example
#'   `list("cmems_mod_glo_phy_my_0.083deg_P1D-m" = c("thetao", "zos"))`.
#' @param lon The western and eastern edges of the region, `c(west, east)`,
#'   in degrees (-180 to 180, or 0 to 360). The region runs east from the
#'   first longitude to the second, so it can cross the 180 degree meridian:
#'   `c(170, -170)` (or `c(170, 190)`) is the 20 degree strip across 180,
#'   while `c(-170, 170)` is everything except that strip.
#' @param lat The southern and northern edges of the region, `c(south, north)`,
#'   in degrees.
#' @param time Start and end dates, `c("YYYY-MM-DD", "YYYY-MM-DD")`.
#' @param depth Depth range in metres, `c(min, max)`, used for datasets with a
#'   depth axis. Either one range for all of them, or a named list with one
#'   range per dataset ID.
#' @param chunk How to split the download into files: `"month"` (the
#'   default) for one file per calendar month, `"day"` for one file per day,
#'   `"year"` for one file per calendar year, or `"all"` for a single file
#'   covering the whole period. Products are never split more finely than
#'   their own time step: monthly data asked for by `"day"` are split by month
#'   instead, and yearly data by year.
#' @param out_dir Folder for the downloaded files.
#' @param catalogue Catalogue table used to look up each dataset's time step,
#'   depth axis and units. Defaults to [cmems_catalogue()].
#' @return An object of class `cmems_request`.
#' @export
cmems_request <- function(datasets, lon, lat, time, depth = c(0, 1),
                          chunk = c("month", "day", "year", "all"),
                          out_dir = "cmems_data", catalogue = NULL) {
  chunk <- match.arg(chunk)
  if (!is.list(datasets) || is.null(names(datasets)) || any(!nzchar(names(datasets)))) {
    stop("`datasets` must be a named list of variable names, named by dataset ID.",
         call. = FALSE)
  }
  lon <- .normalise_lon(lon)
  lat <- as.numeric(lat)
  # A wide range that crosses 180 is more often two edges swapped by mistake
  if (.lon_crosses_180(lon) && .lon_width(lon) > 180) {
    message(sprintf(paste0(
      "Note: lon = c(%s, %s) runs east from %s across 180 to %s: %s. ",
      "If you meant the region between them that does not cross 180, use lon = c(%s, %s)."),
      format(lon[1]), format(lon[2]), .lon_deg(lon[1]), .lon_deg(lon[2]), .lon_label(lon),
      format(lon[2]), format(lon[1])))
  }
  if (length(lat) != 2 || anyNA(lat) || lat[1] >= lat[2] || any(abs(lat) > 90)) {
    stop("`lat` must be c(min, max) with min < max, between -90 and 90.", call. = FALSE)
  }
  time <- as.Date(time)
  if (length(time) != 2 || anyNA(time) || time[1] > time[2]) {
    stop("`time` must be c(start, end) dates with start <= end.", call. = FALSE)
  }
  if (is.null(catalogue)) catalogue <- cmems_catalogue()

  entries <- lapply(names(datasets), function(id) {
    vars <- unique(as.character(datasets[[id]]))
    meta <- catalogue[catalogue$dataset_id == id, , drop = FALSE]
    if (nrow(meta) == 0) {
      warning("Dataset '", id, "' is not in the catalogue; check the ID.", call. = FALSE)
    }
    missing <- setdiff(vars, meta$variable)
    if (nrow(meta) > 0 && length(missing) > 0) {
      warning("Not found in '", id, "': ", paste(missing, collapse = ", "), call. = FALSE)
    }
    has_depth <- nrow(meta) > 0 && any(meta$has_depth)
    d <- if (is.list(depth)) depth[[id]] else depth
    if (has_depth && is.null(d)) d <- c(0, 1)
    units <- meta$units[match(vars, meta$variable)]
    standard_names <- meta$standard_name[match(vars, meta$variable)]
    first <- function(col) {
      if (nrow(meta) > 0 && col %in% names(meta)) as.character(meta[[col]][1]) else NA_character_
    }
    list(
      dataset_id = id,
      variables = vars,
      product_id = first("product_id"),
      product_title = first("product_title"),
      product_doi = first("product_doi"),
      version = first("version"),
      production_center = first("production_center"),
      processing_level = first("processing_level"),
      standard_names = stats_setnames(standard_names, vars),
      time_step_hours = if (nrow(meta) > 0) meta$time_step_hours[1] else NA_real_,
      lon_step = if (nrow(meta) > 0) meta$lon_step[1] else NA_real_,
      lat_step = if (nrow(meta) > 0) meta$lat_step[1] else NA_real_,
      has_depth = has_depth,
      depth = if (has_depth) as.numeric(d) else NULL,
      units = stats_setnames(units, vars)
    )
  })
  names(entries) <- names(datasets)
  structure(
    list(datasets = entries, lon = lon, lat = lat, time = time,
         chunk = chunk, out_dir = out_dir),
    class = "cmems_request"
  )
}

stats_setnames <- function(x, nm) {
  names(x) <- nm
  x
}

.deparse_vec <- function(x) {
  if (is.character(x)) {
    q <- paste0('"', x, '"')
    if (length(q) == 1) q else paste0("c(", paste(q, collapse = ", "), ")")
  } else {
    x <- as.character(x)
    if (length(x) == 1) x else paste0("c(", paste(x, collapse = ", "), ")")
  }
}

#' @export
format.cmems_request <- function(x, ...) {
  ds_lines <- vapply(x$datasets, function(e) {
    sprintf('    "%s" = %s', e$dataset_id, .deparse_vec(e$variables))
  }, character(1))
  depths <- Filter(Negate(is.null), lapply(x$datasets, `[[`, "depth"))
  depth_txt <- if (length(depths) == 0) {
    NULL
  } else if (length(unique(depths)) == 1) {
    sprintf("  depth = %s,", .deparse_vec(depths[[1]]))
  } else {
    inner <- vapply(names(depths), function(n) {
      sprintf('    "%s" = %s', n, .deparse_vec(depths[[n]]))
    }, character(1))
    c("  depth = list(", paste(inner, collapse = ",\n"), "  ),")
  }
  c(
    "req <- cmems_request(",
    "  datasets = list(",
    paste(ds_lines, collapse = ",\n"),
    "  ),",
    sprintf("  lon = %s, lat = %s,", .deparse_vec(x$lon), .deparse_vec(x$lat)),
    sprintf("  time = %s,", .deparse_vec(format(x$time))),
    depth_txt,
    sprintf('  chunk = "%s",', x$chunk),
    sprintf('  out_dir = "%s"', gsub("\\\\", "/", x$out_dir)),
    ")"
  )
}

#' @export
print.cmems_request <- function(x, ...) {
  cat("<cmems_request>\n")
  for (e in x$datasets) {
    cat(sprintf("  %s: %s (%s%s)\n", e$dataset_id, paste(e$variables, collapse = ", "),
                .step_label(e$time_step_hours),
                if (e$has_depth) sprintf(", depth %g-%g m", e$depth[1], e$depth[2]) else ""))
  }
  cat(sprintf("  Region: longitude %s; latitude %g to %g\n", .lon_label(x$lon), x$lat[1], x$lat[2]))
  cat(sprintf("  Period: %s to %s\n", x$time[1], x$time[2]))
  cat(sprintf("  Files: %s\n", .chunk_label(x$chunk)))
  cat(sprintf("  Folder: %s\n", x$out_dir))
  cat("\nCode to recreate this request:\n\n")
  cat(format(x), sep = "\n")
  invisible(x)
}

# Folder for one dataset's files. The suffix is a short hash of the variables,
# region, depth and file split, so files from different requests never get
# mixed up.
.dataset_dir <- function(request, entry) {
  key <- paste(entry$dataset_id, paste(sort(entry$variables), collapse = ","),
               paste(request$lon, collapse = ","), paste(request$lat, collapse = ","),
               paste(entry$depth, collapse = ","), request$chunk, sep = "|")
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(key, tmp)
  hash <- substr(unname(tools::md5sum(tmp)), 1, 8)
  file.path(path.expand(request$out_dir), paste0(entry$dataset_id, "_", hash))
}

.chunk_units <- c("day", "month", "year", "all")

.chunk_label <- function(chunk) {
  c(day = "one file per day", month = "one file per month", year = "one file per year",
    all = "a single file for the whole period")[[chunk]]
}

.month_start <- function(d) as.Date(format(d, "%Y-%m-01"))
.month_end <- function(d) seq(.month_start(d), by = "month", length.out = 2)[2] - 1
.year_start <- function(d) as.Date(format(d, "%Y-01-01"))

# The finest split that makes sense for a product's time step.
.min_chunk <- function(time_step_hours) {
  if (is.na(time_step_hours) || time_step_hours <= 25) return("day")
  if (time_step_hours < 24 * 360) return("month")
  "year"
}

.effective_chunk <- function(chunk, time_step_hours) {
  .chunk_units[max(match(chunk, .chunk_units),
                   match(.min_chunk(time_step_hours), .chunk_units))]
}

# Dates to download. For products coarser than daily, the period is widened
# to whole calendar months or years (or by one time step for 8-day and
# similar products), so every observation date has its time step available.
.download_period <- function(time, time_step_hours) {
  start <- as.Date(time[1])
  end <- as.Date(time[2])
  if (is.na(time_step_hours) || time_step_hours <= 25) return(c(start, end))
  if (time_step_hours >= 24 * 360) {
    return(as.Date(c(format(start, "%Y-01-01"), format(end, "%Y-12-31"))))
  }
  if (time_step_hours >= 24 * 27) return(c(.month_start(start), .month_end(end)))
  pad <- ceiling(time_step_hours / 24)
  c(start - pad, end + pad)
}

# One row per file to download: start and end dates, and the split unit used.
.time_chunks <- function(time, time_step_hours, chunk = "month") {
  unit <- .effective_chunk(chunk, time_step_hours)
  period <- .download_period(time, time_step_hours)
  start <- period[1]
  end <- period[2]
  if (unit == "all") {
    return(data.frame(start = start, end = end, unit = unit))
  }
  if (unit == "day") {
    days <- seq(start, end, by = "day")
    return(data.frame(start = days, end = days, unit = unit))
  }
  floor_fun <- if (unit == "month") .month_start else .year_start
  firsts <- seq(floor_fun(start), floor_fun(end), by = unit)
  nexts <- seq(firsts[1], by = unit, length.out = length(firsts) + 1)[-1]
  data.frame(start = pmax(firsts, start), end = pmin(nexts - 1, end), unit = unit)
}

# Whole days, months and years are named by that unit (dataset_20250715.nc,
# dataset_202507.nc, dataset_2025.nc); anything else by its exact dates, so a
# partial month or year is never mistaken for a full one.
.chunk_filename <- function(entry, chunk) {
  id <- entry$dataset_id
  s <- chunk$start
  e <- chunk$end
  if (chunk$unit == "day") {
    return(sprintf("%s_%s.nc", id, format(s, "%Y%m%d")))
  }
  if (chunk$unit == "month" && s == .month_start(s) && e == .month_end(s)) {
    return(sprintf("%s_%s.nc", id, format(s, "%Y%m")))
  }
  if (chunk$unit == "year" && format(s, "%m%d") == "0101" && format(e, "%m%d") == "1231") {
    return(sprintf("%s_%s.nc", id, format(s, "%Y")))
  }
  sprintf("%s_%s-%s.nc", id, format(s, "%Y%m%d"), format(e, "%Y%m%d"))
}
