#' The Copernicus Marine catalogue as a table
#'
#' Fetches the full Copernicus Marine catalogue and returns one row per
#' dataset and variable, for gridded datasets with latitude, longitude and
#' time axes. Fetching takes a few minutes, so the table is cached on disk
#' and reused until it is `max_age_days` old. While it is fetched, a timer
#' shows it is still working; press Esc to stop.
#'
#' @param refresh If `TRUE`, fetch a fresh copy even if a cached one exists.
#' @param max_age_days Age in days after which the cached copy is refreshed.
#' @return A data frame with columns including `product_title`, `product_doi`,
#'   `dataset_id`, `variable`,
#'   `standard_name`, `units`, `time_start`, `time_end`, `time_step_hours`,
#'   the spatial extent and resolution, and whether the dataset has a depth
#'   axis.
#' @export
cmems_catalogue <- function(refresh = FALSE, max_age_days = 30) {
  if (!refresh && !is.null(.cmems$catalogue)) return(.cmems$catalogue)
  cache_dir <- tools::R_user_dir("copernicopia", "cache")
  cache_file <- file.path(cache_dir, "catalogue.rds")
  if (!refresh && file.exists(cache_file)) {
    age <- as.numeric(difftime(Sys.time(), file.mtime(cache_file), units = "days"))
    cached <- readRDS(cache_file)
    # Copies saved by older versions of copernicopia lack some columns
    if (age <= max_age_days && all(.catalogue_columns %in% names(cached))) {
      .cmems$catalogue <- cached
      return(.cmems$catalogue)
    }
  }
  fetched <- .fetch_catalogue()
  cat_df <- .columns_to_df(fetched$columns)
  if (nrow(cat_df) == 0) stop("The catalogue came back empty; try again later.", call. = FALSE)
  skipped <- unlist(fetched$skipped)
  if (length(skipped)) {
    message(sprintf(
      "Note: %d dataset%s left out because %s catalogue entr%s could not be read: %s",
      length(skipped), if (length(skipped) == 1) " was" else "s were",
      if (length(skipped) == 1) "its" else "their", if (length(skipped) == 1) "y" else "ies",
      paste(skipped, collapse = ", ")))
  }
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(cat_df, cache_file)
  .cmems$catalogue <- cat_df
  cat_df
}

# Fetches the catalogue in a separate Python process (copernicopia_catalogue.py)
# while showing a timer: the toolbox's own progress bars print a new line for
# every update in the R console. Esc stops it. Returns list(columns, skipped).
.fetch_catalogue <- function() {
  .ensure_python()
  out <- tempfile("catalogue_", fileext = ".json")
  log <- tempfile("catalogue_", fileext = ".log")
  proc <- processx::process$new(
    reticulate::py_exe(),
    c(system.file("python", "copernicopia_catalogue.py", package = "copernicopia"), out,
      getOption("copernicopia.describe_function", "copernicusmarine:describe"),
      getOption("copernicopia.python_path", character(0))),
    stdout = NULL, stderr = log, cleanup = TRUE, cleanup_tree = TRUE, supervise = TRUE
  )
  status <- NULL
  on.exit({
    if (proc$is_alive()) {
      proc$kill_tree()
      if (!is.null(status)) status$finish("stopped")
    }
    unlink(c(out, log, paste0(out, ".part")))
  }, add = TRUE)

  prefix <- "Fetching the Copernicus Marine catalogue ... "
  live <- .live_progress()
  if (live) status <- .live_line(prefix)
  if (!live) message(prefix, "(this takes a few minutes)")
  started <- Sys.time()
  elapsed <- function() as.numeric(difftime(Sys.time(), started, units = "secs"))
  tick <- 0L
  while (proc$is_alive()) {
    proc$wait(500)
    if (live) {
      tick <- tick + 1L
      status$update(sprintf("%s %s", .spinner(tick), .fmt_secs(elapsed())))
    }
  }
  if (proc$get_exit_status() != 0 || !file.exists(out)) {
    why <- if (file.exists(log)) readLines(log, warn = FALSE) else character(0)
    why <- utils::tail(why[nzchar(trimws(why))], 1)
    if (!length(why)) why <- sprintf("exit status %s", format(proc$get_exit_status()))
    if (live) status$finish("failed")
    stop("Could not fetch the Copernicus Marine catalogue: ", why, call. = FALSE)
  }
  done <- sprintf("done (%s). Saved for the next 30 days.", .fmt_secs(elapsed()))
  if (live) status$finish(done) else message(done)
  .helpers()$read_json(out)
}

.catalogue_columns <- c("product_id", "product_title", "product_doi", "dataset_id",
                        "version", "variable", "standard_name", "units", "time_step_hours",
                        "lon_step", "lat_step", "has_depth")

# Named list of columns from Python (see to_columns() in copernicopia_helpers.py) to a
# data frame, with NaN turned into NA.
.columns_to_df <- function(cols) {
  cat_df <- as.data.frame(lapply(cols, unlist), stringsAsFactors = FALSE)
  num <- vapply(cat_df, is.numeric, logical(1))
  cat_df[num] <- lapply(cat_df[num], .as_numeric_na)
  cat_df
}

#' Common marine variables
#'
#' The shortlist shown in step B of [cmems_wizard()]. Each entry matches
#' catalogue variables by their CF standard name using a regular expression.
#' Typing an entry's name in the wizard's search (for example
#' `mixed layer depth`) finds the same variables. Any other variable in the
#' catalogue can be found with the search option too.
#'
#' @return A data frame with columns `label` and `pattern`.
#' @export
cmems_variables <- function() {
  data.frame(
    label = c(
      "Sea surface temperature",
      "Sea water temperature (with depth)",
      "Salinity",
      "Sea surface height / sea level",
      "Mixed layer depth",
      "Ocean currents",
      "Chlorophyll-a",
      "Phytoplankton groups",
      "Zooplankton",
      "Primary production",
      "Nutrients (nitrate, phosphate, silicate, iron, ammonium)",
      "Dissolved oxygen",
      "Carbonate system (pH, CO2, alkalinity)",
      "Sea ice concentration",
      "Sea ice thickness",
      "Sea ice edge / type",
      "Sea ice drift",
      "Wind and wind stress",
      "Waves",
      "Water clarity and optics",
      "Bottom temperature",
      "Photosynthetically available radiation (PAR)",
      "Particulate organic carbon (POC)",
      "Surface heat flux"
    ),
    pattern = c(
      "^sea_surface_(foundation_|subskin_|skin_)?temperature$",
      "^sea_water_(potential_|conservative_)?temperature$",
      "^sea_(water|surface)_(practical_|absolute_)?salinity$",
      "^sea_surface_height_above_(geoid|sea_level)$",
      "^ocean_mixed_layer_thickness",
      "(eastward|northward)_sea_water_velocity",
      "chlorophyll",
      "phytoplankton|diatom|dinophyte|haptophyte|prochlorococcus|prokaryote|green_algae",
      "zooplankton",
      "primary_production|primary_productivity",
      "nitrate|phosphate|silicate|iron_in_sea_water|ammonium",
      "oxygen_in_sea_water",
      "sea_water_ph|carbon_dioxide|dissolved_inorganic_carbon|alkalinity",
      "^sea_ice_area_fraction$",
      "^sea_ice_thickness$",
      "^sea_ice_(classification|edge|type|extent)",
      "sea_ice.*(velocity|displacement|drift)",
      "wind|surface_downward_(eastward|northward)_stress",
      "wave",
      "attenuation|turbidity|secchi|suspended|reflectance|upwelling_radiance_emerging_from_sea_water",
      "temperature_at_sea_floor",
      "photosynthetic",
      "particulate_organic",
      "heat_flux"
    ),
    stringsAsFactors = FALSE
  )
}

# Abbreviations recognised by the wizard's search. Each row: the terms (as
# typed, ignoring case, spaces, hyphens and underscores), what they find, and
# a regular expression matched against the variable's CF standard name. A
# variable whose short name is exactly the term is found too.
.search_terms <- function() {
  rows <- list(
    c("chla, chl-a, chlorophyll-a", "Chlorophyll-a", "chlorophyll"),
    c("sst", "Sea surface temperature", "^sea_surface_(foundation_|subskin_|skin_)?temperature$"),
    c("sbt, bottom temperature", "Temperature at the sea floor", "temperature_at_sea_floor"),
    c("sss", "Sea surface salinity",
      "^sea_(surface|water)_(practical_|absolute_)?salinity$"),
    c("ssh", "Sea surface height (ADT and SLA)", "^sea_surface_height_above_(geoid|sea_level)$"),
    c("adt", "Absolute dynamic topography", "^sea_surface_height_above_geoid$"),
    c("sla", "Sea level anomaly", "^sea_surface_height_above_sea_level$"),
    c("mld", "Mixed layer depth", "^ocean_mixed_layer_thickness"),
    c("current, currents, uv", "Ocean currents", "(eastward|northward)_sea_water_velocity"),
    c("do, o2, dox", "Dissolved oxygen", "oxygen_in_sea_water"),
    c("ph", "pH", "^sea_water_ph"),
    c("co2", "Carbon dioxide (pCO2, air-sea flux)", "carbon_dioxide"),
    c("pco2, spco2", "Partial pressure of CO2", "partial_pressure_of_carbon_dioxide"),
    c("dic", "Dissolved inorganic carbon", "dissolved_inorganic_carbon"),
    c("ta, talk", "Total alkalinity", "alkalinity"),
    c("no3", "Nitrate", "nitrate"),
    c("po4", "Phosphate", "phosphate"),
    c("si, sio4", "Silicate", "silicate"),
    c("fe", "Dissolved iron", "iron_in_sea_water"),
    c("nh4", "Ammonium", "ammonium"),
    c("nutrients", "Nitrate, phosphate, silicate, iron and ammonium",
      "nitrate|phosphate|silicate|iron_in_sea_water|ammonium"),
    c("pp, npp, nppv", "Primary production", "primary_production|primary_productivity"),
    c("kd, kd490", "Light attenuation (Kd)", "attenuation"),
    c("par", "Photosynthetically available radiation", "photosynthetic"),
    c("poc", "Particulate organic carbon", "particulate_organic"),
    c("spm, tsm", "Suspended matter", "suspended"),
    c("rrs", "Remote-sensing reflectance",
      "reflectance|upwelling_radiance_emerging_from_sea_water"),
    c("swh, hs", "Significant wave height", "significant_height"),
    c("wind stress, tau", "Wind stress",
      "surface_downward_(eastward|northward)_stress|wind_stress"),
    c("u10, v10", "Wind components", "^(eastward|northward)_wind$"),
    c("sic", "Sea ice concentration", "^sea_ice_area_fraction$"),
    c("sit", "Sea ice thickness", "^sea_ice_thickness$"),
    c("qnet, heat flux", "Surface heat flux", "heat_flux")
  )
  data.frame(terms = vapply(rows, `[`, "", 1), finds = vapply(rows, `[`, "", 2),
             pattern = vapply(rows, `[`, "", 3), stringsAsFactors = FALSE)
}

# Letters and digits only, in lower case: "Chl-a", "chl a" and "CHL_A" all
# become "chla", and "Sea surface height / sea level" becomes
# "seasurfaceheightsealevel".
.normalise_term <- function(x) gsub("[^a-z0-9]+", "", tolower(x))

# The names a shortlist entry or abbreviation can be searched by: the full
# name, the name without its bracketed part ("Nutrients"), the bracketed part
# if it is a single abbreviation ("PAR"), and either side of a " / " when
# that side is more than one word ("Sea surface height", "sea level").
.name_variants <- function(name) {
  base <- trimws(sub("\\s*\\(.*\\)\\s*$", "", name))
  inside <- regmatches(name, regexpr("(?<=\\()[^,() ]+(?=\\)$)", name, perl = TRUE))
  parts <- trimws(strsplit(base, " / ", fixed = TRUE)[[1]])
  parts <- parts[lengths(strsplit(parts, " ")) > 1]
  unique(c(name, base, inside, parts))
}

# What a search keyword stands for: list(finds, pattern), or NULL if it is not
# a shortlist name or a known abbreviation. Checked in this order: shortlist
# names, abbreviations, then the full names of the abbreviations
# ("sea level anomaly").
.find_search_term <- function(keyword) {
  k <- .normalise_term(keyword)
  if (!nzchar(k)) return(NULL)
  groups <- cmems_variables()
  for (i in seq_len(nrow(groups))) {
    if (k %in% .normalise_term(.name_variants(groups$label[i]))) {
      return(list(finds = groups$label[i], pattern = groups$pattern[i]))
    }
  }
  st <- .search_terms()
  terms <- lapply(strsplit(st$terms, ",", fixed = TRUE), .normalise_term)
  hit <- which(vapply(terms, function(t) k %in% t, logical(1)))
  if (length(hit) == 0) {
    hit <- which(vapply(st$finds, function(f) k %in% .normalise_term(.name_variants(f)),
                        logical(1)))
  }
  if (length(hit) == 0) return(NULL)
  list(finds = st$finds[hit[1]], pattern = st$pattern[hit[1]])
}

.step_label <- function(hours) {
  vapply(hours, function(h) {
    if (is.na(h)) return("unknown step")
    if (abs(h - 1) < 0.1) return("hourly")
    if (h < 24 * 0.8) return(sprintf("%g-hourly", round(h, 1)))
    if (h <= 28) return("daily")
    if (h >= 24 * 360) return("yearly")
    if (h >= 24 * 27) return("monthly")
    if (abs(h - 24 * 7) < 12) return("weekly")
    sprintf("%g-day", round(h / 24, 1))
  }, character(1))
}

# One row per dataset for the catalogue rows in `rows`, with the matched
# variables collapsed, ready to show as a numbered list.
.dataset_summary <- function(cat_df, rows, period = NULL) {
  sub <- cat_df[rows, , drop = FALSE]
  ids <- unique(sub$dataset_id)
  out <- do.call(rbind, lapply(ids, function(id) {
    d <- sub[sub$dataset_id == id, , drop = FALSE]
    data.frame(
      dataset_id = id,
      variables = paste(unique(d$variable), collapse = ", "),
      product_title = d$product_title[1],
      processing_level = d$processing_level[1],
      time_start = d$time_start[1],
      time_end = d$time_end[1],
      time_step_hours = d$time_step_hours[1],
      resolution = d$lat_step[1],
      has_depth = any(d$has_depth),
      stringsAsFactors = FALSE
    )
  }))
  out$covers <- NA
  if (!is.null(period)) {
    start <- as.Date(substr(out$time_start, 1, 10))
    end <- as.Date(substr(out$time_end, 1, 10))
    out$covers <- !is.na(start) & !is.na(end) &
      start <= period[1] & end >= period[2]
    out <- out[order(!out$covers, out$dataset_id), , drop = FALSE]
  } else {
    out <- out[order(out$dataset_id), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

# Degree sign, or "deg" on consoles that cannot show it.
.deg <- function() if (isTRUE(l10n_info()[["UTF-8"]])) "\u00b0" else " deg"

.format_dataset_line <- function(i, d) {
  res <- if (is.na(d$resolution)) "?" else sprintf("%.3g%s", d$resolution, .deg())
  cover <- if (isTRUE(d$covers)) "  [covers your dates]" else ""
  sprintf("[%d] %s%s\n     %s | %s | %s to %s | %s%s\n     %s\n",
          i, d$dataset_id, cover,
          d$variables, .step_label(d$time_step_hours),
          substr(d$time_start, 1, 10), substr(d$time_end, 1, 10),
          res, if (d$has_depth) " | has depth" else "",
          d$product_title)
}
