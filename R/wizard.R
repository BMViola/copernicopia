#' Step-by-step download and extraction
#'
#' Walks you through getting Copernicus Marine data in the R console:
#'
#' * **A.** Log in (only the first time on a computer).
#' * **B.** Pick variables from a shortlist of common ones, or search the full
#'   catalogue.
#' * **C.** For each variable, pick the product (dataset) to take it from.
#' * **D.** Set the region's western, eastern, southern and northern edges.
#'   The region runs east from its western edge, so it can cross 180 degrees
#'   (see [cmems_request()]).
#' * **E.** Set the time period, how to split the download into files (one
#'   per day, month or year, or a single file), and a depth range for 3D
#'   products.
#' * **F.** Download the files and, if you supplied `data`, append the values
#'   to it.
#'
#' When you supply `data`, the region and time period default to its extent,
#' and products that cover its dates are listed first. Press Enter at any
#' prompt to accept the default shown in square brackets.
#'
#' The wizard prints the code for the request it builds, so you can rerun the
#' same download non-interactively with [cmems_request()],
#' [cmems_download()] and [cmems_extract()].
#'
#' @param data Optional data frame of observations with longitude, latitude
#'   and time columns.
#' @param lon,lat,time,depth Column names in `data`. If `NULL`, common names
#'   are tried and you are asked to confirm.
#' @param chunk How to split the download into files: `"month"`, `"day"`,
#'   `"year"` or `"all"` (see [cmems_request()]). If `NULL`, you are asked,
#'   with one file per month as the default.
#' @param out_dir Default folder for the downloaded files.
#' @param time_match Passed to [cmems_extract()].
#' @return If `data` was supplied and you chose to append values, `data` with
#'   the new columns (the request is kept in its `"cmems_request"`
#'   attribute). Otherwise the [cmems_request()], invisibly.
#' @export
#' @examples
#' \dontrun{
#' # Download only
#' req <- cmems_wizard()
#'
#' # Download and append to your own data
#' obs <- data.frame(
#'   lon = c(110.2, 140.5),
#'   lat = c(-50.1, -60.3),
#'   date = as.Date(c("2025-07-15", "2025-08-02"))
#' )
#' obs <- cmems_wizard(obs)
#' }
cmems_wizard <- function(data = NULL, lon = NULL, lat = NULL, time = NULL,
                         depth = NULL, chunk = NULL, out_dir = "cmems_data",
                         time_match = c("period", "nearest")) {
  time_match <- match.arg(time_match)
  if (!is.null(chunk)) chunk <- match.arg(chunk, .chunk_units)
  cat("\nCopernicus Marine download wizard\n")
  cat("Press Enter to accept a default shown in [brackets].\n")

  cols <- NULL
  extent <- NULL
  if (!is.null(data)) {
    if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
    .rule("Your data")
    cols <- .wizard_columns(data, lon, lat, time)
    extent <- .data_extent(data, cols, depth)
    shown_lon <- round(extent$lon, 2)
    cat(sprintf("%d rows: longitude %s; latitude %g to %g; %s to %s\n", nrow(data),
                if (shown_lon[1] == shown_lon[2]) .lon_deg(shown_lon[1])
                else .lon_label(shown_lon),
                round(extent$lat[1], 2), round(extent$lat[2], 2),
                extent$time[1], extent$time[2]))
  }

  .rule("A. Log in")
  cmems_login(ask = TRUE)
  catalogue <- cmems_catalogue()

  .rule("B. Choose variables")
  selections <- .choose_variables(catalogue)

  .rule("C. Choose a product for each variable")
  picks <- list()
  area <- NULL
  if (!is.null(extent) && all(is.finite(c(extent$lon, extent$lat)))) {
    area <- list(lon = extent$lon, lat = extent$lat)
  }
  for (s in selections) {
    p <- .choose_dataset(catalogue, s, extent$time, area)
    if (is.null(p)) next
    picks[[p$dataset_id]] <- union(picks[[p$dataset_id]], p$variables)
  }
  if (length(picks) == 0) {
    cat("No products were chosen, so there is nothing to download.\n")
    return(invisible(NULL))
  }

  .rule("D. Region")
  bounds <- .choose_bounds(extent)

  .rule("E. Time period")
  period <- .choose_period(catalogue, names(picks), extent$time)
  if (is.null(chunk)) chunk <- .choose_chunk(catalogue, names(picks))
  depth_range <- .choose_depth(catalogue, names(picks), extent$depth)

  .rule("F. Download")
  out_dir <- .ask_text("Folder for the downloaded files: ", default = out_dir)
  # Step D has already dealt with wide regions crossing 180, so the same note
  # from cmems_request() is not repeated here
  req <- withCallingHandlers(
    cmems_request(picks, bounds$lon, bounds$lat, period, depth = depth_range,
                  chunk = chunk, out_dir = out_dir, catalogue = catalogue),
    message = function(m) {
      if (startsWith(conditionMessage(m), "Note: lon = ")) invokeRestart("muffleMessage")
    }
  )
  cat("\n")
  print(req)
  cat("\n")
  if (.ask_yes_no("Check the products and estimate the download size first?", TRUE)) {
    cmems_download(req, dry_run = TRUE)
  }
  if (!.ask_yes_no("Download now?", TRUE)) {
    cat("Not downloaded. Run cmems_download(req) when you are ready.\n")
    return(invisible(req))
  }
  files <- cmems_download(req)
  if (isTRUE(attr(files, "cancelled"))) {
    cat("Nothing was appended. Run cmems_download(req) to resume the download.\n")
    return(invisible(req))
  }

  if (!is.null(data) && .ask_yes_no("Append the values to your data?", TRUE)) {
    out <- cmems_extract(data, req, cols$lon, cols$lat, cols$time, depth, time_match)
    attr(out, "cmems_request") <- req
    added <- setdiff(names(out), names(data))
    cat(sprintf("\nAdded column(s): %s\n", paste(added, collapse = ", ")))
    cat("Use cmems_sources() on the result to see where each column came from.\n")
    .cite_hint(out_dir)
    return(out)
  }
  .cite_hint(out_dir)
  invisible(req)
}

.cite_hint <- function(out_dir) {
  cat(sprintf("To cite these data, run cmems_getreadme(\"%s\").\n", gsub("\\\\", "/", out_dir)))
}

.wizard_columns <- function(data, lon, lat, time) {
  nms <- names(data)
  found <- list(lon = lon, lat = lat, time = time)
  for (what in names(found)) {
    if (is.null(found[[what]])) found[[what]] <- .guess_column(nms, what)
  }
  ok <- all(vapply(found, function(x) !is.null(x) && x %in% nms, logical(1)))
  if (ok) {
    cat(sprintf("Using columns: longitude = '%s', latitude = '%s', time = '%s'\n",
                found$lon, found$lat, found$time))
    if (.ask_yes_no("Is that right?", TRUE)) return(found)
  }
  cat("Columns in your data:\n")
  cat(sprintf("[%d] %s\n", seq_along(nms), nms), sep = "")
  labels <- c(lon = "longitude", lat = "latitude", time = "time")
  for (what in names(found)) {
    i <- .ask_numbers(sprintf("Which column is %s? ", labels[[what]]), length(nms),
                      multiple = FALSE)
    found[[what]] <- nms[i]
  }
  found
}

.data_extent <- function(data, cols, depth = NULL) {
  lon <- .lon_extent(as.numeric(data[[cols$lon]]))
  lat <- as.numeric(data[[cols$lat]])
  ms <- .as_utc_ms(data[[cols$time]])
  dates <- as.Date(as.POSIXct(ms / 1000, origin = "1970-01-01", tz = "UTC"))
  d <- if (is.null(depth)) NULL else range(as.numeric(data[[depth]]), na.rm = TRUE)
  list(lon = lon, lat = range(lat, na.rm = TRUE),
       time = range(dates, na.rm = TRUE), depth = d)
}

# Step B ---------------------------------------------------------------------

.choose_variables <- function(catalogue) {
  groups <- cmems_variables()
  n_ds <- vapply(groups$pattern, function(p) {
    length(unique(catalogue$dataset_id[grepl(p, catalogue$standard_name, ignore.case = TRUE)]))
  }, numeric(1))
  cat("Common variables:\n")
  cat(sprintf("[%2d] %s %s\n", seq_len(nrow(groups)),
              formatC(groups$label, width = -max(nchar(groups$label))),
              ifelse(n_ds > 0, sprintf("(%d product%s)", n_ds, ifelse(n_ds == 1, "", "s")),
                     "(none available)")), sep = "")
  cat("[ S] Search all variables by keyword\n")
  cat("[ A] List every variable in the catalogue\n")

  selections <- list()
  repeat {
    answer <- .read_input("\nEnter number(s) separated by commas, S to search or A to list all: ")
    new <- list()
    if (tolower(answer) == "s") {
      new <- .pick_standard_names(catalogue, .ask_text("Search for a variable (e.g. chla, sst, mld, nitrate, ice): "))
    } else if (tolower(answer) == "a") {
      new <- .pick_standard_names(catalogue, NULL)
    } else {
      nums <- .parse_numbers(answer)
      if (is.null(nums) || length(nums) == 0 || any(nums < 1 | nums > nrow(groups))) {
        cat(sprintf("Please enter numbers between 1 and %d, S or A.\n", nrow(groups)))
        next
      }
      for (i in nums) {
        rows <- which(grepl(groups$pattern[i], catalogue$standard_name, ignore.case = TRUE))
        if (length(rows) == 0) {
          cat(sprintf("No products found for '%s'; skipped.\n", groups$label[i]))
        } else {
          new[[length(new) + 1]] <- list(label = groups$label[i], rows = rows)
        }
      }
    }
    selections <- c(selections, new)
    if (length(selections) == 0) next
    cat("Selected:", paste(vapply(selections, `[[`, "", "label"), collapse = "; "), "\n")
    if (!.ask_yes_no("Add more variables?", FALSE)) break
  }
  selections
}

.pick_standard_names <- function(catalogue, keyword) {
  key <- ifelse(nzchar(catalogue$standard_name), catalogue$standard_name, catalogue$variable)
  term <- if (is.null(keyword)) NULL else .find_search_term(keyword)
  if (is.null(keyword)) {
    hits <- rep(TRUE, length(key))
  } else if (!is.null(term)) {
    # A shortlist name or known abbreviation: match by standard name, or an
    # exact short name
    if (.normalise_term(term$finds) == .normalise_term(keyword)) {
      cat(sprintf("Searching for %s.\n", term$finds))
    } else {
      cat(sprintf("Searching for %s (\"%s\").\n", term$finds, keyword))
    }
    hits <- grepl(term$pattern, catalogue$standard_name, ignore.case = TRUE) |
      .normalise_term(catalogue$variable) == .normalise_term(keyword)
  } else {
    pattern <- gsub("[ _]+", "[ _]", keyword)
    hits <- tryCatch(
      grepl(pattern, key, ignore.case = TRUE) |
        grepl(pattern, catalogue$variable, ignore.case = TRUE),
      error = function(e) grepl(keyword, key, fixed = TRUE)
    )
  }
  found <- sort(unique(key[hits]))
  if (length(found) == 0) {
    cat("No variables match that search.\n")
    return(list())
  }
  n_products <- function(rows) {
    n <- length(unique(catalogue$dataset_id[rows]))
    sprintf("%d product%s", n, if (n == 1) "" else "s")
  }
  choices <- lapply(found, function(k) {
    rows <- which(key == k)
    list(label = k, rows = rows,
         line = sprintf("%s (%s; %s)", k, paste(utils::head(unique(catalogue$variable[rows]), 3),
                                                collapse = ", "), n_products(rows)))
  })
  # A variable name typed in full (e.g. analysis_error) may be filed under
  # several standard names; offer all of them together as the first choice
  if (!is.null(keyword)) {
    same <- which(.normalise_term(catalogue$variable) == .normalise_term(keyword))
    if (length(unique(key[same])) > 1) {
      name <- catalogue$variable[same[1]]
      choices <- c(list(list(
        label = sprintf("%s (any standard name)", name), rows = same,
        line = sprintf("All variables named %s, whatever their standard name (%s)",
                       name, n_products(same))
      )), choices)
    }
  }
  cat(sprintf("[%d] %s\n", seq_along(choices), vapply(choices, `[[`, "", "line")), sep = "")
  picks <- .ask_numbers("Enter number(s), or press Enter to go back: ", length(choices),
                        default = integer(0))
  lapply(choices[picks], function(ch) list(label = ch$label, rows = ch$rows))
}

# Step C ---------------------------------------------------------------------

.choose_dataset <- function(catalogue, selection, period = NULL, area = NULL) {
  summ_all <- .dataset_summary(catalogue, selection$rows, period, area)
  # With the user's data, only products whose area overlaps it are listed at
  # first; ALL shows the rest
  in_area <- if (is.null(area)) rep(TRUE, nrow(summ_all)) else summ_all$in_area
  if (!any(in_area)) {
    cat(sprintf("\nNone of the products with %s covers your data's area, so all are listed.\n",
                selection$label))
    in_area[] <- TRUE
  }
  summ <- summ_all[in_area, , drop = FALSE]
  hidden <- function() nrow(summ_all) - nrow(summ)
  hidden_note <- function(n) {
    sprintf("(%d product%s outside your data's area %s hidden; type ALL to show %s.)\n",
            n, if (n == 1) "" else "s", if (n == 1) "is" else "are",
            if (n == 1) "it" else "them")
  }
  shown <- summ
  repeat {
    cat(sprintf("\nProducts with %s%s (%d):\n", selection$label,
                if (hidden() > 0) " covering your data's area" else "", nrow(shown)))
    for (i in seq_len(nrow(shown))) cat(.format_dataset_line(i, shown[i, ]))
    if (hidden() > 0) cat(hidden_note(hidden()))
    answer <- .read_input(if (nrow(shown) == 1) {
      "Press Enter to choose it, type text to filter the list, or 0 to skip this variable: "
    } else {
      "Enter a number, type text to filter the list (Enter shows all), or 0 to skip this variable: "
    })
    if (hidden() > 0 && toupper(trimws(answer)) == "ALL") {
      summ <- summ_all
      shown <- summ
      next
    }
    if (!nzchar(answer)) {
      if (nrow(shown) == 1) {
        num <- 1L
        break
      }
      shown <- summ
      next
    }
    num <- .parse_numbers(answer)
    if (!is.null(num)) {
      if (identical(as.integer(num), 0L)) {
        cat(sprintf("Skipped %s.\n", selection$label))
        return(NULL)
      }
      if (length(num) == 1 && num >= 1 && num <= nrow(shown)) break
      cat(sprintf("Please enter one number between 1 and %d, or 0 to skip.\n", nrow(shown)))
      next
    }
    text <- paste(summ$dataset_id, summ$product_title, summ$variables,
                  .step_label(summ$time_step_hours), summ$processing_level)
    hit <- grepl(tolower(answer), tolower(text), fixed = TRUE) |
      tryCatch(grepl(answer, text, ignore.case = TRUE), error = function(e) FALSE)
    if (!any(hit)) {
      text_all <- paste(summ_all$dataset_id, summ_all$product_title)
      n_elsewhere <- if (hidden() > 0) {
        sum(grepl(tolower(answer), tolower(text_all), fixed = TRUE) & !summ_all$in_area)
      } else 0
      if (n_elsewhere > 0) {
        cat(sprintf("Nothing in the list matches that text, but %d product%s outside your data's area %s; type ALL to show %s.\n",
                    n_elsewhere, if (n_elsewhere == 1) "" else "s",
                    if (n_elsewhere == 1) "does" else "do", if (n_elsewhere == 1) "it" else "them"))
      } else {
        cat("Nothing matches that text; showing the full list.\n")
      }
      shown <- summ
    } else {
      shown <- summ[hit, , drop = FALSE]
    }
  }
  choice <- shown[num, ]
  rows <- selection$rows[catalogue$dataset_id[selection$rows] == choice$dataset_id]
  vars <- unique(catalogue$variable[rows])
  if (length(vars) > 1) {
    meta <- catalogue[rows, , drop = FALSE]
    meta <- meta[!duplicated(meta$variable), , drop = FALSE]
    cat("This product has several matching variables:\n")
    cat(sprintf("[%d] %s - %s (%s)\n", seq_along(vars), meta$variable,
                meta$standard_name, meta$units), sep = "")
    picks <- .ask_numbers("Which ones? Enter number(s), or press Enter for all: ",
                          length(vars), default = seq_along(vars))
    vars <- vars[picks]
  }
  if (!is.null(period) && !isTRUE(choice$covers)) {
    cat(sprintf("Note: this product covers %s to %s, which does not span all your dates.\n",
                substr(choice$time_start, 1, 10), substr(choice$time_end, 1, 10)))
  }
  cat(sprintf("-> %s: %s\n", choice$dataset_id, paste(vars, collapse = ", ")))
  list(dataset_id = choice$dataset_id, variables = vars)
}

# Steps D and E --------------------------------------------------------------

.choose_bounds <- function(extent = NULL) {
  dlon <- dlat <- NULL
  if (!is.null(extent)) {
    # Data extent plus a margin, rounded outwards to 0.1 degrees
    padded <- .lon_pad(extent$lon, 0.25)
    if (diff(padded) >= 360) {
      dlon <- c(-180, 180)
    } else {
      dlon <- .normalise_lon(c(floor(padded[1] * 10) / 10, ceiling(padded[2] * 10) / 10))
    }
    dlat <- c(max(-90, floor((extent$lat[1] - 0.25) * 10) / 10),
              min(90, ceiling((extent$lat[2] + 0.25) * 10) / 10))
    cat("Defaults cover your data plus a small margin.\n")
  }
  cat("Longitudes run from -180 to 180 and latitudes from -90 to 90. The region runs east\n",
      "from its western edge to its eastern edge, so it can cross 180: a western edge\n",
      "of 170 and an eastern edge of -170 is the 20-degree strip across 180.\n", sep = "")
  repeat {
    lon <- c(.ask_number("Western edge (longitude): ", dlon[1], -180, 180),
             .ask_number("Eastern edge (longitude): ", dlon[2], -180, 180))
    lon <- tryCatch(.normalise_lon(lon), error = function(e) NULL)
    if (is.null(lon)) {
      cat("The western and eastern edges must differ.\n")
      next
    }
    cat(sprintf("Longitude: %s\n", .lon_label(lon)))
    # A wide region crossing 180 is more often two edges swapped by mistake,
    # unless it is the region suggested from the data
    from_data <- !is.null(dlon) && isTRUE(all.equal(lon, dlon))
    if (from_data && .lon_crosses_180(lon) && .lon_width(lon) > 180) {
      cat("That is the narrowest range of longitudes that covers all your data.\n")
    }
    if (!from_data && .lon_crosses_180(lon) && .lon_width(lon) > 180) {
      deg <- if (trimws(.deg()) == "deg") " degrees" else trimws(.deg())
      if (!.ask_yes_no(sprintf("That region is %s%s wide. Is that what you meant?",
                               format(round(.lon_width(lon), 4)), deg), FALSE)) {
        cat(sprintf("For the region between them, enter %s as the western edge and %s as the eastern edge.\n",
                    format(lon[2]), format(lon[1])))
        next
      }
    }
    break
  }
  repeat {
    lat <- c(.ask_number("Southern edge (latitude): ", dlat[1], -90, 90),
             .ask_number("Northern edge (latitude): ", dlat[2], -90, 90))
    if (lat[1] < lat[2]) break
    cat("The southern edge must be south of the northern edge.\n")
  }
  list(lon = lon, lat = lat)
}

.choose_period <- function(catalogue, dataset_ids, data_period = NULL) {
  meta <- catalogue[match(dataset_ids, catalogue$dataset_id), , drop = FALSE]
  cat("Your products cover:\n")
  cat(sprintf("  %s: %s to %s (%s)\n", meta$dataset_id, substr(meta$time_start, 1, 10),
              substr(meta$time_end, 1, 10), .step_label(meta$time_step_hours)), sep = "")
  if (!is.null(data_period)) cat("Defaults match the dates in your data.\n")
  repeat {
    start <- .ask_date("Start date (YYYY-MM-DD): ", data_period[1])
    end <- .ask_date("End date (YYYY-MM-DD): ", data_period[2])
    if (start <= end) break
    cat("The start date must be on or before the end date.\n")
  }
  starts <- as.Date(substr(meta$time_start, 1, 10))
  ends <- as.Date(substr(meta$time_end, 1, 10))
  short <- !is.na(starts) & !is.na(ends) & (starts > start | ends < end)
  if (any(short)) {
    cat("Note: these products do not cover the whole period; dates outside their ",
        "coverage will have no values:\n", sep = "")
    cat(sprintf("  %s\n", meta$dataset_id[short]), sep = "")
  }
  c(start, end)
}

.choose_chunk <- function(catalogue, dataset_ids) {
  cat("\nHow should the download be split into files?\n")
  cat(sprintf("[%d] %s\n", seq_along(.chunk_units),
              c("One file per day", "One file per month", "One file per year",
                "A single file for the whole period")), sep = "")
  meta <- catalogue[match(dataset_ids, catalogue$dataset_id), , drop = FALSE]
  coarse <- meta$dataset_id[!is.na(meta$time_step_hours) & meta$time_step_hours > 25]
  if (length(coarse) > 0) {
    cat("Products coarser than daily are never split by day; monthly data are split\n",
        "by month at the finest, and yearly data by year.\n", sep = "")
  }
  i <- .ask_numbers("Enter a number: [2] ", length(.chunk_units), default = 2L,
                    multiple = FALSE)
  .chunk_units[i]
}

.choose_depth <- function(catalogue, dataset_ids, data_depth = NULL) {
  meta <- catalogue[catalogue$dataset_id %in% dataset_ids & catalogue$has_depth, , drop = FALSE]
  meta <- meta[!duplicated(meta$dataset_id), , drop = FALSE]
  if (nrow(meta) == 0) return(NULL)
  cat("These products have a depth axis:\n")
  cat(sprintf("  %s (%g to %g m)\n", meta$dataset_id, meta$depth_min, meta$depth_max), sep = "")
  default <- if (is.null(data_depth)) c(0, 1) else c(0, ceiling(data_depth[2]))
  if (is.null(data_depth)) cat("The default, 0 to 1 m, gives the surface layer.\n")
  repeat {
    d <- c(.ask_number("Minimum depth (m): ", default[1], 0, 12000),
           .ask_number("Maximum depth (m): ", default[2], 0, 12000))
    if (d[1] <= d[2]) return(d)
    cat("The minimum must not be more than the maximum.\n")
  }
}
