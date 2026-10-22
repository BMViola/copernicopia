#' Sources and citations for your downloads
#'
#' Prints a README of every Copernicus Marine product you have downloaded,
#' ready for citing them: an acknowledgement sentence, a reference for each
#' product with its DOI and the date you downloaded it, and the details of each
#' download (dataset ID and version, variables and units, time step, grid
#' resolution, region, depth, period and files).
#'
#' The references follow Copernicus Marine's guidance
#' (<https://help.marine.copernicus.eu/en/articles/4444611-how-to-cite-or-reference-copernicus-marine-products-and-services>):
#' products are cited by their DOI, since datasets have no DOIs of their own.
#'
#' Every download keeps this up to date automatically in
#' `README_sources.txt` in the download folder, so you can also just open that
#' file.
#'
#' @param x What to list: a download folder (the default, `"cmems_data"`,
#'   lists everything in it), a [cmems_request()], or a data frame returned by
#'   [cmems_wizard()] or [cmems_extract()] (both list only the products in that
#'   request).
#' @param file Optional path to save the text to, for example
#'   `"data_sources.txt"`.
#' @return The text, invisibly, as a character vector with one element per
#'   line.
#' @export
#' @examples
#' \dontrun{
#' cmems_getreadme()                         # everything in "cmems_data"
#' cmems_getreadme(obs)                      # just the products behind obs
#' cmems_getreadme(file = "data_sources.txt")
#' }
cmems_getreadme <- function(x = "cmems_data", file = NULL) {
  found <- .collect_sources(x)
  if (length(found$infos) == 0) {
    message("No downloaded products found in ", found$folder,
            ". Sources are recorded when files are downloaded with cmems_download().")
    return(invisible(character(0)))
  }
  text <- .format_readme(found$infos, found$folder)
  cat(text, sep = "\n")
  if (!is.null(file)) {
    writeLines(text, file, useBytes = TRUE)
    message("Saved to ", normalizePath(file))
  }
  invisible(text)
}

.collect_sources <- function(x) {
  if (is.data.frame(x)) {
    req <- attr(x, "cmems_request")
    if (is.null(req)) {
      stop("This data frame has no download request attached. Pass the download folder ",
           "instead, e.g. cmems_getreadme(\"cmems_data\").", call. = FALSE)
    }
    x <- req
  }
  if (inherits(x, "cmems_request")) {
    folder <- path.expand(x$out_dir)
    dirs <- vapply(x$datasets, function(e) .dataset_dir(x, e), "")
  } else if (is.character(x) && length(x) == 1) {
    folder <- path.expand(x)
    dirs <- if (dir.exists(folder)) list.dirs(folder, recursive = FALSE) else character(0)
  } else {
    stop("`x` must be a download folder, a cmems_request or a data frame from cmems_wizard().",
         call. = FALSE)
  }
  files <- file.path(dirs, "source_info.dcf")
  files <- files[file.exists(files)]
  infos <- lapply(files, function(f) {
    info <- as.list(read.dcf(f, keep.white = .source_fields)[1, ])
    info$folder <- dirname(f)
    info
  })
  if (length(infos) > 1) {
    titles <- vapply(infos, .title, "")
    ids <- vapply(infos, function(i) .chr(i$Dataset_ID), "")
    infos <- infos[order(tolower(titles), ids)]
  }
  list(folder = folder, infos = infos)
}

.source_fields <- c("Product_title", "Product_ID", "DOI", "Dataset_ID", "Dataset_version",
                    "Producer", "Processing_level", "Variables", "Time_step_hours",
                    "Lon_step", "Lat_step", "Longitude", "Latitude", "Depth",
                    "Requested_from", "Requested_to", "Covers_from", "Covers_to", "Files",
                    "Split", "First_downloaded", "Last_downloaded", "Toolbox_version",
                    "Package_version")

# Records what is in one dataset folder, so a README can be written later.
# Called after each download; keeps the earliest download date seen.
.write_source_info <- function(request, e, dir) {
  f <- file.path(dir, "source_info.dcf")
  old <- if (file.exists(f)) as.list(read.dcf(f)[1, ]) else list()
  files <- list.files(dir, pattern = "\\.nc$")
  files <- files[!startsWith(files, "tmp_")]
  if (length(files) == 0) return(invisible())
  cover <- .files_coverage(files, e$dataset_id)
  # Local dates (as.Date() on a time would give the UTC date)
  written <- as.Date(format(file.mtime(file.path(dir, files)), "%Y-%m-%d"))
  old_dates <- as.Date(c(old$First_downloaded, old$Last_downloaded))
  dates <- range(c(written, old_dates), na.rm = TRUE)
  vars <- paste(sprintf("%s|%s|%s", e$variables,
                        .chr(e$standard_names[e$variables]), .chr(e$units[e$variables])),
                collapse = " ;; ")
  rec <- c(
    Product_title = .chr(e$product_title),
    Product_ID = .chr(e$product_id),
    DOI = .clean_doi(e$product_doi),
    Dataset_ID = e$dataset_id,
    Dataset_version = .chr(e$version),
    Producer = .chr(e$production_center),
    Processing_level = .chr(e$processing_level),
    Variables = vars,
    Time_step_hours = .chr(e$time_step_hours),
    Lon_step = .chr(e$lon_step),
    Lat_step = .chr(e$lat_step),
    Longitude = .lon_text(request$lon),
    Latitude = paste(request$lat, collapse = " to "),
    Depth = if (isTRUE(e$has_depth)) paste(e$depth, collapse = " to ") else "",
    Requested_from = format(request$time[1]),
    Requested_to = format(request$time[2]),
    Covers_from = format(cover[1]),
    Covers_to = format(cover[2]),
    Files = length(files),
    Split = .chunk_label(.effective_chunk(request$chunk, e$time_step_hours)),
    First_downloaded = format(dates[1]),
    Last_downloaded = format(dates[2]),
    Toolbox_version = .chr(if (!is.null(.cmems$toolbox_version)) .cmems$toolbox_version
                           else old$Toolbox_version),
    Package_version = as.character(utils::packageVersion("copernicopia"))
  )
  write.dcf(as.data.frame(as.list(rec), stringsAsFactors = FALSE), f, width = 10000)
  invisible(f)
}

# Rewrites README_sources.txt in a download folder.
.write_readme <- function(folder) {
  found <- .collect_sources(folder)
  if (length(found$infos) == 0) return(invisible(NULL))
  path <- file.path(path.expand(folder), "README_sources.txt")
  writeLines(.format_readme(found$infos, found$folder), path, useBytes = TRUE)
  invisible(path)
}

.format_readme <- function(infos, folder) {
  doi <- vapply(infos, function(i) .chr(i$DOI), "")
  key <- ifelse(nzchar(doi), doi, vapply(infos, function(i) .chr(i$Product_ID), ""))
  products <- unique(key)
  links <- unique(sprintf("https://doi.org/%s", doi[nzchar(doi)]))
  rule <- function(title, ch = "-") c(title, strrep(ch, nchar(title)))

  out <- c(
    rule("Copernicus Marine data sources", "="),
    "",
    strwrap(sprintf("Products downloaded into %s, listed by copernicopia %s on %s.",
                    folder, utils::packageVersion("copernicopia"), .fmt_date(Sys.Date())),
            width = 78),
    "",
    rule("Acknowledgement"),
    strwrap(paste0("This study has been conducted using E.U. Copernicus Marine Service ",
                   "Information; ",
                   if (length(links)) paste(links, collapse = "; ") else "[product DOIs]"),
            width = 78),
    "",
    rule("References")
  )
  for (p in products) {
    group <- infos[key == p]
    first <- group[[1]]
    accessed <- .fmt_date(max(as.Date(vapply(group, function(i) .chr(i$Last_downloaded), ""))))
    ref <- if (nzchar(.chr(first$DOI))) {
      sprintf("%s. E.U. Copernicus Marine Service Information (CMEMS). Marine Data Store (MDS). DOI: %s (Accessed on %s)",
              .title(first), first$DOI, accessed)
    } else {
      sprintf(paste0("%s. E.U. Copernicus Marine Service Information (CMEMS). Marine Data ",
                     "Store (MDS). Product ID: %s (Accessed on %s). [No DOI was listed for this ",
                     "product in the catalogue; check its page in the Copernicus Marine Data Store.]"),
              .title(first), .chr(first$Product_ID), accessed)
    }
    out <- c(out, strwrap(ref, width = 78, exdent = 2), "")
  }

  out <- c(out, rule("Details of each download"))
  for (k in seq_along(infos)) {
    i <- infos[[k]]
    field <- function(label, value) {
      value <- value[nzchar(value)]
      if (length(value) == 0) return(character(0))
      c(sprintf("   %-16s %s", paste0(label, ":"), value[1]),
        if (length(value) > 1) sprintf("   %-16s %s", "", value[-1]))
    }
    vars <- strsplit(strsplit(.chr(i$Variables), " ;; ", fixed = TRUE)[[1]], "|", fixed = TRUE)
    var_lines <- vapply(vars, function(v) {
      v <- c(v, "", "")[1:3]
      paste0(v[1], if (nzchar(v[2])) paste0(": ", v[2]) else "",
             if (nzchar(v[3])) paste0(" (", v[3], ")") else "")
    }, "")
    lon_step <- suppressWarnings(as.numeric(i$Lon_step))
    lat_step <- suppressWarnings(as.numeric(i$Lat_step))
    resolution <- if (!is.na(lon_step) && !is.na(lat_step)) {
      sprintf("%s x %s degrees (longitude x latitude; about %s km north-south)",
              signif(lon_step, 4), signif(lat_step, 4), signif(lat_step * 111.32, 2))
    } else {
      ""
    }
    downloaded <- if (identical(i$First_downloaded, i$Last_downloaded)) {
      .fmt_date(as.Date(i$Last_downloaded))
    } else {
      sprintf("%s to %s", .fmt_date(as.Date(i$First_downloaded)),
              .fmt_date(as.Date(i$Last_downloaded)))
    }
    out <- c(out,
      sprintf("%d. %s", k, .title(i)),
      field("Product ID", .chr(i$Product_ID)),
      field("DOI", if (nzchar(.chr(i$DOI))) sprintf("%s (https://doi.org/%s)", i$DOI, i$DOI) else ""),
      field("Dataset ID", paste0(i$Dataset_ID, if (nzchar(.chr(i$Dataset_version)))
        sprintf(" (version %s)", i$Dataset_version) else "")),
      field("Producer", paste(c(.chr(i$Producer), if (nzchar(.chr(i$Processing_level)))
        paste("processing level", i$Processing_level)), collapse = ", ")),
      field("Variables", var_lines),
      field("Time step", .step_label(suppressWarnings(as.numeric(i$Time_step_hours)))),
      field("Resolution", resolution),
      field("Region", sprintf("longitude %s, latitude %s", i$Longitude, i$Latitude)),
      field("Depth", if (nzchar(.chr(i$Depth))) paste(i$Depth, "m") else ""),
      field("Period", sprintf("%s to %s requested; files cover %s to %s", i$Requested_from,
                              i$Requested_to, i$Covers_from, i$Covers_to)),
      field("Files", sprintf("%s (%s) in %s", i$Files, i$Split, i$folder)),
      field("Downloaded", downloaded),
      field("Software", paste0(if (nzchar(.chr(i$Toolbox_version)))
        sprintf("copernicusmarine toolbox %s, ", i$Toolbox_version) else "",
        sprintf("copernicopia %s", i$Package_version))),
      ""
    )
  }

  c(out,
    rule("Notes"),
    strwrap(paste(
      "Cite the DOI of every product you used. Copernicus Marine datasets do not have",
      "DOIs of their own, so give the dataset ID if you need to say exactly which",
      "dataset you used. If a product's documentation lists further references, cite",
      "those as well. Where these examples and the Copernicus Marine licence differ,",
      "the licence applies. Guidance:",
      "https://help.marine.copernicus.eu/en/articles/4444611-how-to-cite-or-reference-copernicus-marine-products-and-services"
    ), width = 78))
}

.title <- function(i) {
  t <- .chr(i$Product_title)
  if (nzchar(t)) t else .chr(i$Dataset_ID)
}

.chr <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

.clean_doi <- function(doi) {
  doi <- trimws(.chr(doi))
  sub("^(https?://(dx\\.)?doi\\.org/|doi:\\s*)", "", doi, ignore.case = TRUE)
}

# "22 Oct 2026", with English month names whatever the locale
.fmt_date <- function(d) {
  d <- as.Date(d)
  if (is.na(d)) return("unknown date")
  sprintf("%d %s %s", as.integer(format(d, "%d")), month.abb[as.integer(format(d, "%m"))],
          format(d, "%Y"))
}

# First and last date covered by a dataset's files, read from their names
# (dataset_20250715.nc, dataset_202507.nc, dataset_2025.nc or
# dataset_20250710-20250731.nc).
.files_coverage <- function(files, dataset_id) {
  stamps <- substring(sub("\\.nc$", "", files), nchar(dataset_id) + 2)
  ranges <- lapply(stamps, function(s) {
    if (grepl("^[0-9]{8}-[0-9]{8}$", s)) {
      as.Date(strsplit(s, "-")[[1]], "%Y%m%d")
    } else if (grepl("^[0-9]{8}$", s)) {
      rep(as.Date(s, "%Y%m%d"), 2)
    } else if (grepl("^[0-9]{6}$", s)) {
      start <- as.Date(paste0(s, "01"), "%Y%m%d")
      c(start, .month_end(start))
    } else if (grepl("^[0-9]{4}$", s)) {
      as.Date(paste0(s, c("-01-01", "-12-31")))
    } else {
      as.Date(c(NA, NA))
    }
  })
  all <- do.call(c, ranges)
  if (all(is.na(all))) return(as.Date(c(NA, NA)))
  range(all, na.rm = TRUE)
}
