#' Download the files for a request
#'
#' Downloads NetCDF files for every dataset in a [cmems_request()], split into
#' files as set by its `chunk` (one per month by default). Products coarser
#' than daily are downloaded for whole months or years, so every date in the
#' period has its time step. Files that already exist are skipped, so an
#' interrupted download can simply be restarted. Each file is written under a
#' temporary name and renamed only once complete, so a half-finished file is
#' never mistaken for a finished one. Every attempt is logged to
#' `cmems_download_log.csv` in the output folder.
#'
#' Each dataset's files go into their own subfolder of `out_dir`, named after
#' the dataset ID plus a short code for the variables, region, depth and file
#' split. A `request.txt` file in each subfolder records what it contains.
#'
#' **Progress.** A line is printed for each file as it starts and finishes,
#' with the overall percentage done and an estimate of the time left. In
#' interactive sessions, the line for the file in progress also shows a
#' running timer and the amount downloaded so far, updated twice a second
#' (turn this off with `options(copernicopia.live_progress = FALSE)`).
#'
#' **Cancelling.** The downloads run in a separate Python process. Press Esc
#' to stop both R and that process at once. If R is not responding, run
#' [cmems_abort()] from another R session or a terminal. Files that finished
#' downloading are kept; run `cmems_download()` again to resume.
#'
#' @param request A [cmems_request()].
#' @param dry_run If `TRUE`, download nothing: check each dataset and variable
#'   and estimate the download size instead.
#' @param overwrite If `TRUE`, download files again even if they exist.
#' @return Invisibly, a data frame with one row per file: its path, dataset,
#'   dates, `result` (`"ok"`, `"exists"`, `"cancelled"` or the error), size in
#'   MB and download time in seconds. For a dry run, one row per dataset with
#'   size estimates.
#' @export
cmems_download <- function(request, dry_run = FALSE, overwrite = FALSE) {
  if (!inherits(request, "cmems_request")) {
    stop("`request` must be created with cmems_request() or cmems_wizard().", call. = FALSE)
  }
  out_dir <- path.expand(request$out_dir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (dry_run) {
    cm <- .cm()
    restore <- .quiet_toolbox()
    on.exit(restore(), add = TRUE)
    return(.dry_run(request, cm))
  }

  items <- .download_plan(request, overwrite)
  todo <- which(vapply(items, `[[`, logical(1), "todo"))
  n <- length(todo)
  skipped <- length(items) - n
  if (n == 0) {
    message(sprintf("All %d file(s) are already downloaded.", length(items)))
  } else {
    message(sprintf("Downloading %d file(s) for %d product(s)%s.%s", n, length(request$datasets),
                    if (skipped > 0) sprintf(" (%d already downloaded, skipped)", skipped) else "",
                    if (.live_progress()) " Press Esc to cancel." else ""))
  }

  log_path <- file.path(out_dir, "cmems_download_log.csv")
  result <- rep("exists", length(items))
  seconds <- rep(NA_real_, length(items))
  started <- Sys.time()
  if (n > 0) {
    run <- .run_worker(request, items[todo], log_path)
    result[todo] <- run$result
    seconds[todo] <- run$seconds
  }

  finals <- vapply(items, `[[`, "", "final")
  out <- data.frame(
    file = finals,
    dataset_id = vapply(items, function(it) it$e$dataset_id, ""),
    start = do.call(c, lapply(items, function(it) it$chunk$start)),
    end = do.call(c, lapply(items, function(it) it$chunk$end)),
    result = result,
    size_mb = round(file.size(finals) / 1024^2, 2),
    seconds = round(seconds, 1),
    stringsAsFactors = FALSE
  )
  # Record each product's details for citing, and refresh README_sources.txt
  for (e in request$datasets) {
    if (any(out$dataset_id == e$dataset_id & out$result %in% c("ok", "exists"))) {
      .write_source_info(request, e, .dataset_dir(request, e))
    }
  }
  readme <- .write_readme(out_dir)

  failed <- sum(startsWith(out$result, "FAILED"))
  cancelled <- sum(out$result == "cancelled")
  took <- if (n > 0) {
    sprintf(" in %s", .fmt_secs(as.numeric(difftime(Sys.time(), started, units = "secs"))))
  } else {
    ""
  }
  message(sprintf("\n%s%s: %d file(s) ready%s%s. Log: %s",
                  if (cancelled) "Cancelled" else "Done", took,
                  sum(out$result %in% c("ok", "exists")),
                  if (failed) sprintf(", %d failed", failed) else "",
                  if (failed || cancelled) " (run again to resume)" else "",
                  log_path))
  if (!is.null(readme)) {
    message("Sources for citing are in ", readme, " (or run cmems_getreadme()).")
  }
  attr(out, "cancelled") <- cancelled > 0
  invisible(out)
}

# Downloads `items` in a separate Python process (copernicopia_worker.py) and
# shows progress as it reports each file starting and finishing. Returns the
# result and download time of each item.
.run_worker <- function(request, items, log_path) {
  n <- length(items)
  tmps <- vapply(items, function(it) .tmp_name(it$fname), "")
  result <- rep("cancelled", n)
  seconds <- rep(NA_real_, n)
  control <- .new_control_dir()
  stop_file <- file.path(control, "stop")
  job <- list(
    control_dir = control,
    subset_function = getOption("copernicopia.subset_function", "copernicusmarine:subset"),
    python_path = as.list(getOption("copernicopia.python_path", character(0))),
    tasks = lapply(seq_len(n), function(k) {
      .subset_args(request, items[[k]]$e, items[[k]]$chunk, items[[k]]$dir, tmps[k],
                   dry_run = FALSE)
    })
  )
  worker <- .start_worker(control, job)

  live <- .live_progress()
  current <- NA_integer_
  status <- NULL
  t_file <- NULL
  tick <- 0L
  finished <- FALSE
  cancelled <- FALSE
  started <- Sys.time()

  # Remove whatever the file in progress left behind (our temporary name and
  # the toolbox's own "<name>.<random>" file).
  remove_partial <- function(k) {
    files <- list.files(items[[k]]$dir)
    unlink(file.path(items[[k]]$dir, files[startsWith(files, tmps[k])]))
  }
  # Runs on any exit, including Esc or an error: stop the worker and tidy up.
  on.exit({
    if (worker$is_alive()) {
      file.create(stop_file)
      worker$kill_tree()
    }
    if (!is.na(current)) {
      remove_partial(current)
      if (!is.null(status)) status$finish("cancelled") else message("cancelled")
    }
    if (!finished) {
      message("\nDownload stopped; the Python download process has been closed. ",
              "Files that finished are kept. Run cmems_download() again to resume.")
    }
    unlink(control, recursive = TRUE)
  }, add = TRUE)

  begin_file <- function(k) {
    current <<- k
    t_file <<- Sys.time()
    prefix <- sprintf("[%*d/%d] %s ... ", nchar(n), k, n, items[[k]]$fname)
    if (live) {
      status <<- .live_line(prefix)
    } else {
      message(prefix, appendLF = FALSE)
    }
  }
  end_file <- function(k, res) {
    it <- items[[k]]
    seconds[k] <<- as.numeric(difftime(Sys.time(), t_file, units = "secs"))
    if (res == "ok") {
      tmp <- file.path(it$dir, tmps[k])
      if (file.exists(tmp)) {
        if (file.exists(it$final)) unlink(it$final)
        if (!file.rename(tmp, it$final)) res <- "FAILED: could not rename the temporary file"
      } else {
        res <- "FAILED: the toolbox did not write a file"
      }
    } else {
      remove_partial(k)
    }
    result[k] <<- res
    .append_log(log_path, it$fname, it$e$dataset_id, it$chunk, res)
    end <- .ticker_line(res, file.size(it$final) / 1024^2, seconds[k], k, n, started)
    if (live) status$finish(end) else message(end)
    current <<- NA_integer_
    status <<- NULL
  }
  handle <- function(lines) {
    for (line in lines) {
      f <- strsplit(line, "\t", fixed = TRUE)[[1]]
      k <- if (length(f) > 1) as.integer(f[2]) + 1L else NA_integer_
      switch(f[1],
        start = begin_file(k),
        done = end_file(k, "ok"),
        error = end_file(k, paste("FAILED:", substr(f[3], 1, 300))),
        cancelled = cancelled <<- TRUE,
        finished = finished <<- TRUE
      )
    }
  }

  repeat {
    alive <- worker$is_alive()
    if (alive) worker$poll_io(500)
    handle(if (alive) worker$read_output_lines() else worker$read_all_output_lines())
    if (finished || cancelled || !alive) break
    if (file.exists(stop_file)) {
      cancelled <- TRUE
      break
    }
    if (live && !is.na(current)) {
      tick <- tick + 1L
      files <- list.files(items[[current]]$dir)
      mb <- sum(file.size(file.path(items[[current]]$dir,
                                    files[startsWith(files, tmps[current])])), na.rm = TRUE) / 1024^2
      secs <- as.numeric(difftime(Sys.time(), t_file, units = "secs"))
      status$update(sprintf("downloading %s %s%s", .spinner(tick), .fmt_secs(secs),
                            if (mb > 0) sprintf(", %s so far", .fmt_size(mb)) else ""))
    }
  }

  # A worker that cmems_abort() had to kill never reports "cancelled"
  if (!finished && file.exists(stop_file)) cancelled <- TRUE
  if (cancelled) {
    # Let the worker stop by itself, then read anything it reported in the
    # meantime: a file may have finished just before the stop arrived.
    # (Killing a stuck worker closes its output, so read what it sent first.)
    if (worker$is_alive()) worker$wait(2000)
    quietly <- function(f) tryCatch(f(), error = function(e) character(0))
    if (worker$is_alive()) {
      handle(quietly(worker$read_output_lines))
      worker$kill_tree()
    } else {
      handle(quietly(worker$read_all_output_lines))
    }
    cancelled <- !finished
  }
  if (cancelled) {
    if (!is.na(current)) {
      remove_partial(current)
      if (live) status$finish("cancelled") else message("cancelled")
      current <- NA_integer_
    }
    message("\nDownload cancelled with cmems_abort(); the Python download process has been closed.")
    finished <- TRUE
  } else if (!finished) {
    # The worker stopped without saying why: report its last words
    why <- utils::tail(readLines(file.path(control, "worker.log"), warn = FALSE), 1)
    why <- if (length(why)) why else sprintf("exit status %s", format(worker$get_exit_status()))
    if (!is.na(current)) end_file(current, paste("FAILED: the download process stopped:", why))
    result[result == "cancelled"] <- "FAILED: not attempted (the download process stopped)"
    warning("The Python download process stopped unexpectedly: ", why, call. = FALSE)
    finished <- TRUE
  }
  list(result = result, seconds = seconds)
}

# Starts copernicopia_worker.py in its own process. `supervise` makes processx
# kill it if this R session ends, even by crashing.
.start_worker <- function(control, job) {
  job_path <- file.path(control, "job.json")
  .helpers()$write_job(job_path, reticulate::r_to_py(job))
  processx::process$new(
    reticulate::py_exe(),
    c(system.file("python", "copernicopia_worker.py", package = "copernicopia"), job_path),
    stdout = "|", stderr = file.path(control, "worker.log"),
    cleanup = TRUE, cleanup_tree = TRUE, supervise = TRUE
  )
}

# Every file the request needs, and whether it still has to be downloaded.
.download_plan <- function(request, overwrite) {
  items <- list()
  for (e in request$datasets) {
    dir <- .dataset_dir(request, e)
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    .write_request_note(dir, request, e)
    unlink(list.files(dir, pattern = "^tmp_", full.names = TRUE))
    .note_chunk(request$chunk, e)
    chunks <- .time_chunks(request$time, e$time_step_hours, request$chunk)
    for (i in seq_len(nrow(chunks))) {
      fname <- .chunk_filename(e, chunks[i, ])
      final <- file.path(dir, fname)
      items[[length(items) + 1]] <- list(
        e = e, chunk = chunks[i, ], dir = dir, fname = fname, final = final,
        todo = overwrite || !file.exists(final)
      )
    }
  }
  items
}

# End of a ticker line: the outcome for this file, then overall progress.
.ticker_line <- function(result, size_mb, seconds, k, n, started) {
  outcome <- if (result == "ok") {
    sprintf("done (%s in %s)", .fmt_size(size_mb), .fmt_secs(seconds))
  } else {
    result
  }
  left <- ""
  if (k < n) {
    per_file <- as.numeric(difftime(Sys.time(), started, units = "secs")) / k
    left <- sprintf(", about %s left", .fmt_secs(per_file * (n - k)))
  }
  pct <- if (k < n) min(99, round(100 * k / n)) else 100
  sprintf("%s  [%d%%%s]", outcome, pct, left)
}

# Live progress (a timer and the size so far, updated in place) needs a
# console that understands carriage returns, so it is on in interactive
# sessions only. options(copernicopia.live_progress = FALSE) turns it off.
.live_progress <- function() {
  isTRUE(getOption("copernicopia.live_progress", interactive()))
}

# A console line that can be rewritten in place: update() replaces the text
# after the prefix, finish() writes the final text and ends the line.
# A line that wraps can no longer be rewritten (the carriage return only goes
# back to the start of the last row), so updates are kept within the console
# width by shortening the prefix; the final text is written in full.
.live_line <- function(prefix) {
  width <- 0
  write <- function(text) {
    cat("\r", text, strrep(" ", max(0, width - nchar(text))), sep = "")
    width <<- nchar(text)
    utils::flush.console()
  }
  write(.fit_line(prefix, ""))
  list(
    update = function(text) write(.fit_line(prefix, text)),
    finish = function(text) {
      write(paste0(prefix, text))
      cat("\n")
    }
  )
}

# prefix + text in at most one console row less one character, shortening the
# middle of the prefix (e.g. a long file name) so the changing text stays whole.
.fit_line <- function(prefix, text, width = getOption("width", 80)) {
  room <- max(20L, as.integer(width) - 1L)
  line <- paste0(prefix, text)
  if (nchar(line) <= room) return(line)
  keep <- room - nchar(text) - 3L
  if (keep < 8L) return(substr(line, 1L, room))
  head <- ceiling(keep / 2)
  paste0(substr(prefix, 1L, head), "...",
         substr(prefix, nchar(prefix) - (keep - head) + 1L, nchar(prefix)), text)
}

.spinner <- function(i) c("|", "/", "-", "\\")[i %% 4 + 1]

# A temporary file name unique to this attempt, so leftovers from a stopped
# download never collide with a new one.
.tmp_name <- function(fname) {
  paste0("tmp_", sub("^file", "", basename(tempfile())), "_", fname)
}

.fmt_secs <- function(s) {
  s <- round(s)
  if (is.na(s)) return("?")
  if (s < 60) return(sprintf("%ds", s))
  if (s < 3600) return(sprintf("%dm %02ds", s %/% 60, s %% 60))
  sprintf("%dh %02dm", s %/% 3600, (s %% 3600) %/% 60)
}

# The toolbox prints its own log lines and progress bar for every file, which
# would break up the ticker. This turns its logging down to warnings and
# returns a function that restores it.
.quiet_toolbox <- function() {
  logger <- tryCatch(reticulate::import("logging")$getLogger("copernicusmarine"),
                     error = function(e) NULL)
  if (is.null(logger)) return(function() invisible())
  old <- logger$level
  logger$setLevel(30L)
  function() try(logger$setLevel(old), silent = TRUE)
}

# Region padded by one grid cell, so cells that straddle the edge of the
# requested region are included. (The toolbox's "outside" selection would do
# this too, but it also widens the time range to the next time step, which
# would make neighbouring files overlap.)
#
# Longitudes are passed as c(west, east) with east > west, e.g. c(169.5,
# 190.5) for a region crossing 180. The toolbox then returns one continuous
# grid running from 169.5 to 190.5.
.padded_bounds <- function(request, e) {
  step <- function(x) if (is.null(x) || is.na(x) || x <= 0) 0.25 else x
  lon <- .lon_pad(request$lon, step(e$lon_step))
  lat <- request$lat + c(-1, 1) * step(e$lat_step)
  list(lon = lon, lat = pmin(pmax(lat, -90), 90))
}

.note_chunk <- function(chunk, e) {
  used <- .effective_chunk(chunk, e$time_step_hours)
  if (used != chunk) {
    message(sprintf("%s has %s data, so it is saved as %s instead.", e$dataset_id,
                    .step_label(e$time_step_hours), .chunk_label(used)))
  }
}

# Arguments for the toolbox's subset() for one file.
.subset_args <- function(request, e, chunk, dir, filename, dry_run) {
  bounds <- .padded_bounds(request, e)
  args <- list(
    dataset_id = e$dataset_id,
    variables = as.list(e$variables),
    minimum_longitude = bounds$lon[1], maximum_longitude = bounds$lon[2],
    minimum_latitude = bounds$lat[1], maximum_latitude = bounds$lat[2],
    start_datetime = format(chunk$start, "%Y-%m-%dT00:00:00"),
    end_datetime = format(chunk$end, "%Y-%m-%dT23:59:59"),
    coordinates_selection_method = "inside",
    output_directory = dir,
    output_filename = filename,
    netcdf_compression_level = 1L,
    disable_progress_bar = TRUE,
    dry_run = dry_run
  )
  if (isTRUE(e$has_depth)) {
    args$minimum_depth <- e$depth[1]
    args$maximum_depth <- e$depth[2]
  }
  args
}

.dry_run <- function(request, cm) {
  n <- length(request$datasets)
  rows <- lapply(seq_len(n), function(i) {
    e <- request$datasets[[i]]
    .note_chunk(request$chunk, e)
    message(sprintf("Checking %d/%d: %s ... ", i, n, e$dataset_id), appendLF = FALSE)
    chunks <- .time_chunks(request$time, e$time_step_hours, request$chunk)
    res <- tryCatch(
      do.call(cm$subset, .subset_args(request, e, chunks[1, ], tempdir(), "dryrun.nc",
                                      dry_run = TRUE)),
      error = function(err) err
    )
    message(if (inherits(res, "error")) "problem" else "ok")
    if (inherits(res, "error")) {
      return(data.frame(dataset_id = e$dataset_id, ok = FALSE, files = nrow(chunks),
                        mb_per_file = NA_real_, total_mb = NA_real_,
                        error = .short_error(res), stringsAsFactors = FALSE))
    }
    mb <- res$file_size
    if (inherits(mb, "python.builtin.object")) mb <- reticulate::py_to_r(mb)
    mb <- suppressWarnings(as.numeric(mb))
    if (length(mb) == 0) mb <- NA_real_
    data.frame(dataset_id = e$dataset_id, ok = TRUE, files = nrow(chunks),
               mb_per_file = round(mb, 1), total_mb = round(mb * nrow(chunks), 1),
               error = "", stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  cat("\nEstimated download size\n")
  for (i in seq_len(nrow(out))) {
    r <- out[i, ]
    if (r$ok) {
      cat(sprintf("  %s: %d file(s), ~%s each, ~%s in total\n", r$dataset_id,
                  r$files, .fmt_size(r$mb_per_file), .fmt_size(r$total_mb)))
    } else {
      cat(sprintf("  %s: PROBLEM - %s\n", r$dataset_id, r$error))
    }
  }
  cat(sprintf("  Total: ~%s (estimates can be off for compressed NetCDF)\n",
              .fmt_size(sum(out$total_mb, na.rm = TRUE))))
  invisible(out)
}

.fmt_size <- function(mb) {
  if (is.na(mb)) return("? MB")
  if (mb >= 1024) sprintf("%.1f GB", mb / 1024) else sprintf("%.3g MB", mb)
}

.append_log <- function(log_path, fname, dataset_id, chunk, result) {
  row <- data.frame(time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), file = fname,
                    dataset = dataset_id, start = format(chunk$start),
                    end = format(chunk$end), result = result)
  utils::write.table(row, log_path, sep = ",", append = file.exists(log_path),
                     col.names = !file.exists(log_path), row.names = FALSE,
                     qmethod = "double")
}

.write_request_note <- function(dir, request, e) {
  note <- file.path(dir, "request.txt")
  if (file.exists(note)) return(invisible())
  writeLines(c(
    paste("Dataset:  ", e$dataset_id),
    paste("Variables:", paste(e$variables, collapse = ", ")),
    paste("Longitude:", .lon_text(request$lon)),
    sprintf("Latitude:  %g to %g", request$lat[1], request$lat[2]),
    if (isTRUE(e$has_depth)) sprintf("Depth:     %g to %g m", e$depth[1], e$depth[2]),
    paste("Files:    ", .chunk_label(.effective_chunk(request$chunk, e$time_step_hours)))
  ), note)
}

.short_error <- function(err) {
  lines <- strsplit(conditionMessage(err), "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines) & !grepl("py_last_error|^Run `", lines)]
  substr(paste(lines, collapse = " "), 1, 300)
}
