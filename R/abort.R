#' Cancel all running downloads
#'
#' Stops every copernicopia download running on this computer, in any R
#' session: the R side stops waiting, and the Python process doing the
#' download is closed, freeing its memory and network connection.
#'
#' Use this when a download has made R unresponsive: open a new R session (or
#' a terminal and run `Rscript -e "copernicopia::cmems_abort()"`) and call
#' `cmems_abort()`. In the session that is downloading, pressing Esc does the
#' same for that session's download.
#'
#' Each download's Python process checks for a stop signal twice a second and
#' closes itself. If it has not closed after `wait` seconds, it is killed.
#' Files that finished downloading are kept; partly downloaded files are
#' removed. Run [cmems_download()] again to resume.
#'
#' @param wait Seconds to wait for downloads to stop themselves before
#'   killing them.
#' @return The number of downloads cancelled, invisibly.
#' @export
cmems_abort <- function(wait = 5) {
  root <- .running_root()
  dirs <- if (dir.exists(root)) list.dirs(root, recursive = FALSE) else character(0)
  active <- dirs[vapply(dirs, .is_active, logical(1))]
  unlink(setdiff(dirs, active), recursive = TRUE)
  if (length(active) == 0) {
    message("No copernicopia downloads are running.")
    return(invisible(0L))
  }
  for (d in active) file.create(file.path(d, "stop"))
  stopped <- function(d) !dir.exists(d) || file.exists(file.path(d, "stopped"))
  deadline <- Sys.time() + wait
  while (Sys.time() < deadline && !all(vapply(active, stopped, logical(1)))) Sys.sleep(0.2)

  forced <- 0L
  for (d in active[!vapply(active, stopped, logical(1))]) {
    pid <- .worker_pid(d)
    if (!is.na(pid) && .pid_is_worker(pid)) {
      .kill_pid(pid)
      forced <- forced + 1L
    }
  }
  message(sprintf(
    "Cancelled %d download%s%s. Files that finished are kept; run cmems_download() again to resume.",
    length(active), if (length(active) == 1) "" else "s",
    if (forced > 0) sprintf(" (%d had to be force-stopped)", forced) else ""
  ))
  invisible(length(active))
}

# Each running download has a control folder here. Its Python process updates
# `heartbeat` twice a second and stops when a `stop` file appears.
.running_root <- function() {
  file.path(tools::R_user_dir("copernicopia", "cache"), "running")
}

.new_control_dir <- function() {
  root <- .running_root()
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  for (d in list.dirs(root, recursive = FALSE)) {
    if (!.is_active(d)) unlink(d, recursive = TRUE)
  }
  d <- file.path(root, paste0(Sys.getpid(), "_", sub("^file", "", basename(tempfile()))))
  dir.create(d)
  writeLines(c(paste("R process:", Sys.getpid()), paste("Started:", format(Sys.time()))),
             file.path(d, "info.txt"))
  d
}

# Active: the heartbeat was updated in the last 10 minutes, or the download
# started less than 2 minutes ago and its Python process is still starting up.
.is_active <- function(d) {
  age <- function(f) as.numeric(difftime(Sys.time(), file.mtime(f), units = "secs"))
  hb <- file.path(d, "heartbeat")
  if (file.exists(hb)) return(age(hb) < 600)
  info <- file.path(d, "info.txt")
  file.exists(info) && age(info) < 120
}

.worker_pid <- function(d) {
  f <- file.path(d, "worker.pid")
  if (!file.exists(f)) return(NA_integer_)
  suppressWarnings(as.integer(readLines(f, warn = FALSE)[1]))
}

# Only ever kill a process whose command line shows it is a worker, so a
# process ID that has since been reused by another program is left alone.
.pid_is_worker <- function(pid) {
  cmd <- tryCatch(
    if (.Platform$OS.type == "windows") {
      suppressWarnings(system2("powershell", c(
        "-NoProfile", "-Command",
        shQuote(sprintf("(Get-CimInstance Win32_Process -Filter 'ProcessId=%d').CommandLine", pid))
      ), stdout = TRUE, stderr = FALSE))
    } else {
      suppressWarnings(system2("ps", c("-p", pid, "-o", "args="), stdout = TRUE, stderr = FALSE))
    },
    error = function(e) character(0)
  )
  any(grepl("copernicopia_worker", cmd, fixed = TRUE))
}

.kill_pid <- function(pid) {
  if (.Platform$OS.type == "windows") {
    tools::pskill(pid)
  } else {
    tools::pskill(pid, tools::SIGTERM)
    Sys.sleep(1)
    if (isTRUE(tools::pskill(pid, 0L))) tools::pskill(pid, tools::SIGKILL)
  }
  invisible()
}
