# An in-process stand-in for the copernicusmarine module, used for dry runs
# (which run in the R session rather than the download worker).
fake_toolbox <- function() {
  calls <- list()
  subset <- function(...) {
    args <- list(...)
    calls[[length(calls) + 1]] <<- args
    list(file_size = 12.5)
  }
  list(subset = subset, calls = function() calls)
}

read_json <- function(path) {
  reticulate::import("json")$loads(paste(readLines(path, warn = FALSE), collapse = ""))
}

control_dirs <- function() {
  list.dirs(copernicopia:::.running_root(), recursive = FALSE)
}

test_that("files are downloaded by day, failures logged, and reruns resume", {
  skip_if_no_python()
  use_fake_worker_toolbox()
  rec <- withr::local_tempdir()
  withr::local_envvar(FAKE_FAIL_ON = "20250802", FAKE_RECORD = rec)
  out_dir <- withr::local_tempdir()
  req <- cmems_request(list("synth_phy_P1D-m" = "thetao"), c(100, 160), c(-70, -30),
                       c("2025-07-31", "2025-08-02"), depth = c(0, 5), chunk = "day",
                       out_dir = out_dir, catalogue = fake_catalogue())
  msgs <- capture_messages(res <- cmems_download(req))
  expect_equal(basename(res$file),
               paste0("synth_phy_P1D-m_", c("20250731", "20250801", "20250802"), ".nc"))
  expect_equal(res$result[1:2], c("ok", "ok"))
  expect_equal(res$result[3], "FAILED: ValueError: server unavailable")
  expect_false(attr(res, "cancelled"))

  ticker <- paste(msgs, collapse = "")
  expect_match(msgs[1], "Downloading 3 file(s) for 1 product(s).", fixed = TRUE)
  expect_match(ticker, "[1/3] synth_phy_P1D-m_20250731.nc ... done (0.286 MB in 0s)  [33%, about",
               fixed = TRUE)
  expect_match(ticker, "[3/3] synth_phy_P1D-m_20250802.nc ... FAILED: ValueError: server unavailable  [100%]",
               fixed = TRUE)
  expect_match(ticker, "Done in [0-9]+s: 2 file\\(s\\) ready, 1 failed \\(run again to resume\\)")

  # No temporary files left behind, and the control folder is gone
  dir <- copernicopia:::.dataset_dir(req, req$datasets[[1]])
  expect_setequal(list.files(dir), c("request.txt", "source_info.dcf",
                                     "synth_phy_P1D-m_20250731.nc", "synth_phy_P1D-m_20250801.nc"))
  expect_true(file.exists(file.path(out_dir, "README_sources.txt")))
  expect_length(control_dirs(), 0)
  log <- utils::read.csv(file.path(out_dir, "cmems_download_log.csv"))
  expect_equal(nrow(log), 3)

  # What the worker asked of the toolbox: the days, padded region and depth.
  # The first file starts one time step (a day) early and the last ends one
  # late, so every point has its nearest time step; the stand-in toolbox names
  # its records by the first day asked for
  a <- read_json(file.path(rec, "synth_phy_P1D-m_20250730.json"))
  expect_equal(a$start_datetime, "2025-07-30T00:00:00")
  expect_equal(a$end_datetime, "2025-07-31T23:59:59")
  middle <- read_json(file.path(rec, "synth_phy_P1D-m_20250801.json"))
  expect_equal(c(middle$start_datetime, middle$end_datetime),
               c("2025-08-01T00:00:00", "2025-08-01T23:59:59"))
  last <- read_json(file.path(rec, "synth_phy_P1D-m_20250802.json"))
  expect_equal(c(last$start_datetime, last$end_datetime),
               c("2025-08-02T00:00:00", "2025-08-03T23:59:59"))
  expect_equal(a$coordinates_selection_method, "inside")
  expect_equal(c(a$minimum_longitude, a$maximum_longitude), c(99.5, 160.5))
  expect_equal(c(a$minimum_latitude, a$maximum_latitude), c(-70.5, -29.5))
  expect_equal(c(a$minimum_depth, a$maximum_depth), c(0, 5))
  expect_equal(unlist(a$variables), "thetao")
  expect_true(a$disable_progress_bar)

  # Running again only retries the failed day
  withr::local_envvar(FAKE_FAIL_ON = NA)
  msgs2 <- capture_messages(res2 <- cmems_download(req))
  expect_equal(res2$result, c("exists", "exists", "ok"))
  expect_match(msgs2[1], "Downloading 1 file(s) for 1 product(s) (2 already downloaded, skipped).",
               fixed = TRUE)
  msgs3 <- capture_messages(cmems_download(req))
  expect_match(msgs3[1], "All 3 file(s) are already downloaded.", fixed = TRUE)
})

test_that("monthly data asked for by day are split by month, with a note", {
  skip_if_no_python()
  use_fake_worker_toolbox()
  rec <- withr::local_tempdir()
  withr::local_envvar(FAKE_RECORD = rec)
  req <- cmems_request(list("synth_chl_P1M" = "CHL"), c(100, 160), c(-70, -30),
                       c("2025-07-10", "2025-08-20"), chunk = "day",
                       out_dir = withr::local_tempdir(), catalogue = fake_catalogue())
  msgs <- capture_messages(res <- cmems_download(req))
  expect_match(msgs[1], "monthly data, so it is saved as one file per month")
  expect_equal(basename(res$file), c("synth_chl_P1M_202507.nc", "synth_chl_P1M_202508.nc"))
  a <- read_json(file.path(rec, "synth_chl_P1M_20250701.json"))
  expect_null(a$minimum_depth)
  expect_equal(a$start_datetime, "2025-07-01T00:00:00")
})

test_that("a dry run downloads nothing and estimates the size", {
  out_dir <- withr::local_tempdir()
  tb <- fake_toolbox()
  local_mocked_bindings(.cm = function() tb)
  req <- cmems_request(list("synth_phy_P1D-m" = "zos"), c(100, 160), c(-70, -30),
                       c("2025-07-01", "2025-09-30"), out_dir = out_dir,
                       catalogue = fake_catalogue())
  msgs <- capture_messages(out <- capture.output(est <- cmems_download(req, dry_run = TRUE)))
  expect_match(paste(msgs, collapse = ""), "Checking 1/1: synth_phy_P1D-m ... ok", fixed = TRUE)
  expect_equal(est$files, 3)
  expect_equal(est$total_mb, 37.5)
  expect_true(any(grepl("~37.5 MB in total", out)))
  expect_length(list.files(out_dir, pattern = "\\.nc$", recursive = TRUE), 0)
  expect_true(isTRUE(tb$calls()[[1]]$dry_run))
})

test_that("toolbox warnings in a dry run are shown on their own lines", {
  skip_if_no_python()
  logging <- reticulate::import("logging")
  logger <- logging$getLogger("copernicusmarine")
  n_handlers <- length(logger$handlers)
  tb <- list(subset = function(...) {
    logging$getLogger("copernicusmarine")$warning("Something about the time range")
    list(file_size = 12.5)
  })
  local_mocked_bindings(.cm = function() tb)
  req <- cmems_request(list("synth_phy_P1D-m" = "zos"), c(100, 160), c(-70, -30),
                       c("2025-07-01", "2025-07-31"), out_dir = withr::local_tempdir(),
                       catalogue = fake_catalogue())
  msgs <- capture_messages(capture.output(cmems_download(req, dry_run = TRUE)))
  all <- paste(msgs, collapse = "")
  expect_match(all, "Checking 1/1: synth_phy_P1D-m ... ok\n", fixed = TRUE)
  expect_match(all, "ok\n  Note from the Copernicus Marine toolbox: Something about the time range",
               fixed = TRUE)
  # The toolbox's own logging is put back afterwards
  expect_length(logger$handlers, n_handlers)
})

test_that("live progress shows a timer and size while each file downloads", {
  skip_if_no_python()
  use_fake_worker_toolbox()
  withr::local_envvar(FAKE_DELAY = "0.6", FAKE_FAIL_ON = "20250802")
  withr::local_options(copernicopia.live_progress = TRUE)
  req <- cmems_request(list("synth_chl_P1M" = "CHL", "synth_phy_P1D-m" = "zos"),
                       c(100, 160), c(-70, -30), c("2025-08-01", "2025-08-02"),
                       chunk = "day", out_dir = withr::local_tempdir(),
                       catalogue = fake_catalogue())
  printed <- capture.output(res <- suppressMessages(cmems_download(req)))
  screen <- paste(printed, collapse = "\n")
  expect_equal(res$result[1:2], c("ok", "ok"))
  expect_match(screen, "\r[1/3] synth_chl_P1M_202508.nc ... downloading", fixed = TRUE)
  expect_match(screen, "MB so far")
  expect_match(screen, "[1/3] synth_chl_P1M_202508.nc ... done (0.286 MB in 2s)", fixed = TRUE)
  expect_match(screen, "[3/3] synth_phy_P1D-m_20250802.nc ... FAILED: ValueError", fixed = TRUE)
  expect_length(printed, 3)
})

test_that("a stop signal (as from cmems_abort) cancels R and the worker", {
  skip_if_no_python()
  use_fake_worker_toolbox()
  withr::local_envvar(FAKE_STOP_ON = "20250801")
  req <- cmems_request(list("synth_phy_P1D-m" = "zos"), c(100, 160), c(-70, -30),
                       c("2025-07-31", "2025-08-02"), chunk = "day",
                       out_dir = withr::local_tempdir(), catalogue = fake_catalogue())
  t0 <- Sys.time()
  msgs <- capture_messages(res <- cmems_download(req))
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 10)
  expect_equal(res$result, c("ok", "cancelled", "cancelled"))
  expect_true(attr(res, "cancelled"))
  all <- paste(msgs, collapse = "")
  expect_match(all, "[2/3] synth_phy_P1D-m_20250801.nc ... cancelled", fixed = TRUE)
  expect_match(all, "Download cancelled with cmems_abort()", fixed = TRUE)
  expect_match(all, "Cancelled in [0-9]+s: 1 file\\(s\\) ready \\(run again to resume\\)")
  dir <- copernicopia:::.dataset_dir(req, req$datasets[[1]])
  expect_false(any(startsWith(list.files(dir), "tmp_")))
  expect_length(control_dirs(), 0)
})

test_that("stopping R mid-download (Esc) also stops the worker", {
  skip_if_no_python()
  skip_on_os("windows")
  use_fake_worker_toolbox()
  withr::local_envvar(FAKE_DELAY = "2")
  withr::local_options(copernicopia.live_progress = TRUE)
  pid <- NA
  # The first live update stands in for the user pressing Esc
  local_mocked_bindings(.live_line = function(prefix) {
    list(update = function(text) {
      pid <<- copernicopia:::.worker_pid(control_dirs()[1])
      stop("simulated Esc")
    }, finish = function(text) invisible())
  })
  req <- cmems_request(list("synth_phy_P1D-m" = "zos"), c(100, 160), c(-70, -30),
                       c("2025-07-31", "2025-08-02"), chunk = "day",
                       out_dir = withr::local_tempdir(), catalogue = fake_catalogue())
  msgs <- capture_messages(expect_error(cmems_download(req), "simulated Esc"))
  expect_match(paste(msgs, collapse = ""), "Download stopped; the Python download process has been closed")
  expect_false(is.na(pid))
  Sys.sleep(0.5)
  expect_false(worker_alive(pid))
  dir <- copernicopia:::.dataset_dir(req, req$datasets[[1]])
  expect_false(any(startsWith(list.files(dir), "tmp_")))
  expect_length(control_dirs(), 0)
})

test_that("cmems_abort() says so when nothing is running", {
  expect_message(n <- cmems_abort(), "No copernicopia downloads are running")
  expect_equal(n, 0L)
})

# Starts cmems_download() in a separate R session, as a user's RStudio would be
start_background_download <- function(req, env_vars) {
  root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  dev <- file.exists(file.path(root, "DESCRIPTION")) && dir.exists(file.path(root, "R"))
  callr::r_bg(function(root, dev, req, opts) {
    if (dev) pkgload::load_all(root, helpers = FALSE, quiet = TRUE) else library(copernicopia)
    options(opts)
    suppressMessages(copernicopia::cmems_download(req))
  }, args = list(root = root, dev = dev, req = req,
                 opts = list(copernicopia.subset_function = "fake_toolbox:subset",
                             copernicopia.python_path = file.path(tempdir(), "fake-toolbox"),
                             copernicopia.live_progress = FALSE,
                             copernicopia.check_login = FALSE)),
  env = c(callr::rcmd_safe_env(), Sys.getenv(), env_vars))
}

wait_for_heartbeat <- function(timeout = 60) {
  deadline <- Sys.time() + timeout
  while (Sys.time() < deadline) {
    if (any(file.exists(file.path(control_dirs(), "heartbeat")))) return(TRUE)
    Sys.sleep(0.25)
  }
  FALSE
}

test_that("cmems_abort() in another session cancels a running download", {
  skip_if_no_python()
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload")
  skip_on_os("windows")
  use_fake_worker_toolbox()
  req <- cmems_request(list("synth_phy_P1D-m" = "zos"), c(100, 160), c(-70, -30),
                       c("2025-07-01", "2025-07-10"), chunk = "day",
                       out_dir = withr::local_tempdir(), catalogue = fake_catalogue())
  bg <- start_background_download(req, c(FAKE_DELAY = "1"))
  on.exit(bg$kill(), add = TRUE)
  expect_true(wait_for_heartbeat())
  pid <- copernicopia:::.worker_pid(control_dirs()[1])
  expect_message(n <- cmems_abort(), "Cancelled 1 download\\. Files that finished are kept")
  expect_equal(n, 1L)
  bg$wait(30000)
  res <- bg$get_result()
  expect_true(attr(res, "cancelled"))
  expect_true(all(res$result %in% c("ok", "cancelled")))
  expect_true(any(res$result == "cancelled"))
  expect_false(worker_alive(pid))
  expect_length(control_dirs(), 0)
})

test_that("a stuck worker is stopped by its own session when cmems_abort() asks", {
  skip_if_no_python()
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload")
  skip_on_os("windows")
  use_fake_worker_toolbox()
  req <- cmems_request(list("synth_phy_P1D-m" = "zos"), c(100, 160), c(-70, -30),
                       c("2025-07-01", "2025-07-02"), chunk = "day",
                       out_dir = withr::local_tempdir(), catalogue = fake_catalogue())
  # The first file starts a day early, on 30 June (see .chunk_times())
  bg <- start_background_download(req, c(FAKE_HANG_ON = "20250630"))
  on.exit(bg$kill(), add = TRUE)
  expect_true(wait_for_heartbeat())
  Sys.sleep(2)
  pid <- copernicopia:::.worker_pid(control_dirs()[1])
  expect_true(worker_alive(pid))
  expect_message(cmems_abort(wait = 5), "Cancelled 1 download")
  Sys.sleep(1)
  expect_false(worker_alive(pid))
  bg$wait(30000)
  res <- bg$get_result()
  expect_true(attr(res, "cancelled"))
  expect_equal(res$result, c("cancelled", "cancelled"))
})

test_that("cmems_abort() force-stops a stuck worker when R is not responding", {
  skip_if_no_python()
  skip_on_os("windows")
  use_fake_worker_toolbox()
  # A worker with no R session watching it, as when R itself has frozen
  out_dir <- withr::local_tempdir()
  control <- copernicopia:::.new_control_dir()
  job <- list(
    control_dir = control,
    subset_function = "fake_toolbox:subset",
    python_path = list(file.path(tempdir(), "fake-toolbox")),
    tasks = list(list(dataset_id = "ds", start_datetime = "2025-07-01T00:00:00",
                      output_directory = out_dir, output_filename = "tmp_x.nc"))
  )
  withr::local_envvar(FAKE_HANG_ON = "20250701")
  worker <- copernicopia:::.start_worker(control, job)
  on.exit(worker$kill(), add = TRUE)
  expect_true(wait_for_heartbeat())
  Sys.sleep(2)
  expect_true(worker$is_alive())
  expect_message(n <- cmems_abort(wait = 2), "1 had to be force-stopped")
  expect_equal(n, 1L)
  Sys.sleep(1)
  expect_false(worker$is_alive())
  # The leftover control folder is tidied away once it goes quiet
  unlink(control, recursive = TRUE)
})

test_that("the toolbox's false alarm about regions crossing 180 on global grids is dropped", {
  w <- c("Some of your subset selection [76.45, -71.45] for the longitude dimension exceed the dataset coordinates [-179.975, 179.975]",
         "Some of your subset selection [2026-01-01, 2026-12-31] for the time dimension exceed the dataset coordinates [2024-01-17, 2026-10-24]")
  global <- list(lon_min = -179.975, lon_max = 179.975, lon_step = 0.05)
  regional <- list(lon_min = -6, lon_max = 36.5, lon_step = 0.05)
  expect_equal(copernicopia:::.useful_toolbox_warnings(w, global), w[2])
  expect_equal(copernicopia:::.useful_toolbox_warnings(w, regional), w)
  expect_equal(copernicopia:::.useful_toolbox_warnings(w, list()), w)
})

test_that("daily and finer data get one extra time step at each end of the period", {
  cat_df <- fake_catalogue()
  cat_df$time_step_hours[cat_df$dataset_id == "synth_old_P1D"] <- 1
  cat_df$time_start[cat_df$dataset_id == "synth_old_P1D"] <- "2025-01-01T00:00:00"
  cat_df$time_end[cat_df$dataset_id == "synth_old_P1D"] <- "2025-12-31T23:00:00"
  req <- cmems_request(list("synth_old_P1D" = "analysed_sst"), c(100, 160), c(-70, -30),
                       c("2025-07-10", "2025-08-20"), out_dir = tempdir(), catalogue = cat_df)
  e <- req$datasets[[1]]
  chunks <- copernicopia:::.time_chunks(req$time, e$time_step_hours, req$chunk)
  times <- lapply(seq_len(nrow(chunks)), function(i) copernicopia:::.chunk_times(req, e, chunks[i, ]))
  # Hourly, by month: July starts an hour early, August ends an hour late; they meet without overlap
  expect_equal(times[[1]], c("2025-07-09T23:00:00", "2025-07-31T23:59:59"))
  expect_equal(times[[2]], c("2025-08-01T00:00:00", "2025-08-21T00:59:59"))
  # Daily, by month: July starts a day early and August ends a day late, so a
  # point late on the last day has the next day's time step to be nearest to
  req_d <- cmems_request(list("synth_phy_P1D-m" = "zos"), c(100, 160), c(-70, -30),
                         c("2025-07-10", "2025-08-20"), out_dir = tempdir(), catalogue = fake_catalogue())
  e_d <- req_d$datasets[[1]]
  ch_d <- copernicopia:::.time_chunks(req_d$time, e_d$time_step_hours, req_d$chunk)
  expect_equal(copernicopia:::.chunk_times(req_d, e_d, ch_d[1, ]), c("2025-07-09T00:00:00", "2025-07-31T23:59:59"))
  expect_equal(copernicopia:::.chunk_times(req_d, e_d, ch_d[2, ]), c("2025-08-01T00:00:00", "2025-08-21T23:59:59"))
  # A single file for the whole period gets both
  req_a <- cmems_request(list("synth_phy_P1D-m" = "zos"), c(100, 160), c(-70, -30),
                         c("2025-07-10", "2025-08-20"), chunk = "all", out_dir = tempdir(),
                         catalogue = fake_catalogue())
  ch_a <- copernicopia:::.time_chunks(req_a$time, e_d$time_step_hours, req_a$chunk)
  expect_equal(copernicopia:::.chunk_times(req_a, e_d, ch_a[1, ]), c("2025-07-09T00:00:00", "2025-08-21T23:59:59"))
  # Monthly data are downloaded as whole months instead, with nothing added
  req_m <- cmems_request(list("synth_chl_P1M" = "CHL"), c(100, 160), c(-70, -30),
                         c("2025-07-10", "2025-08-20"), out_dir = tempdir(), catalogue = fake_catalogue())
  e_m <- req_m$datasets[[1]]
  ch_m <- copernicopia:::.time_chunks(req_m$time, e_m$time_step_hours, req_m$chunk)
  expect_equal(copernicopia:::.chunk_times(req_m, e_m, ch_m[1, ]), c("2025-07-01T00:00:00", "2025-07-31T23:59:59"))
  expect_equal(copernicopia:::.chunk_times(req_m, e_m, ch_m[2, ]), c("2025-08-01T00:00:00", "2025-08-31T23:59:59"))
})

test_that("with nearest matching, a point late on the last day gets the next day's time step", {
  skip_if_no_python()
  # Files as cmems_download() now asks for them: 9 July to 21 July for a
  # request for 10 to 20 July, with one time step a day at 00:00
  req <- cmems_request(list("synth_phy_P1D-m" = "zos"), c(100, 160), c(-70, -30),
                       c("2025-07-10", "2025-07-20"), out_dir = withr::local_tempdir(),
                       catalogue = fake_catalogue())
  e <- req$datasets[[1]]
  chunk <- copernicopia:::.time_chunks(req$time, e$time_step_hours, req$chunk)
  asked <- copernicopia:::.chunk_times(req, e, chunk[1, ])
  expect_equal(asked, c("2025-07-09T00:00:00", "2025-07-21T23:59:59"))
  dir <- copernicopia:::.dataset_dir(req, e)
  dir.create(dir, recursive = TRUE)
  reticulate::py_run_string("
import numpy as np, pandas as pd, xarray as xr
def _midnight_file(path, start, end):
    lat = np.arange(-70, -29.9, 0.5); lon = np.arange(100, 160.01, 0.5)
    t = pd.date_range(start[:10], end[:10], freq='D')
    v = t.day.values.astype(float)[:, None, None] + 0 * lat[None, :, None] + 0 * lon[None, None, :]
    xr.Dataset({'zos': (('time', 'latitude', 'longitude'), v)},
               coords=dict(time=t, latitude=lat, longitude=lon)).to_netcdf(path, engine='h5netcdf')
")
  reticulate::py$`_midnight_file`(file.path(dir, "synth_phy_P1D-m_20250710-20250720.nc"), asked[1], asked[2])
  obs <- data.frame(lon = 130, lat = -50,
                    time = as.POSIXct(c("2025-07-10 01:00", "2025-07-15 13:00", "2025-07-20 11:00",
                                        "2025-07-20 13:00", "2025-07-20 23:00"), tz = "UTC"))
  # Value = day of the month of the time step used
  near <- suppressMessages(cmems_extract(obs, req, time_match = "nearest"))
  expect_equal(near$zos, c(10, 16, 20, 21, 21))
  # Matching on the calendar day is unchanged by the extra time steps
  day <- suppressMessages(cmems_extract(obs, req))
  expect_equal(day$zos, c(10, 15, 20, 20, 20))
})

test_that("downloads stop with a clear message when not logged in", {
  skip_if_no_python()
  use_fake_worker_toolbox()
  withr::local_options(copernicopia.check_login = TRUE, copernicopia.input = NULL)
  state <- copernicopia:::.cmems
  old <- state$logged_in
  withr::defer(assign("logged_in", old, envir = state))
  state$logged_in <- NULL
  checks <- 0
  answer <- FALSE
  toolbox <- list(
    login = function(...) {
      checks <<- checks + 1
      if (is.na(answer)) stop("could not connect to the authentication system")
      answer
    },
    subset = function(...) stop("should not be reached")
  )
  local_mocked_bindings(.cm = function() toolbox)
  out_dir <- withr::local_tempdir()
  req <- cmems_request(list("synth_phy_P1D-m" = "zos"), c(100, 160), c(-70, -30),
                       c("2025-07-01", "2025-07-02"), chunk = "day", out_dir = out_dir,
                       catalogue = fake_catalogue())
  # No saved credentials: nothing is started, for a download or a dry run
  expect_error(cmems_download(req), "not logged in to Copernicus Marine.*cmems_login\\(\\)")
  expect_error(cmems_download(req, dry_run = TRUE), "not logged in to Copernicus Marine")
  expect_length(list.files(out_dir, pattern = "\\.nc$", recursive = TRUE), 0)
  expect_false(file.exists(file.path(out_dir, "cmems_download_log.csv")))
  expect_length(control_dirs(), 0)
  expect_equal(checks, 2)

  # If the check cannot be made, the download goes ahead and reports what happens
  answer <- NA
  expect_false(copernicopia:::.require_login())
  expect_null(state$logged_in)

  # Valid credentials are checked once a session
  answer <- TRUE
  before <- checks
  expect_true(copernicopia:::.require_login())
  expect_true(copernicopia:::.require_login())
  expect_equal(checks, before + 1)
  res <- suppressMessages(cmems_download(req))
  expect_equal(res$result, c("ok", "ok"))
  expect_equal(checks, before + 1)

  # Files that are already there need no login to be found again
  state$logged_in <- NULL
  answer <- FALSE
  msgs <- capture_messages(cmems_download(req))
  expect_match(msgs[1], "All 2 file(s) are already downloaded", fixed = TRUE)
})

test_that("where questions can be asked, a download asks you to log in", {
  withr::local_options(copernicopia.check_login = TRUE,
                       copernicopia.input = script_input(c("someone", "secret")))
  state <- copernicopia:::.cmems
  old <- state$logged_in
  withr::defer(assign("logged_in", old, envir = state))
  state$logged_in <- NULL
  given <- NULL
  toolbox <- list(login = function(username = NULL, password = NULL, ...) {
    if (is.null(username)) return(FALSE)   # no saved credentials
    given <<- c(username, password)
    TRUE
  })
  local_mocked_bindings(.cm = function() toolbox, .quiet_toolbox = function() function() invisible())
  msgs <- capture_messages(out <- capture.output(ok <- copernicopia:::.require_login()))
  expect_true(ok)
  expect_equal(given, c("someone", "secret"))
  expect_true(state$logged_in)
  expect_match(paste(msgs, collapse = ""), "No valid Copernicus Marine credentials found.*Logged in\\.")
  expect_true(any(grepl("Copernicus Marine username: someone", out, fixed = TRUE)))
})

test_that("a file the toolbox silently did not write is a failure that mentions logging in", {
  skip_if_no_python()
  use_fake_worker_toolbox()
  # As the real toolbox behaves in the download process without saved
  # credentials: it prints "Abort" and returns, with no error
  withr::local_envvar(FAKE_SILENT_ON = "20250702")
  req <- cmems_request(list("synth_phy_P1D-m" = "zos"), c(100, 160), c(-70, -30),
                       c("2025-07-01", "2025-07-02"), chunk = "day",
                       out_dir = withr::local_tempdir(), catalogue = fake_catalogue())
  msgs <- capture_messages(res <- cmems_download(req))
  expect_equal(res$result[1], "ok")
  expect_match(res$result[2], "^FAILED: the toolbox did not write a file\\. If you are not logged in, run cmems_login\\(\\)")
  expect_match(paste(msgs, collapse = ""), "1 file\\(s\\) ready, 1 failed")
})

test_that("a dry run that gets nothing back from the toolbox reports a problem, not ok", {
  tb <- list(subset = function(...) NULL)
  local_mocked_bindings(.cm = function() tb)
  req <- cmems_request(list("synth_phy_P1D-m" = "zos"), c(100, 160), c(-70, -30),
                       c("2025-07-01", "2025-09-30"), out_dir = withr::local_tempdir(),
                       catalogue = fake_catalogue())
  msgs <- capture_messages(out <- capture.output(est <- cmems_download(req, dry_run = TRUE)))
  expect_match(paste(msgs, collapse = ""), "Checking 1/1: synth_phy_P1D-m ... problem", fixed = TRUE)
  expect_false(est$ok)
  expect_match(est$error, "not logged in; run cmems_login()", fixed = TRUE)
  expect_true(any(grepl("PROBLEM - the toolbox returned nothing", out)))
})

test_that("temporary names are short, and Windows path limits are warned about", {
  a <- copernicopia:::.tmp_name()
  expect_match(a, "^tmp_[0-9a-f]+\\.nc$")
  expect_false(identical(a, copernicopia:::.tmp_name()))
  expect_lt(nchar(a), 25)
  dir <- withr::local_tempdir()
  long <- file.path(dir, paste0(strrep("x", 300), ".nc"))
  short <- file.path(dir, "a.nc")
  warn <- copernicopia:::.warn_long_paths
  expect_warning(warn(long, windows = TRUE, long_ok = FALSE), "more than Windows allows")
  expect_silent(warn(short, windows = TRUE, long_ok = FALSE))
  expect_silent(warn(long, windows = TRUE, long_ok = TRUE))
  expect_silent(warn(long, windows = FALSE, long_ok = FALSE))
  expect_silent(warn(character(0), windows = TRUE, long_ok = FALSE))
})

test_that("Windows' long-paths setting is only looked up when a path is too long", {
  looked <- FALSE
  long_ok <- function() { looked <<- TRUE; FALSE }
  dir <- withr::local_tempdir()
  expect_silent(copernicopia:::.warn_long_paths(file.path(dir, "a.nc"), windows = TRUE, long_ok = long_ok()))
  expect_false(looked)
})
