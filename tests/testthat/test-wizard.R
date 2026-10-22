test_that("the wizard runs steps A-F and appends values", {
  skip_if_no_python()
  cat_df <- fake_catalogue()
  out_dir <- file.path(tempdir(), "wizard-test")
  downloads <- 0
  local_mocked_bindings(
    cmems_login = function(...) invisible(TRUE),
    cmems_catalogue = function(...) cat_df,
    # Stand-in for the real download: writes synthetic files where the real
    # ones would go
    cmems_download = function(request, dry_run = FALSE, ...) {
      if (dry_run) return(invisible(NULL))
      downloads <<- downloads + 1
      for (e in request$datasets) {
        dir <- copernicopia:::.dataset_dir(request, e)
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
        monthly <- e$time_step_hours > 24 * 20
        write_synthetic(file.path(dir, paste0(e$dataset_id, "_test.nc")), e$variables,
                        if (monthly) "2025-07-01" else "2025-06-25",
                        if (monthly) "2025-09-01" else "2025-09-05",
                        monthly = monthly, depth = isTRUE(e$has_depth))
      }
      invisible(NULL)
    }
  )
  obs <- data.frame(
    lon = c(110.2, 150, 130),
    lat = c(-50.1, -45, -60),
    date = as.Date(c("2025-07-15", "2025-08-31", "2025-08-01"))
  )
  answers <- c(
    "",            # columns guessed correctly
    "2, 4, 7",     # B: sea water temperature, SSH, chlorophyll
    "y",           # add more
    "s", "ice",    # search the full catalogue
    "1",           # sea_ice_area_fraction
    "",            # no more
    "1",           # C: thetao -> only product
    "1",           # zos -> same product
    "1",           # CHL
    "south", "1",  # ice: filter the list, then pick
    "", "", "", "",  # D: default bounds from the data
    "", "",        # E: default dates from the data
    "1",           # one file per day
    "", "",        # depth 0-1 m
    out_dir,       # folder
    "",            # estimate size first
    "",            # download now
    ""             # append
  )
  old <- options(copernicopia.input = script_input(answers))
  on.exit(options(old))
  output <- capture.output(res <- suppressMessages(cmems_wizard(obs)))

  expect_equal(downloads, 1)
  expect_true(all(c("thetao", "zos", "CHL", "ice_conc") %in% names(res)))
  expect_equal(res$thetao, expected_daily(obs$date, obs$lat), tolerance = 1e-6)
  expect_equal(res$CHL, c(7, 8, 8) + round(obs$lat * 2) / 2 / 1000, tolerance = 1e-6)
  req <- attr(res, "cmems_request")
  expect_equal(names(req$datasets), c("synth_phy_P1D-m", "synth_chl_P1M", "synth_ice-south_P1D"))
  expect_equal(req$datasets[["synth_phy_P1D-m"]]$variables, c("thetao", "zos"))
  expect_equal(req$lon, c(109.9, 150.3))
  expect_equal(req$lat, c(-60.3, -44.7))
  expect_equal(req$time, as.Date(c("2025-07-15", "2025-08-31")))
  expect_equal(req$chunk, "day")
  expect_true(any(grepl("covers your dates", output)))
  expect_true(any(grepl("cmems_request\\(", output)))
})

test_that("the wizard stops cleanly when the user declines to download", {
  cat_df <- fake_catalogue()
  local_mocked_bindings(
    cmems_login = function(...) invisible(TRUE),
    cmems_catalogue = function(...) cat_df,
    cmems_download = function(...) stop("should not download")
  )
  answers <- c("7", "", "1", "100", "120", "-60", "-40", "2025-07-01", "2025-07-31",
               "", file.path(tempdir(), "x"), "n", "n")
  old <- options(copernicopia.input = script_input(answers))
  on.exit(options(old))
  capture.output(req <- cmems_wizard())
  expect_s3_class(req, "cmems_request")
  expect_equal(names(req$datasets), "synth_chl_P1M")
  expect_equal(req$chunk, "month")
})
