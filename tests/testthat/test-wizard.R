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

test_that("step C can skip a variable, and Enter picks the only product left", {
  cat_df <- fake_catalogue()
  sel_sst <- list(label = "Sea surface temperature",
                  rows = which(cat_df$variable == "analysed_sst"))
  sel_ice <- list(label = "Sea ice concentration",
                  rows = which(cat_df$variable == "ice_conc"))
  pick <- function(selection, answers) {
    withr::local_options(copernicopia.input = script_input(answers))
    out <- capture.output(res <- copernicopia:::.choose_dataset(cat_df, selection))
    list(res = res, out = out)
  }
  # Filtered to one product, Enter chooses it
  r <- pick(sel_ice, c("south", ""))
  expect_equal(r$res$dataset_id, "synth_ice-south_P1D")
  expect_true(any(grepl("Press Enter to choose it", r$out)))
  # 0 skips the variable, even after a filter that matched nothing
  r <- pick(sel_ice, c("METOFFICE", "0"))
  expect_null(r$res)
  expect_true(any(grepl("Nothing matches that text", r$out)))
  expect_true(any(grepl("Skipped Sea ice concentration.", r$out, fixed = TRUE)))
  # With several products listed, Enter still shows the full list again
  r <- pick(sel_ice, c("", "2"))
  expect_equal(r$res$dataset_id, "synth_ice-south_P1D")
})

test_that("the wizard stops if every variable is skipped", {
  cat_df <- fake_catalogue()
  local_mocked_bindings(
    cmems_login = function(...) invisible(TRUE),
    cmems_catalogue = function(...) cat_df,
    cmems_download = function(...) stop("should not download")
  )
  withr::local_options(copernicopia.input = script_input(c("7", "", "0")))
  out <- capture.output(res <- cmems_wizard())
  expect_null(res)
  expect_true(any(grepl("No products were chosen", out)))
})

test_that("the region suggested from data crossing 180 is not questioned", {
  extent <- list(lon = c(76.76, -71.84659), lat = c(-69.2232, -33.2929))
  ask <- function(answers) {
    withr::local_options(copernicopia.input = script_input(answers))
    out <- capture.output(res <- copernicopia:::.choose_bounds(extent))
    list(res = res, out = out)
  }
  # Accepting the suggested edges: no "Is that what you meant?"
  r <- ask(c("", "", "", ""))
  expect_equal(r$res$lon, c(76.5, -71.5))
  expect_equal(r$res$lat, c(-69.5, -33))
  expect_false(any(grepl("Is that what you meant", r$out)))
  expect_true(any(grepl("narrowest range of longitudes that covers all your data", r$out)))
  # Typing a wide crossing region by hand is still checked
  r <- ask(c("100", "-100", "y", "", ""))
  expect_equal(r$res$lon, c(100, -100))
})

test_that("the shortlist's product counts line up", {
  cat_df <- fake_catalogue()
  withr::local_options(copernicopia.input = script_input(c("1", "")))
  out <- capture.output(copernicopia:::.choose_variables(cat_df))
  rows <- grep("^\\[[ 0-9]{2}\\] ", out, value = TRUE)
  starts <- regexpr("\\((none available|[0-9]+ products?)\\)$", rows)
  expect_true(all(starts > 0))
  expect_length(unique(starts), 1)
})

test_that("products are checked against the area of the data, across 180 too", {
  # Points from 76.76E eastwards across 180 to 71.85W, in the Southern Ocean
  area <- list(lon = c(76.76, -71.85), lat = c(-69.2, -33.3))
  d <- data.frame(
    name = c("global", "arctic", "med", "antarctic", "south atlantic gap",
             "pacific 0-360", "edge", "unknown"),
    lon_min = c(-179.975, -180, -6, -180, -60, 150, -75, NA),
    lon_max = c(179.975, 180, 36.5, 180, 20, 250, -65, NA),
    lat_min = c(-89.975, 50, 30, -90, -70, -60, -60, NA),
    lat_max = c(89.975, 90, 46, -50, -30, -30, -40, NA),
    lon_step = 0.05, lat_step = 0.05
  )
  got <- copernicopia:::.overlaps_area(d, area)
  expect_equal(stats::setNames(got, d$name),
               c(global = TRUE, arctic = FALSE, med = FALSE, antarctic = TRUE,
                 `south atlantic gap` = FALSE, `pacific 0-360` = TRUE, edge = TRUE,
                 unknown = TRUE))
  # A single point
  expect_equal(copernicopia:::.overlaps_area(d[2:3, ], list(lon = c(10, 10), lat = c(40, 40))),
               c(FALSE, TRUE))
})

test_that("step C lists products covering the data's area first, and ALL shows the rest", {
  cat_df <- fake_catalogue()
  north <- cat_df$dataset_id == "synth_ice-north_P1D"
  cat_df$lat_min[north] <- 50
  cat_df$lat_max[north] <- 90
  south <- cat_df$dataset_id == "synth_ice-south_P1D"
  cat_df$lat_min[south] <- -90
  cat_df$lat_max[south] <- -50
  sel <- list(label = "Sea ice concentration", rows = which(cat_df$variable == "ice_conc"))
  area <- list(lon = c(100, 160), lat = c(-70, -30))
  pick <- function(answers, area_ = area) {
    withr::local_options(copernicopia.input = script_input(answers))
    out <- capture.output(res <- copernicopia:::.choose_dataset(cat_df, sel, area = area_))
    list(res = res, out = out)
  }
  # Only the southern product is listed; Enter chooses it
  r <- pick("")
  expect_equal(r$res$dataset_id, "synth_ice-south_P1D")
  expect_true(any(grepl("Products with Sea ice concentration covering your data's area (1):",
                        r$out, fixed = TRUE)))
  expect_true(any(grepl("(1 product outside your data's area is hidden; type ALL to show it.)",
                        r$out, fixed = TRUE)))
  expect_false(any(grepl("synth_ice-north_P1D", r$out)))
  # ALL lists both, marking the one outside the area
  # ALL lists both: those covering the area first, the other marked
  r <- pick(c("all", "2"))
  expect_true(any(grepl("[2] synth_ice-north_P1D  [outside your data's area]", r$out, fixed = TRUE)))
  expect_equal(r$res$dataset_id, "synth_ice-north_P1D")
  # Filtering for a hidden product points to ALL
  r <- pick(c("north", "0"))
  expect_true(any(grepl("but 1 product outside your data's area does; type ALL to show it",
                        r$out, fixed = TRUE)))
  # If nothing covers the area, everything is listed
  r <- pick("2", list(lon = c(100, 160), lat = c(-10, 10)))
  expect_true(any(grepl("None of the products with Sea ice concentration covers your data's area",
                        r$out, fixed = TRUE)))
  expect_equal(r$res$dataset_id, "synth_ice-south_P1D")
})

test_that("the wizard does not repeat cmems_request()'s note about wide regions", {
  cat_df <- fake_catalogue()
  local_mocked_bindings(
    cmems_login = function(...) invisible(TRUE),
    cmems_catalogue = function(...) cat_df,
    cmems_download = function(...) stop("should not download")
  )
  answers <- c("7", "", "1", "100", "-100", "y", "-60", "-40", "2025-07-01", "2025-07-31",
               "", file.path(tempdir(), "x"), "n", "n")
  withr::local_options(copernicopia.input = script_input(answers))
  msgs <- capture_messages(capture.output(req <- cmems_wizard()))
  expect_equal(req$lon, c(100, -100))
  expect_false(any(grepl("Note: lon =", msgs, fixed = TRUE)))
})
