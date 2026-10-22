test_that("longitude ranges are read west to east, and may cross 180", {
  n <- copernicopia:::.normalise_lon
  expect_equal(n(c(100, 160)), c(100, 160))
  expect_equal(n(c(170, -170)), c(170, -170))   # the 20-degree strip across 180
  expect_equal(n(c(170, 190)), c(170, -170))    # the same, written 0-360 style
  expect_equal(n(c(-170, 170)), c(-170, 170))   # everything except that strip
  expect_equal(n(c(10, -10)), c(10, -10))       # 340 degrees, crossing 180
  expect_equal(n(c(0, 180)), c(0, 180))
  expect_equal(n(c(170, 180)), c(170, 180))
  expect_equal(n(c(-180, 180)), c(-180, 180))
  expect_equal(n(c(0, 360)), c(-180, 180))
  expect_error(n(c(20, 20)), "must differ")
  expect_error(n(c(20, NA)), "c\\(west, east\\)")
  expect_error(n(1), "c\\(west, east\\)")

  w <- copernicopia:::.lon_width
  expect_equal(w(c(170, -170)), 20)
  expect_equal(w(c(-170, 170)), 340)
  expect_equal(w(c(-180, 180)), 360)
  x <- copernicopia:::.lon_crosses_180
  expect_true(x(c(170, -170)))
  expect_false(x(c(-170, 170)))
  expect_false(x(c(170, 180)))
  expect_false(x(c(-180, 180)))
})

test_that("ranges are described plainly", {
  withr::local_locale(LC_CTYPE = "C.UTF-8")
  label <- copernicopia:::.lon_label
  expect_equal(label(c(170, -170)), "170°E to 170°W, crossing 180° (20° wide)")
  expect_equal(label(c(100, 160.5)), "100°E to 160.5°E (60.5° wide)")
  expect_equal(label(c(-10, 10)), "10°W to 10°E (20° wide)")
  expect_equal(label(c(-180, 180)), "all longitudes (360°)")
  text <- copernicopia:::.lon_text
  expect_equal(text(c(100, 160)), "100 to 160")
  expect_equal(text(c(170, -170)), "170 to -170 (crossing 180 degrees; 20 degrees wide)")
  expect_equal(text(c(-180, 180)), "-180 to 180 (all longitudes)")
})

test_that("the extent of data crossing 180 is the short way round", {
  ext <- copernicopia:::.lon_extent
  expect_equal(ext(c(179, -179, 178.5)), c(178.5, -179))
  expect_equal(ext(c(179, 181)), c(179, -179))          # 0-360 style input
  expect_equal(ext(c(110.2, 150, 130)), c(110.2, 150))
  expect_equal(ext(c(-10, 10)), c(-10, 10))
  expect_equal(ext(c(5, 5, NA)), c(5, 5))
})

test_that("padding works across 180 and saturates at all longitudes", {
  pad <- copernicopia:::.lon_pad
  expect_equal(pad(c(100, 160), 0.5), c(99.5, 160.5))
  expect_equal(pad(c(170, -170), 0.5), c(169.5, 190.5))
  expect_equal(pad(c(-180, -170), 0.5), c(-180.5, -169.5))
  expect_equal(pad(c(5, 5), 0.25), c(4.75, 5.25))
  expect_equal(pad(c(-179.8, 179.8), 0.5), c(-180, 180))
  expect_equal(pad(c(-180, 180), 0.5), c(-180, 180))
})

test_that("requests accept regions crossing 180, and flag likely swaps", {
  cat_df <- fake_catalogue()
  make <- function(lon) {
    cmems_request(list("synth_chl_P1M" = "CHL"), lon, c(-70, -30),
                  c("2025-07-01", "2025-07-31"), catalogue = cat_df)
  }
  r <- make(c(170, 190))
  expect_equal(r$lon, c(170, -170))
  expect_true(any(grepl("lon = c(170, -170)", format(r), fixed = TRUE)))
  expect_output(print(r), "crossing 180")
  expect_silent(make(c(170, -170)))
  expect_silent(make(c(-170, 170)))
  expect_message(make(c(150, 110)), "use lon = c\\(110, 150\\)")
  expect_error(make(c(20, 20)), "must differ")
  # The toolbox gets a continuous range one grid cell wider on each side
  b <- copernicopia:::.padded_bounds(r, r$datasets[[1]])
  expect_equal(b$lon, c(169.5, 190.5))
})

test_that("values are extracted from a file that crosses 180", {
  skip_if_no_python()
  out_dir <- withr::local_tempdir()
  req <- cmems_request(list("synth_chl_P1M" = "CHL"), c(170, -170), c(-70, -30),
                       c("2025-07-01", "2025-07-31"), out_dir = out_dir,
                       catalogue = fake_catalogue())
  dir <- copernicopia:::.dataset_dir(req, req$datasets[[1]])
  dir.create(dir, recursive = TRUE)
  # As the toolbox writes it: longitudes run continuously from 169.5 to 190.5
  reticulate::py_run_string("
import numpy as np, pandas as pd, xarray as xr
def _crossing_file(path):
    lon = np.arange(169.5, 190.51, 0.5); lat = np.arange(-70, -29.9, 0.5)
    t = pd.date_range('2025-07-01', periods=1, freq='MS')
    v = lon[None, None, :] + 0 * lat[None, :, None]   # value = longitude
    xr.Dataset({'CHL': (('time', 'latitude', 'longitude'), v)},
               coords=dict(time=t, latitude=lat, longitude=lon)).to_netcdf(path, engine='h5netcdf')
")
  reticulate::py$`_crossing_file`(file.path(dir, "synth_chl_P1M_202507.nc"))
  obs <- data.frame(lon = c(175, -175, 180, -180, 160, 185),
                    lat = -50, date = as.Date("2025-07-15"))
  res <- suppressMessages(cmems_extract(obs, req))
  expect_equal(res$CHL, c(175, 185, 180, 180, NA, 185))
})

test_that("the wizard suggests a region crossing 180 for data that does", {
  cat_df <- fake_catalogue()
  local_mocked_bindings(
    cmems_login = function(...) invisible(TRUE),
    cmems_catalogue = function(...) cat_df,
    cmems_download = function(...) stop("should not download")
  )
  obs <- data.frame(lon = c(179.5, -179.5, 179.8), lat = c(-50, -51, -52),
                    date = as.Date("2025-07-15"))
  answers <- c("", "7", "", "1",
               "", "",          # longitude defaults
               "", "",          # latitude defaults
               "", "", "",      # dates and file split
               file.path(tempdir(), "x"), "n", "n")
  withr::local_options(copernicopia.input = script_input(answers))
  out <- capture.output(req <- cmems_wizard(obs))
  expect_equal(req$lon, c(179.2, -179.2))
  expect_true(any(grepl("crossing 180", out)))
  expect_true(any(grepl("Western edge (longitude): [179.2]", out, fixed = TRUE)))
  expect_true(any(grepl("Eastern edge (longitude): [-179.2]", out, fixed = TRUE)))
})

test_that("the wizard double-checks a wide region crossing 180", {
  cat_df <- fake_catalogue()
  local_mocked_bindings(
    cmems_login = function(...) invisible(TRUE),
    cmems_catalogue = function(...) cat_df,
    cmems_download = function(...) stop("should not download")
  )
  answers <- c("7", "", "1",
               "150", "110",   # swapped by mistake: 320 degrees across 180
               "n",            # not what I meant
               "110", "150",   # corrected
               "-60", "-40", "2025-07-01", "2025-07-31", "",
               file.path(tempdir(), "x"), "n", "n")
  withr::local_options(copernicopia.input = script_input(answers))
  out <- capture.output(req <- cmems_wizard())
  expect_equal(req$lon, c(110, 150))
  expect_true(any(grepl("That region is 320", out)))
  expect_true(any(grepl("enter 110 as the western edge and 150 as the eastern edge", out)))
})
