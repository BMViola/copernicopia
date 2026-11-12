test_that("values are appended with daily, monthly and depth matching", {
  skip_if_no_python()
  cat_df <- fake_catalogue()
  out_dir <- file.path(tempdir(), "extract-test")
  req <- cmems_request(list("synth_phy_P1D-m" = c("thetao", "zos"), "synth_chl_P1M" = "CHL"),
                       lon = c(100, 160), lat = c(-70, -30),
                       time = c("2025-07-01", "2025-08-31"), depth = c(0, 10),
                       out_dir = out_dir, catalogue = cat_df)
  phy <- copernicopia:::.dataset_dir(req, req$datasets[[1]])
  chl <- copernicopia:::.dataset_dir(req, req$datasets[[2]])
  dir.create(phy, recursive = TRUE)
  dir.create(chl, recursive = TRUE)
  write_synthetic(file.path(phy, "synth_phy_P1D-m_202507.nc"), c("thetao", "zos"),
                  "2025-07-01", "2025-07-31", depth = TRUE)
  write_synthetic(file.path(phy, "synth_phy_P1D-m_202508.nc"), c("thetao", "zos"),
                  "2025-08-01", "2025-08-31", depth = TRUE)
  write_synthetic(file.path(chl, "synth_chl_P1M_20250701-20250831.nc"), "CHL",
                  "2025-07-01", "2025-08-01", monthly = TRUE)
  # A stray temporary file from an interrupted download must be ignored
  file.copy(file.path(chl, "synth_chl_P1M_20250701-20250831.nc"),
            file.path(chl, "tmp_broken.nc"))

  obs <- data.frame(
    longitude = c(110.2, 150, 130, 130, 99, 130),
    latitude = c(-50.1, -45, -69.9, -50, -50, -50),
    when = as.POSIXct(c("2025-07-15 23:00", "2025-08-31 06:00", "2025-07-01 00:00",
                        "2025-08-10 12:00", "2025-07-10 00:00", "2025-09-02 00:00"), tz = "UTC"),
    d = c(0, 6, 0, 20, 0, 0),
    zos = 1
  )
  msgs <- capture_messages(
    res <- cmems_extract(obs, req, time = "when", depth = "d", keep_matched_time = TRUE)
  )
  expect_match(msgs[1], "synth_phy_P1D-m: values for 4 of 6 rows")
  expect_match(msgs[2], "1 outside the downloaded period")
  exp_day <- expected_daily(as.Date(obs$when), obs$latitude)
  expect_equal(res$thetao[1:4], exp_day[1:4] + c(0, 100, 0, 200), tolerance = 1e-6)
  expect_true(all(is.na(res$thetao[5:6])))
  # zos already existed in the data, so the new column gets a dataset suffix
  expect_true("zos_synth_phy_P1D_m" %in% names(res))
  expect_equal(res$zos, rep(1, 6))
  # monthly data: matched on calendar month (value = month + lat/1000)
  expect_equal(res$CHL[1:4], c(7, 8, 7, 8) + round(obs$latitude[1:4] * 2) / 2 / 1000,
               tolerance = 1e-6)
  expect_true(is.na(res$CHL[6]))
  expect_s3_class(res$thetao_matched_time, "POSIXct")
  src <- cmems_sources(res)
  expect_equal(src$column, c("thetao", "zos_synth_phy_P1D_m", "CHL"))
  expect_equal(src$units, c("degC", "m", "mg m-3"))
})

test_that("missing files give a clear warning", {
  skip_if_no_python()
  req <- cmems_request(list("synth_chl_P1M" = "CHL"), c(100, 160), c(-70, -30),
                       c("2025-07-01", "2025-07-31"), out_dir = file.path(tempdir(), "none"),
                       catalogue = fake_catalogue())
  obs <- data.frame(lon = 110, lat = -50, date = as.Date("2025-07-02"))
  expect_warning(res <- cmems_extract(obs, req), "cmems_download")
  expect_equal(names(res), names(obs))
})

# A request with one month of daily synthetic files in place, for the tests below
extract_fixture <- function(env = parent.frame()) {
  out_dir <- withr::local_tempdir(.local_envir = env)
  req <- cmems_request(list("synth_phy_P1D-m" = c("thetao", "zos")), lon = c(100, 160),
                       lat = c(-70, -30), time = c("2025-07-01", "2025-07-31"), depth = c(0, 10),
                       out_dir = out_dir, catalogue = fake_catalogue())
  dir <- copernicopia:::.dataset_dir(req, req$datasets[[1]])
  dir.create(dir, recursive = TRUE)
  write_synthetic(file.path(dir, "synth_phy_P1D-m_202507.nc"), c("thetao", "zos"),
                  "2025-07-01", "2025-07-31", depth = TRUE)
  req
}

test_that("a depth column that does not exist is reported by name", {
  req <- cmems_request(list("synth_phy_P1D-m" = "thetao"), c(100, 160), c(-70, -30),
                       c("2025-07-01", "2025-07-31"), out_dir = file.path(tempdir(), "none"),
                       catalogue = fake_catalogue())
  obs <- data.frame(lon = 130, lat = -50, date = as.Date("2025-07-15"), depth = 5)
  expect_error(cmems_extract(obs, req, depth = "depth_m"),
               "Could not find the depth column \"depth_m\" in `data`")
  expect_error(cmems_extract(obs, req, depth = 5), "`depth` must name one of its columns")
})

test_that("text times written day/month/year stop with an explanation", {
  req <- cmems_request(list("synth_phy_P1D-m" = "thetao"), c(100, 160), c(-70, -30),
                       c("2025-07-01", "2025-07-31"), out_dir = file.path(tempdir(), "none"),
                       catalogue = fake_catalogue())
  obs <- data.frame(lon = 130, lat = -50, date = c("15/07/2025", "16/07/2025"))
  expect_error(cmems_extract(obs, req), "year-month-day.*as.Date\\(x, format = \"%d/%m/%Y\"\\)")
})

test_that("text times with and without a time of day are matched to the right hour", {
  skip_if_no_python()
  cat_df <- fake_catalogue()
  cat_df$time_step_hours[cat_df$dataset_id == "synth_old_P1D"] <- 1
  req <- cmems_request(list("synth_old_P1D" = "analysed_sst"), c(100, 160), c(-70, -30),
                       c("2025-07-15", "2025-07-16"), out_dir = withr::local_tempdir(),
                       catalogue = cat_df)
  dir <- copernicopia:::.dataset_dir(req, req$datasets[[1]])
  dir.create(dir, recursive = TRUE)
  # Hourly data whose value is the hour of the day
  reticulate::py_run_string("
import numpy as np, pandas as pd, xarray as xr
def _hourly_file(path):
    lat = np.arange(-70, -29.9, 0.5); lon = np.arange(100, 160.01, 0.5)
    t = pd.date_range('2025-07-15', '2025-07-16 23:00', freq='h')
    v = t.hour.values.astype(float)[:, None, None] + 0 * lat[None, :, None] + 0 * lon[None, None, :]
    xr.Dataset({'analysed_sst': (('time', 'latitude', 'longitude'), v)},
               coords=dict(time=t, latitude=lat, longitude=lon)).to_netcdf(path, engine='h5netcdf')
")
  reticulate::py$`_hourly_file`(file.path(dir, "synth_old_P1D_20250715-20250716.nc"))
  obs <- data.frame(lon = 130, lat = -50,
                    time = c("2025-07-15 22:00:00", "2025-07-15 23:00:00", "2025-07-16",
                             "2025-07-16 01:00:00", "2025-07-16T12:00:00+10:00"))
  res <- suppressMessages(cmems_extract(obs, req))
  expect_equal(res$analysed_sst, c(22, 23, 0, 1, 2))
})

test_that("appending the same request again lists each column once", {
  skip_if_no_python()
  req <- extract_fixture()
  obs <- data.frame(lon = 130, lat = -50, date = as.Date("2025-07-15"))
  once <- suppressMessages(cmems_extract(obs, req))
  twice <- suppressMessages(cmems_extract(once, req))
  thrice <- suppressMessages(cmems_extract(twice, req))
  expect_equal(names(thrice), names(twice))
  expect_equal(cmems_sources(thrice)$column,
               c("thetao", "zos", "thetao_synth_phy_P1D_m", "zos_synth_phy_P1D_m"))
  expect_equal(cmems_sources(thrice), cmems_sources(twice))
})

test_that("rows with missing coordinates get NA, without NumPy warnings", {
  skip_if_no_python()
  req <- extract_fixture()
  # Any "invalid value encountered" warning from NumPy becomes an error here
  reticulate::py_run_string(paste(
    "import warnings as _w", "_saved_filters = _w.filters[:]",
    "_w.filterwarnings('error', message='invalid value encountered')", sep = "\n"))
  withr::defer(reticulate::py_run_string("_w.filters[:] = _saved_filters"))
  obs <- data.frame(lon = c(130, NA, 130, 130), lat = c(-50, -50, NA, -50),
                    date = as.Date(c("2025-07-15", "2025-07-15", "2025-07-15", NA)), d = c(0, 0, 0, NA))
  res <- suppressMessages(cmems_extract(obs, req, depth = "d"))
  expect_equal(is.na(res$thetao), c(FALSE, TRUE, TRUE, TRUE))
})

test_that("a file with a single row or column only matches points inside that cell", {
  skip_if_no_python()
  h <- copernicopia:::.helpers()
  # (whole-number arrays come back from Python unconverted)
  index <- function(x) {
    if (inherits(x, "python.builtin.object")) x <- x$tolist()
    as.integer(unlist(x))
  }
  # Without the grid step, a single coordinate cannot say how big its cell is
  expect_equal(index(h$`_nearest_index`(list(-42.5), list(-12.5, -42.4))), c(0L, 0L))
  expect_equal(index(h$`_nearest_index`(list(-42.5), list(-12.5, -42.4, -42.8), step = 0.5)),
               c(-1L, 0L, -1L))
  expect_equal(index(h$`_nearest_lon_index`(list(145), list(10, 145.1, 144.8, 145.3, 505.1), step = 0.5)),
               c(-1L, 0L, 0L, -1L, 0L))
  expect_equal(index(h$`_nearest_lon_index`(list(180), list(179.9, -179.9, 179), step = 0.5)),
               c(0L, 0L, -1L))

  # Through cmems_extract(): a download that reached just one row of the grid
  req <- cmems_request(list("synth_chl_P1M" = "CHL"), c(100, 160), c(-90, -80.2),
                       c("2025-07-01", "2025-07-31"), out_dir = withr::local_tempdir(),
                       catalogue = fake_catalogue())
  dir <- copernicopia:::.dataset_dir(req, req$datasets[[1]])
  dir.create(dir, recursive = TRUE)
  reticulate::py_run_string("
import numpy as np, pandas as pd, xarray as xr
def _one_row_file(path):
    lon = np.arange(100, 160.01, 0.5); lat = np.array([-80.0])
    t = pd.date_range('2025-07-01', periods=1, freq='MS')
    v = np.ones((1, 1, lon.size))
    xr.Dataset({'CHL': (('time', 'latitude', 'longitude'), v)},
               coords=dict(time=t, latitude=lat, longitude=lon)).to_netcdf(path, engine='h5netcdf')
")
  reticulate::py$`_one_row_file`(file.path(dir, "synth_chl_P1M_202507.nc"))
  obs <- data.frame(lon = 130, lat = c(-80.1, -80.2, -80.4, -85), date = as.Date("2025-07-15"))
  res <- suppressMessages(cmems_extract(obs, req))
  expect_equal(res$CHL, c(1, 1, NA, NA))
})
