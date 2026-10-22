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
