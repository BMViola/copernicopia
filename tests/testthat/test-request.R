test_that("requests are built and validated", {
  cat_df <- fake_catalogue()
  req <- cmems_request(list("synth_phy_P1D-m" = c("thetao", "zos"), "synth_chl_P1M" = "CHL"),
                       lon = c(100, 160), lat = c(-70, -30),
                       time = c("2025-07-10", "2025-09-30"), out_dir = tempdir(),
                       catalogue = cat_df)
  expect_s3_class(req, "cmems_request")
  expect_equal(req$datasets[["synth_phy_P1D-m"]]$depth, c(0, 1))
  expect_null(req$datasets[["synth_chl_P1M"]]$depth)
  expect_equal(unname(req$datasets[["synth_chl_P1M"]]$units), "mg m-3")
  code <- format(req)
  expect_silent(parse(text = code))
  expect_output(print(req), "cmems_request\\(")
  expect_error(cmems_request(list("x" = "a"), c(10, 10), c(0, 1), c("2025-01-01", "2025-01-02"),
                             catalogue = cat_df), "lon")
  expect_warning(cmems_request(list("synth_chl_P1M" = "nope"), c(0, 1), c(0, 1),
                               c("2025-01-01", "2025-01-02"), catalogue = cat_df), "nope")
})

test_that("downloads are split by the chosen unit", {
  period <- as.Date(c("2025-07-10", "2026-02-03"))
  e <- list(dataset_id = "ds")
  fnames <- function(ch) vapply(seq_len(nrow(ch)), function(i) copernicopia:::.chunk_filename(e, ch[i, ]), "")

  # Default: one file per month, partial months named by date
  ch <- copernicopia:::.time_chunks(period, 24)
  expect_equal(nrow(ch), 8)
  expect_equal(ch$start[1:2], as.Date(c("2025-07-10", "2025-08-01")))
  expect_equal(ch$end[c(1, 8)], as.Date(c("2025-07-31", "2026-02-03")))
  expect_equal(fnames(ch)[c(1, 2, 8)],
               c("ds_20250710-20250731.nc", "ds_202508.nc", "ds_20260201-20260203.nc"))

  ch <- copernicopia:::.time_chunks(as.Date(c("2025-07-30", "2025-08-02")), 24, "day")
  expect_equal(fnames(ch), paste0("ds_", c("20250730", "20250731", "20250801", "20250802"), ".nc"))

  ch <- copernicopia:::.time_chunks(period, 24, "year")
  expect_equal(fnames(ch), c("ds_20250710-20251231.nc", "ds_20260101-20260203.nc"))
  ch <- copernicopia:::.time_chunks(as.Date(c("2024-01-01", "2025-12-31")), 24, "year")
  expect_equal(fnames(ch), c("ds_2024.nc", "ds_2025.nc"))

  ch <- copernicopia:::.time_chunks(period, 24, "all")
  expect_equal(fnames(ch), "ds_20250710-20260203.nc")
})

test_that("coarse products are never split finer than their time step", {
  monthly <- 24 * 30.4
  yearly <- 24 * 365
  expect_equal(copernicopia:::.effective_chunk("day", monthly), "month")
  expect_equal(copernicopia:::.effective_chunk("day", 24 * 8), "month")
  expect_equal(copernicopia:::.effective_chunk("month", yearly), "year")
  expect_equal(copernicopia:::.effective_chunk("all", monthly), "all")
  expect_equal(copernicopia:::.effective_chunk("day", NA), "day")
  expect_equal(copernicopia:::.effective_chunk("day", 1), "day")

  # Monthly data are downloaded for whole months, so every date has its value
  ch <- copernicopia:::.time_chunks(as.Date(c("2025-07-10", "2025-09-05")), monthly, "day")
  expect_equal(ch$start, as.Date(c("2025-07-01", "2025-08-01", "2025-09-01")))
  expect_equal(ch$end[3], as.Date("2025-09-30"))
  ch <- copernicopia:::.time_chunks(as.Date(c("2025-07-10", "2025-09-05")), monthly, "all")
  expect_equal(c(ch$start, ch$end), as.Date(c("2025-07-01", "2025-09-30")))
  ch <- copernicopia:::.time_chunks(as.Date(c("2025-07-10", "2026-03-05")), yearly)
  expect_equal(c(ch$start[1], ch$end[2]), as.Date(c("2025-01-01", "2026-12-31")))
  # 8-day data: widened by one time step at each end
  ch <- copernicopia:::.time_chunks(as.Date(c("2025-07-10", "2025-07-20")), 24 * 8, "all")
  expect_equal(c(ch$start, ch$end), as.Date(c("2025-07-02", "2025-07-28")))
})

test_that("chunk is validated, printed and part of the folder name", {
  cat_df <- fake_catalogue()
  make <- function(chunk) {
    cmems_request(list("synth_chl_P1M" = "CHL"), c(100, 160), c(-70, -30),
                  c("2025-07-01", "2025-07-31"), chunk = chunk, catalogue = cat_df)
  }
  expect_equal(cmems_request(list("synth_chl_P1M" = "CHL"), c(100, 160), c(-70, -30),
                             c("2025-07-01", "2025-07-31"), catalogue = cat_df)$chunk, "month")
  expect_error(make("week"), "should be one of")
  r <- make("day")
  expect_true(any(grepl('chunk = "day"', format(r))))
  expect_output(print(r), "one file per day")
  expect_false(copernicopia:::.dataset_dir(r, r$datasets[[1]]) ==
                 copernicopia:::.dataset_dir(make("year"), r$datasets[[1]]))
})

test_that("different regions or variables get different folders", {
  cat_df <- fake_catalogue()
  r1 <- cmems_request(list("synth_chl_P1M" = "CHL"), c(100, 160), c(-70, -30),
                      c("2025-07-01", "2025-07-31"), catalogue = cat_df)
  r2 <- cmems_request(list("synth_chl_P1M" = "CHL"), c(100, 150), c(-70, -30),
                      c("2025-07-01", "2025-07-31"), catalogue = cat_df)
  d1 <- copernicopia:::.dataset_dir(r1, r1$datasets[[1]])
  d2 <- copernicopia:::.dataset_dir(r2, r2$datasets[[1]])
  expect_false(d1 == d2)
  expect_match(basename(d1), "^synth_chl_P1M_[0-9a-f]{8}$")
})

test_that("time columns of different types convert to UTC milliseconds", {
  ms <- 1752537600000 # 2025-07-15 00:00 UTC
  expect_equal(copernicopia:::.as_utc_ms(as.Date("2025-07-15")), ms)
  expect_equal(copernicopia:::.as_utc_ms("2025-07-15"), ms)
  expect_equal(copernicopia:::.as_utc_ms("2025-07-15 13:00"), ms + 13 * 3600000)
  expect_equal(copernicopia:::.as_utc_ms(as.POSIXct("2025-07-15 23:00", tz = "Australia/Melbourne")),
               ms + 13 * 3600000)
  expect_error(copernicopia:::.as_utc_ms(1:3), "time column")
})
