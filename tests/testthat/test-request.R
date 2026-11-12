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

test_that("text times are read one value at a time, strictly year-month-day", {
  ms <- copernicopia:::.as_utc_ms
  day <- 1752537600000 # 2025-07-15 00:00 UTC
  h <- 3600000
  # A column mixing dates with and without a time of day, which is how
  # write.csv() writes times that fall on midnight
  expect_equal(ms(c("2025-07-15 23:00:00", "2025-07-16", "2025-07-16 01:00:00")),
               day + c(23, 24, 25) * h)
  # Other spellings of a time
  expect_equal(ms(c("2025-07-15T13:00", "2025/07/15 13:00:00", "2025-07-15 13:00:00.5",
                    "2025-7-15 9:05")),
               day + c(13 * h, 13 * h, 13 * h + 500, 9 * h + 5 * 60000))
  # Time zones are applied, not ignored
  expect_equal(ms(c("2025-07-15T13:00:00Z", "2025-07-15T13:00:00+10:00", "2025-07-15 13:00 -0930",
                    "2025-07-15T13:00:00+10", "2025-07-15 13:00:00 UTC")),
               day + c(13, 3, 22.5, 3, 13) * h)
  # Missing values stay missing
  expect_equal(ms(c("2025-07-15", NA, "", "  ")), c(day, NA, NA, NA))
  expect_equal(ms(factor(c("2025-07-15", NA))), c(day, NA))
  expect_equal(ms(character(0)), numeric(0))
  # Anything else is an error rather than a guess
  expect_error(ms("15/07/2025"),
               "Could not read 1 of the 1 time value\\(s\\), for example \"15/07/2025\"")
  expect_error(ms(c("2025-07-15", "15-07-2025", "07/15/2025")), "Could not read 2 of the 3")
  expect_error(ms("2025-07-15 13:00 AEST"), "Could not read")
  expect_error(ms("2025-02-30"), "Could not read")
  expect_error(ms("2025-07-15 25:00"), "Could not read")
  expect_error(ms("20250715"), "year-month-day")
})

test_that("hourly times survive a round trip through a CSV file", {
  obs <- data.frame(time = as.POSIXct("2025-07-15 22:00", tz = "UTC") + 3600 * 0:4)
  f <- withr::local_tempfile(fileext = ".csv")
  write.csv(obs, f, row.names = FALSE)
  back <- read.csv(f)
  expect_type(back$time, "character")
  expect_equal(copernicopia:::.as_utc_ms(back$time), copernicopia:::.as_utc_ms(obs$time))
})

test_that("request dates must be dates written year-month-day", {
  cat_df <- fake_catalogue()
  make <- function(time) {
    cmems_request(list("synth_chl_P1M" = "CHL"), c(100, 160), c(-70, -30), time, catalogue = cat_df)
  }
  july <- as.Date(c("2025-07-01", "2025-07-31"))
  expect_equal(make(c("2025-07-01", "2025-07-31"))$time, july)
  expect_equal(make(july)$time, july)
  expect_equal(make(c("2025/07/01", "2025-07-31T12:00:00Z"))$time, july)
  expect_equal(make(as.POSIXct(c("2025-07-01 08:00", "2025-07-31 08:00"), tz = "UTC"))$time, july)
  # Day/month/year used to be read as the years 1 and 31
  expect_error(make(c("01/07/2025", "31/07/2025")), "Could not read 2 of the 2 `time` value")
  expect_error(make(c(20250701, 20250731)), "`time`")
  expect_error(make(c("2025-07-31", "2025-07-01")), "start <= end")
  expect_error(make("2025-07-01"), "c\\(start, end\\)")
  expect_error(make(c("2025-07-01", NA)), "c\\(start, end\\)")
})

test_that("depth ranges are checked for datasets with a depth axis", {
  cat_df <- fake_catalogue()
  make <- function(depth, datasets = list("synth_phy_P1D-m" = "thetao")) {
    cmems_request(datasets, c(100, 160), c(-70, -30), c("2025-07-01", "2025-07-31"),
                  depth = depth, catalogue = cat_df)
  }
  expect_equal(make(c(0, 50))$datasets[[1]]$depth, c(0, 50))
  expect_equal(make(c(5L, 5L))$datasets[[1]]$depth, c(5, 5))
  expect_equal(make(list(other = c(0, 5)))$datasets[[1]]$depth, c(0, 1))
  expect_error(make(c(10, 0)), "`depth` for 'synth_phy_P1D-m' must be c\\(min, max\\)")
  expect_error(make(5), "`depth`")
  expect_error(make("surface"), "`depth`")
  expect_error(make(c(0, NA)), "`depth`")
  expect_error(make(list("synth_phy_P1D-m" = c(10, 0))), "`depth`")
  # Datasets without a depth axis ignore it, so it is not checked for them
  expect_null(make(c(10, 0), list("synth_chl_P1M" = "CHL"))$datasets[[1]]$depth)
})
