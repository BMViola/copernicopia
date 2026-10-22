download_two_products <- function(out_dir, time = c("2025-07-30", "2025-08-01")) {
  req <- cmems_request(list("synth_phy_P1D-m" = c("thetao", "zos"), "synth_chl_P1M" = "CHL"),
                       c(100, 160), c(-70, -30), time, depth = c(0, 5), chunk = "day",
                       out_dir = out_dir, catalogue = fake_catalogue())
  msgs <- capture_messages(cmems_download(req))
  list(req = req, msgs = msgs)
}

test_that("downloads record their sources and keep a README up to date", {
  skip_if_no_python()
  use_fake_worker_toolbox()
  out_dir <- withr::local_tempdir()
  run <- download_two_products(out_dir)
  readme <- file.path(out_dir, "README_sources.txt")
  expect_true(file.exists(readme))
  expect_match(paste(run$msgs, collapse = ""), "Sources for citing are in .*README_sources.txt")

  text <- capture.output(res <- cmems_getreadme(out_dir))
  expect_equal(readLines(readme), res)
  all <- paste(res, collapse = "\n")
  today <- copernicopia:::.fmt_date(Sys.Date())

  # Acknowledgement with every product DOI as a link (cleaned of any prefix);
  # products are listed by title
  ack <- paste(res[(which(res == "Acknowledgement") + 2):(which(res == "References") - 2)],
               collapse = " ")
  expect_equal(ack, paste0("This study has been conducted using E.U. Copernicus Marine Service ",
                           "Information; https://doi.org/10.0000/test-oc; ",
                           "https://doi.org/10.0000/test-phy"))
  # One reference per product, in the Copernicus Marine format
  refs <- gsub("\\s+", " ", paste(res, collapse = " "))
  expect_match(refs, paste0("Synthetic ocean physics. E.U. Copernicus Marine Service Information ",
                            "(CMEMS). Marine Data Store (MDS). DOI: 10.0000/test-phy (Accessed on ",
                            today, ")"), fixed = TRUE)
  expect_match(refs, "DOI: 10.0000/test-oc (Accessed on", fixed = TRUE)
  # Details of each download
  expect_match(all, "Dataset ID:      synth_phy_P1D-m (version 202501)", fixed = TRUE)
  expect_match(all, "Variables:       thetao: sea_water_potential_temperature (degC)", fixed = TRUE)
  expect_match(all, "                 zos: sea_surface_height_above_geoid (m)", fixed = TRUE)
  expect_match(all, "Time step:       monthly", fixed = TRUE)
  expect_match(all, "Resolution:      0.5 x 0.5 degrees (longitude x latitude; about 56 km",
               fixed = TRUE)
  expect_match(all, "Region:          longitude 100 to 160, latitude -70 to -30", fixed = TRUE)
  expect_match(all, "Depth:           0 to 5 m", fixed = TRUE)
  expect_match(all, "2025-07-30 to 2025-08-01 requested; files cover 2025-07-30 to 2025-08-01",
               fixed = TRUE)
  expect_match(all, "2025-07-30 to 2025-08-01 requested; files cover 2025-07-01 to 2025-08-31",
               fixed = TRUE)
  expect_match(all, "Files:           3 (one file per day)", fixed = TRUE)
  expect_match(all, "Files:           2 (one file per month)", fixed = TRUE)
  expect_match(all, paste("Downloaded:     ", today), fixed = TRUE)
  expect_match(all, "Software:        copernicusmarine toolbox [0-9.]+, copernicopia [0-9.]+")
  expect_match(refs, "Copernicus Marine datasets do not have DOIs of their own", fixed = TRUE)
})

test_that("cmems_getreadme() can list just one request, and save to a file", {
  skip_if_no_python()
  use_fake_worker_toolbox()
  out_dir <- withr::local_tempdir()
  run <- download_two_products(out_dir)
  # A second, unrelated download into the same folder
  other <- cmems_request(list("synth_ice-south_P1D" = "ice_conc"), c(100, 160), c(-70, -30),
                         c("2025-07-30", "2025-07-30"), out_dir = out_dir,
                         catalogue = fake_catalogue())
  suppressMessages(cmems_download(other))

  everything <- gsub("\\s+", " ", paste(capture.output(cmems_getreadme(out_dir)), collapse = " "))
  expect_match(everything, "Synthetic sea ice south")
  # The ice product has no DOI: say so rather than invent one
  expect_match(everything, "Product ID: SYNTH (Accessed on", fixed = TRUE)
  expect_match(everything, "No DOI was listed for this product", fixed = TRUE)

  just_req <- capture.output(cmems_getreadme(run$req))
  expect_false(any(grepl("sea ice", just_req, ignore.case = TRUE)))
  expect_true(any(grepl("Synthetic ocean colour", just_req)))

  # A data frame from cmems_extract() carries its request
  obs <- data.frame(lon = 110, lat = -50, date = as.Date("2025-07-31"))
  attr(obs, "cmems_request") <- run$req
  expect_equal(capture.output(cmems_getreadme(obs)), just_req)

  f <- withr::local_tempfile(fileext = ".txt")
  msgs <- capture_messages(capture.output(cmems_getreadme(run$req, file = f)))
  expect_match(msgs, "Saved to")
  expect_equal(readLines(f), just_req)
})

test_that("the first download date is kept when files are added later", {
  skip_if_no_python()
  use_fake_worker_toolbox()
  out_dir <- withr::local_tempdir()
  run <- download_two_products(out_dir)
  dir <- copernicopia:::.dataset_dir(run$req, run$req$datasets[[1]])
  info <- read.dcf(file.path(dir, "source_info.dcf"))
  info[, "First_downloaded"] <- "2026-01-05"
  write.dcf(info, file.path(dir, "source_info.dcf"), width = 10000)
  download_two_products(out_dir, time = c("2025-07-30", "2025-08-03"))
  info2 <- read.dcf(file.path(dir, "source_info.dcf"))
  expect_equal(unname(info2[, "First_downloaded"]), "2026-01-05")
  expect_equal(unname(info2[, "Last_downloaded"]), format(Sys.Date()))
  expect_equal(unname(info2[, "Files"]), "5")
  expect_match(paste(capture.output(cmems_getreadme(out_dir)), collapse = "\n"),
               paste("Downloaded:      5 Jan 2026 to", copernicopia:::.fmt_date(Sys.Date())),
               fixed = TRUE)
})

test_that("an empty folder gives a helpful message", {
  expect_message(res <- cmems_getreadme(withr::local_tempdir()), "No downloaded products found")
  expect_length(res, 0)
  expect_error(cmems_getreadme(data.frame(a = 1)), "no download request attached")
})

test_that("helpers format dates, DOIs and file coverage", {
  expect_equal(copernicopia:::.fmt_date(as.Date("2026-09-15")), "15 Sep 2026")
  expect_equal(copernicopia:::.clean_doi("https://doi.org/10.48670/moi-00021"), "10.48670/moi-00021")
  expect_equal(copernicopia:::.clean_doi("doi: 10.48670/moi-00021"), "10.48670/moi-00021")
  expect_equal(copernicopia:::.clean_doi(NA), "")
  cov <- copernicopia:::.files_coverage(
    c("ds_20250715.nc", "ds_202508.nc", "ds_20250901-20250910.nc", "ds_2024.nc"), "ds")
  expect_equal(cov, as.Date(c("2024-01-01", "2025-09-10")))
})

test_that("products are listed by title", {
  skip_if_no_python()
  use_fake_worker_toolbox()
  out_dir <- withr::local_tempdir()
  download_two_products(out_dir)
  ice <- cmems_request(list("synth_ice-south_P1D" = "ice_conc"), c(100, 160), c(-70, -30),
                       c("2025-07-30", "2025-07-30"), out_dir = out_dir,
                       catalogue = fake_catalogue())
  suppressMessages(cmems_download(ice))
  res <- capture.output(cmems_getreadme(out_dir))
  expect_equal(grep("^[0-9]\\. ", res, value = TRUE),
               c("1. Synthetic ocean colour", "2. Synthetic ocean physics",
                 "3. Synthetic sea ice south"))
})
