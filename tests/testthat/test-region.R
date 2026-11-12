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

test_that("regions reaching past a dataset's outermost grid points are cut back to them", {
  cat_df <- fake_catalogue()
  cat_df$lon_min <- -179.97500610351562
  cat_df$lon_max <- 179.97500610351562
  cat_df$lat_min <- -89.97500610351562
  cat_df$lat_max <- 89.97500610351562
  cat_df$lon_step <- cat_df$lat_step <- 0.05
  make <- function(lon, lat, catalogue = cat_df) {
    r <- cmems_request(list("synth_old_P1D" = "analysed_sst"), lon, lat,
                       c("1995-01-01", "1995-01-31"), out_dir = tempdir(), catalogue = catalogue)
    copernicopia:::.padded_bounds(r, r$datasets[[1]])
  }
  # All longitudes: the grid's own edges, so the toolbox has nothing to warn about
  b <- make(c(-180, 180), c(-80, -15))
  expect_equal(b$lon, c(-179.97500610351562, 179.97500610351562))
  expect_equal(b$lat, c(-80.05, -14.95))
  # Latitudes past the poles of the grid
  expect_equal(make(c(100, 160), c(-90, 90))$lat, c(-89.97500610351562, 89.97500610351562))
  # Partial and crossing regions keep their padding
  expect_equal(make(c(100, 160), c(-70, -30))$lon, c(99.95, 160.05))
  expect_equal(make(c(170, -170), c(-70, -30))$lon, c(169.95, 190.05))
  # Grids written from 0 to 360 are left to the toolbox
  cat360 <- cat_df
  cat360$lon_min <- 0.025
  cat360$lon_max <- 359.975
  expect_equal(make(c(-180, 180), c(-80, -15), cat360)$lon, c(-180, 180))
  # Requests made before the edges were stored are padded as before
  r <- cmems_request(list("synth_old_P1D" = "analysed_sst"), c(-180, 180), c(-90, 90),
                     c("1995-01-01", "1995-01-31"), out_dir = tempdir(), catalogue = cat_df)
  old <- r$datasets[[1]]
  old[c("lon_min", "lon_max", "lat_min", "lat_max")] <- NULL
  b_old <- copernicopia:::.padded_bounds(r, old)
  expect_equal(b_old$lon, c(-180, 180))
  expect_equal(b_old$lat, c(-90, 90))
})

test_that("a request covering all longitudes prints plainly", {
  r <- cmems_request(list("synth_old_P1D" = "analysed_sst"), c(-180, 180), c(-80, -15),
                     c("1995-01-01", "1995-01-31"), out_dir = tempdir(),
                     catalogue = fake_catalogue())
  out <- capture.output(print(r))
  expect_true(any(grepl("Region: all longitudes; latitude -80 to -15", out, fixed = TRUE)))
})

test_that("tracks crossing 180 or 0 get the same values however their times are written", {
  skip_if_no_python()
  cat_df <- fake_catalogue()
  cat_df$time_step_hours[cat_df$dataset_id == "synth_old_P1D"] <- 1   # an hourly product
  # Value = 1000 x hours since 15 July 00:00 UTC + the longitude of the grid cell
  reticulate::py_run_string("
import numpy as np, pandas as pd, xarray as xr
def _track_file(path, west, east):
    # As the toolbox writes a region crossing 180: longitudes run on past 180
    lon = np.arange(west, east + 0.01, 0.5); lat = np.arange(-70, -29.9, 0.5)
    t = pd.date_range('2025-07-14 23:00', '2025-07-17 01:00', freq='h')
    hours = ((t - pd.Timestamp('2025-07-15')) / pd.Timedelta(hours=1)).values.astype(float)
    cell = np.mod(lon + 180, 360) - 180
    v = hours[:, None, None] * 1000 + cell[None, None, :] + 0 * lat[None, :, None]
    xr.Dataset({'analysed_sst': (('time', 'latitude', 'longitude'), v)},
               coords=dict(time=t, latitude=lat, longitude=lon)).to_netcdf(path, engine='h5netcdf')
")
  for (line in c(180, 0)) {
    req <- cmems_request(list("synth_old_P1D" = "analysed_sst"), c(line - 10, line + 10), c(-70, -30),
                         c("2025-07-15", "2025-07-16"), out_dir = withr::local_tempdir(),
                         catalogue = cat_df)
    dir <- copernicopia:::.dataset_dir(req, req$datasets[[1]])
    dir.create(dir, recursive = TRUE)
    reticulate::py$`_track_file`(file.path(dir, "synth_old_P1D_20250715-20250716.nc"),
                                 line - 10.5, line + 10.5)

    # 48 hourly fixes swinging back and forth across the line
    h <- 0:47
    lon <- copernicopia:::.lon_mod(line + 6 * sin(2 * pi * h / 16) + 0.13)
    t <- as.POSIXct("2025-07-15 00:00", tz = "UTC") + 3600 * h
    expect_gte(sum(diff(sign(lon)) != 0), 5)
    cell <- round(lon * 2) / 2
    cell[cell == 180] <- -180
    expected <- h * 1000 + cell

    # The same instants written six ways. Local times with a UTC offset jump
    # by almost a day where the track crosses 180 (+11:5x on one side, -11:5x
    # on the other), yet are the same instants
    local <- t
    attr(local, "tzone") <- "Pacific/Auckland"
    csv <- withr::local_tempfile(fileext = ".csv")
    write.csv(data.frame(t = t), csv, row.names = FALSE)
    off <- round(lon * 4)
    with_offset <- paste0(format(t + off * 60, "%Y-%m-%dT%H:%M:%S"),
                          sprintf("%s%02d:%02d", ifelse(off < 0, "-", "+"), abs(off) %/% 60, abs(off) %% 60))
    forms <- list(utc = t, local_zone = local, text = format(t, "%Y-%m-%d %H:%M:%S"),
                  csv = read.csv(csv)$t, iso_z = format(t, "%Y-%m-%dT%H:%M:%SZ"),
                  utc_offset = with_offset)
    expect_true(any(nchar(forms$csv) == 10))          # midnights written without a time
    if (line == 180) expect_true(all(c("+", "-") %in% substr(with_offset, 20, 20)))
    for (form in names(forms)) {
      obs <- data.frame(lon = lon, lat = -50)
      obs$when <- forms[[form]]
      res <- suppressMessages(cmems_extract(obs, req, time = "when"))
      expect_equal(res$analysed_sst, expected, info = paste("line", line, "times as", form))
    }
  }
})

# A stand-in for copernicusmarine.subset() that cuts the region out of a
# global 0.5-degree grid the way the toolbox does with
# coordinates_selection_method = "inside": only cell centres within the bounds,
# longitudes taken modulo 360 and shifted across 180 when the region crosses
# it, and the nearest single row or column when no centre falls inside. Each
# cell holds its own centre, so the tests can see which cell a point got.
use_edge_toolbox <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  writeLines(c(
    "import os",
    "import numpy as np, pandas as pd, xarray as xr",
    "def _lon_mod(x):",
    "    return ((x + 180.0) % 360.0) - 180.0",
    "def _inside(ds, name, lo, hi):",
    "    out = ds.sel({name: slice(lo, hi)})",
    "    if out[name].size == 0:",
    "        near = ds[name].sel({name: lo}, method='nearest').values",
    "        out = ds.sel({name: slice(near, near)})",
    "    return out",
    "def subset(**kw):",
    "    lat = np.arange(-79.75, 89.76, 0.5)",
    "    lon = np.arange(-179.75, 179.76, 0.5)",
    "    t = pd.date_range(kw['start_datetime'][:10], kw['end_datetime'][:10], freq='D')",
    "    shape = (t.size, lat.size, lon.size)",
    "    ds = xr.Dataset(",
    "        {'cell_lon': (('time', 'latitude', 'longitude'), np.broadcast_to(lon, shape).copy()),",
    "         'cell_lat': (('time', 'latitude', 'longitude'), np.broadcast_to(lat[:, None], shape).copy())},",
    "        coords=dict(time=t, latitude=lat, longitude=lon))",
    "    ds = _inside(ds, 'latitude', kw['minimum_latitude'], kw['maximum_latitude'])",
    "    x0, x1 = kw['minimum_longitude'], kw['maximum_longitude']",
    "    if x1 - x0 < 360:",
    "        x0, x1 = _lon_mod(x0), _lon_mod(x1)",
    "        if x1 < x0:",
    "            x1 += 360",
    "            ds = ds.assign_coords(longitude=((ds.longitude - x0) % 360) + x0).sortby('longitude')",
    "        ds = _inside(ds, 'longitude', x0, x1)",
    "    ds[list(kw['variables'])].to_netcdf(",
    "        os.path.join(kw['output_directory'], kw['output_filename']), engine='h5netcdf')"
  ), file.path(dir, "edge_toolbox.py"))
  withr::local_options(copernicopia.subset_function = "edge_toolbox:subset",
                       copernicopia.python_path = dir, .local_envir = env)
  invisible(dir)
}

edge_catalogue <- function() {
  cat_df <- fake_catalogue()
  cat_df <- cat_df[cat_df$dataset_id == "synth_old_P1D", ][c(1, 1), ]
  cat_df$dataset_id <- cat_df$dataset_name <- "synth_edge_P1D"
  cat_df$variable <- c("cell_lon", "cell_lat")
  cat_df$standard_name <- c("longitude", "latitude")
  cat_df$units <- "degrees"
  cat_df$time_start <- "2025-01-01T00:00:00"
  cat_df$time_end <- "2025-12-31T00:00:00"
  cat_df$lon_min <- -179.75
  cat_df$lon_max <- 179.75
  cat_df$lat_min <- -79.75
  cat_df$lat_max <- 89.75
  rownames(cat_df) <- NULL
  cat_df
}

# Downloads the region with the stand-in toolbox and returns the cell each
# point was matched to.
edge_cells <- function(lon, lat, points) {
  req <- suppressMessages(cmems_request(
    list("synth_edge_P1D" = c("cell_lon", "cell_lat")), lon, lat,
    c("2025-07-15", "2025-07-15"), out_dir = withr::local_tempdir(),
    catalogue = edge_catalogue()))
  res <- suppressMessages(cmems_download(req))
  expect_equal(res$result, "ok")
  points$date <- as.Date("2025-07-15")
  suppressMessages(cmems_extract(points, req))[, c("cell_lon", "cell_lat")]
}

test_that("points on the region's edges get the cell just beyond it", {
  skip_if_no_python()
  use_edge_toolbox()

  # Cell centres are at x.25 and x.75. Each point sits on an edge (or corner)
  # of the region, and its nearest cell centre lies just outside the region:
  # it is only there because one extra cell is downloaded on every side.
  # Without that cell these points would be more than half a cell from any
  # downloaded centre.
  pts <- data.frame(lon = c(99.9, 110.1, 105.1, 105.1, 99.9, 110.1),
                    lat = c(-45.1, -45.1, -50.1, -39.9, -50.1, -39.9))
  got <- edge_cells(c(99.9, 110.1), c(-50.1, -39.9), pts)
  expect_equal(got$cell_lon, c(99.75, 110.25, 105.25, 105.25, 99.75, 110.25))
  expect_equal(got$cell_lat, c(-45.25, -45.25, -50.25, -39.75, -50.25, -39.75))

  # The same across 180: the region is 179.9 to 180.1 (written -179.9)
  pts <- data.frame(lon = c(179.9, -179.9), lat = -45.1)
  got <- edge_cells(c(179.9, -179.9), c(-45.2, -45.1), pts)
  expect_equal(got$cell_lon, c(179.75, -179.75))
  expect_equal(got$cell_lat, c(-45.25, -45.25))

  # A region smaller than one cell (around a mooring, say), holding no cell
  # centre at all, still gets the cells around it
  got <- edge_cells(c(150.3, 150.35), c(-45.3, -45.27), data.frame(lon = 150.3, lat = -45.3))
  expect_equal(unlist(got), c(cell_lon = 150.25, cell_lat = -45.25))

  # All longitudes: trimmed to the grid's own edges, the points either side of
  # 180 still get the nearest cell, across the line where that is nearer
  pts <- data.frame(lon = c(179.9, -179.9, 179.99, -179.99), lat = -45.1)
  got <- edge_cells(c(-180, 180), c(-45.2, -45.1), pts)
  expect_equal(got$cell_lon, c(179.75, -179.75, 179.75, -179.75))
})

test_that("a region at the grid's edge, downloaded as a single row, matches only nearby points", {
  skip_if_no_python()
  use_edge_toolbox()
  # The last row of the grid is at 89.75. The region's padding is cut at the
  # grid's edge, so only that row is downloaded; a single row has no spacing
  # to measure a cell by, so the catalogue's step (0.5) is used. Points within
  # half a cell of the row get it; a point far south of it gets NA.
  pts <- data.frame(lon = 10.1, lat = c(89.9, 89.6, 80))
  got <- edge_cells(c(10.1, 10.4), c(89.8, 89.9), pts)
  expect_equal(got$cell_lat, c(89.75, 89.75, NA))
  expect_equal(got$cell_lon, c(10.25, 10.25, NA))
})
