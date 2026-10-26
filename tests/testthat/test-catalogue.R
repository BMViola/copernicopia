test_that("a copernicusmarine catalogue converts to the R table", {
  skip_if_no_python()
  reticulate::py_run_string("
import copernicusmarine as cm
def _coord(cid, unit='', lo=None, hi=None, step=None, values=None):
    return cm.CopernicusMarineCoordinate(coordinate_id=cid, coordinate_unit=unit,
        minimum_value=lo, maximum_value=hi, step=step, values=values, chunking_length=None,
        chunk_type=None, chunk_reference_coordinate=None, chunk_geometric_factor=None, axis=None)
def _var(name, std, coords, units):
    return cm.CopernicusMarineVariable(short_name=name, standard_name=std, units=units,
        bbox=None, coordinates=coords)
def _dataset(did, part_name, service_name, variables):
    service = cm.CopernicusMarineService(service_name=service_name, service_short_name=None,
        service_format=None, uri='x', variables=variables, platforms_metadata=None,
        arco_sparse_type=None)
    part = cm.CopernicusMarinePart(name=part_name, services=[service], retired_date=None,
        released_date=None, arco_updating_start_date=None, arco_updated_date=None, url_metadata='x')
    return cm.CopernicusMarineDataset(dataset_id=did, dataset_name=did, product_id='P',
        digital_object_identifier=None,
        versions=[cm.CopernicusMarineVersion(label='202406', parts=[part])])
ms = 'milliseconds since 1970-01-01 00:00:00Z'
daily = [_coord('time', ms, 1751328000000, 1759190400000, 86400000),
         _coord('latitude', 'degrees_north', -80, 90, 0.083333),
         _coord('longitude', 'degrees_east', -180, 179.916, 0.083333),
         _coord('depth', 'm', values=[0.494, 1.54, 2.65])]
monthly = [_coord('time', ms, values=['2025-07-01T00:00:00Z', '2025-08-01T00:00:00Z']),
           _coord('latitude', 'degrees_north', -89.98, 89.98, 0.0417),
           _coord('longitude', 'degrees_east', -179.98, 179.98, 0.0417)]
xy = [_coord('time', ms, 0, 864000000, 86400000), _coord('x', 'm', 0, 10, 1), _coord('y', 'm', 0, 10, 1)]
datasets = [
    _dataset('phy_P1D', 'default', 'arco-geo-series',
             [_var('thetao', 'sea_water_potential_temperature', daily, 'degC'),
              _var('zos', 'sea_surface_height_above_geoid', daily[:3], 'm')]),
    _dataset('chl_P1M', 'default', 'arco-geo-series', [_var('CHL', None, monthly, 'mg m-3')]),
    _dataset('ice_xy', 'originalGrid', 'arco-geo-series', [_var('ice_conc', 'sea_ice_area_fraction', xy, '%')]),
    _dataset('insitu', 'default', 'arco-platform-series', [_var('TEMP', 'sea_water_temperature', daily, 'degC')]),
]
_test_catalogue = cm.CopernicusMarineCatalogue(products=[cm.CopernicusMarineProduct(
    title='Test product', product_id='TEST', thumbnail_url='', description=None,
    digital_object_identifier=None, sources=[], processing_level=None, production_center='X',
    keywords=None, datasets=datasets, product_user_manual=None,
    quality_information_document=None, synthesis_quality_overview=None)])
")
  h <- copernicopia:::.helpers()
  cat_df <- copernicopia:::.columns_to_df(h$to_columns(h$flatten_catalogue(reticulate::py$`_test_catalogue`)))
  expect_equal(cat_df$dataset_id, c("phy_P1D", "phy_P1D", "chl_P1M"))
  expect_equal(cat_df$variable, c("thetao", "zos", "CHL"))
  expect_equal(cat_df$standard_name[3], "")
  expect_equal(cat_df$processing_level, c("", "", ""))
  expect_equal(cat_df$time_step_hours[1], 24)
  expect_equal(cat_df$time_step_hours[3], 31 * 24)
  expect_equal(cat_df$has_depth, c(TRUE, FALSE, FALSE))
  expect_equal(cat_df$n_depth, c(3, NA, NA))
  expect_true(is.na(cat_df$depth_min[2]))
  expect_equal(cat_df$time_start[1], "2025-07-01T00:00:00")
  expect_type(cat_df$lon_step, "double")
})

test_that("the variable shortlist matches catalogue standard names", {
  g <- cmems_variables()
  expect_true(all(c("label", "pattern") %in% names(g)))
  hit <- function(label, std) grepl(g$pattern[g$label == label], std, ignore.case = TRUE)
  expect_true(hit("Sea surface temperature", "sea_surface_foundation_temperature"))
  expect_false(hit("Sea surface temperature", "sea_water_potential_temperature"))
  expect_true(hit("Sea surface height / sea level", "sea_surface_height_above_sea_level"))
  expect_true(hit("Ocean currents", "surface_geostrophic_eastward_sea_water_velocity"))
  expect_true(hit("Sea ice concentration", "sea_ice_area_fraction"))
  expect_true(hit("Chlorophyll-a", "mass_concentration_of_chlorophyll_a_in_sea_water"))
})

test_that("time units with extra notes or time zones are read", {
  skip_if_no_python()
  h <- copernicopia:::.helpers()
  origin <- function(u) h$`_unit_factor_and_origin`(u)
  expect_equal(origin("seconds since 1970-01-01 00:00:00Z (no leap seconds)"), list(1000, 0))
  expect_equal(origin("milliseconds since 1970-01-01 00:00:00Z"), list(1, 0))
  expect_equal(origin("days since 1950-01-01")[[2]], -631152000000)
  expect_equal(origin("hours since 1900-01-01T00:00:00.000")[[2]], -2208988800000)
  expect_equal(origin("minutes since 0001-01-01 00:00:00")[[2]], -62135596800000)
  ms <- function(x) h$`_datetime_to_ms`(x)
  expect_equal(ms("2025-07-01T00:00:00Z"), 1751328000000)
  expect_equal(ms("2025-07-01T10:00:00+10:00"), 1751328000000)
  expect_equal(ms("2025-07-01 00:30:00 -0030"), 1751328000000 + 3600000)
  res <- reticulate::py_run_string(paste(
    "import copernicopia_helpers as _h",
    "try:",
    "    _h._datetime_to_ms('not a date'); _raised = False",
    "except ValueError:",
    "    _raised = True", sep = "\n"))
  expect_true(res$`_raised`)
})

# A stand-in for copernicusmarine.describe(): one readable dataset, one whose
# time unit carries a note, and one whose time unit cannot be read.
write_fake_describe <- function(dir) {
  writeLines(c(
    "import copernicusmarine as cm",
    "def _coord(cid, unit='', lo=None, hi=None, step=None, values=None):",
    "    return cm.CopernicusMarineCoordinate(coordinate_id=cid, coordinate_unit=unit,",
    "        minimum_value=lo, maximum_value=hi, step=step, values=values, chunking_length=None,",
    "        chunk_type=None, chunk_reference_coordinate=None, chunk_geometric_factor=None, axis=None)",
    "def _dataset(did, time_unit, t0, t1, step):",
    "    coords = [_coord('time', time_unit, t0, t1, step),",
    "              _coord('latitude', 'degrees_north', -80, 90, 0.25),",
    "              _coord('longitude', 'degrees_east', -180, 179.75, 0.25)]",
    "    var = cm.CopernicusMarineVariable(short_name='v', standard_name='sea_surface_temperature',",
    "        units='K', bbox=None, coordinates=coords)",
    "    service = cm.CopernicusMarineService(service_name='arco-geo-series', service_short_name=None,",
    "        service_format=None, uri='x', variables=[var], platforms_metadata=None, arco_sparse_type=None)",
    "    part = cm.CopernicusMarinePart(name='default', services=[service], retired_date=None,",
    "        released_date=None, arco_updating_start_date=None, arco_updated_date=None, url_metadata='x')",
    "    return cm.CopernicusMarineDataset(dataset_id=did, dataset_name=did, product_id='P',",
    "        digital_object_identifier=None,",
    "        versions=[cm.CopernicusMarineVersion(label='202406', parts=[part])])",
    "def describe(disable_progress_bar=False):",
    "    assert disable_progress_bar",
    "    datasets = [",
    "        _dataset('ms_P1D', 'milliseconds since 1970-01-01 00:00:00Z', 1751328000000, 1759190400000, 86400000),",
    "        _dataset('s_P1D', 'seconds since 1970-01-01 00:00:00Z (no leap seconds)', 1751328000, 1759190400, 86400),",
    "        _dataset('odd_P1D', 'seconds since the start of the mission', 0, 864000, 86400)]",
    "    return cm.CopernicusMarineCatalogue(products=[cm.CopernicusMarineProduct(",
    "        title='Test product', product_id='TEST', thumbnail_url='', description=None,",
    "        digital_object_identifier='10.0000/test', sources=[], processing_level=None,",
    "        production_center='X', keywords=None, datasets=datasets, product_user_manual=None,",
    "        quality_information_document=None, synthesis_quality_overview=None)])"
  ), file.path(dir, "fake_describe.py"))
}

test_that("the catalogue is fetched in a separate process, leaving out unreadable datasets", {
  skip_if_no_python()
  dir <- withr::local_tempdir()
  write_fake_describe(dir)
  withr::local_options(copernicopia.describe_function = "fake_describe:describe",
                       copernicopia.python_path = dir,
                       copernicopia.live_progress = FALSE)
  old <- copernicopia:::.cmems$catalogue
  withr::defer(assign("catalogue", old, envir = copernicopia:::.cmems))

  msgs <- character(0)
  cat_df <- withCallingHandlers(cmems_catalogue(refresh = TRUE),
                                message = function(m) {
                                  msgs <<- c(msgs, conditionMessage(m))
                                  invokeRestart("muffleMessage")
                                })
  expect_equal(cat_df$dataset_id, c("ms_P1D", "s_P1D"))
  expect_equal(cat_df$time_start, rep("2025-07-01T00:00:00", 2))
  expect_equal(cat_df$time_step_hours, c(24, 24))
  expect_equal(cat_df$product_doi, rep("10.0000/test", 2))
  expect_true(any(grepl("1 dataset was left out .*odd_P1D", msgs)))
  expect_true(any(grepl("^done \\(", msgs)))
  # Saved for next time
  expect_true(file.exists(file.path(tools::R_user_dir("copernicopia", "cache"), "catalogue.rds")))
})

test_that("a failed catalogue fetch reports the reason", {
  skip_if_no_python()
  dir <- withr::local_tempdir()
  writeLines(c("def describe(disable_progress_bar=False):",
               "    raise RuntimeError('the catalogue server is down')"),
             file.path(dir, "broken_describe.py"))
  withr::local_options(copernicopia.describe_function = "broken_describe:describe",
                       copernicopia.python_path = dir,
                       copernicopia.live_progress = FALSE)
  expect_error(suppressMessages(copernicopia:::.fetch_catalogue()),
               "Could not fetch the Copernicus Marine catalogue: .*the catalogue server is down")
})

test_that("live lines stay within the console width", {
  fit <- copernicopia:::.fit_line
  expect_equal(fit("[1/3] a.nc ... ", "downloading | 5s", width = 80),
               "[1/3] a.nc ... downloading | 5s")
  prefix <- "[1/1] METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2_202601.nc ... "
  text <- "downloading | 1m 05s, 120.3 MB so far"
  line <- fit(prefix, text, width = 70)
  expect_lte(nchar(line), 69)
  expect_true(endsWith(line, text))
  expect_true(startsWith(line, "[1/1] METOFF"))
  expect_true(grepl("...", line, fixed = TRUE))
  expect_lte(nchar(fit(prefix, strrep("x", 100), width = 60)), 59)
})
