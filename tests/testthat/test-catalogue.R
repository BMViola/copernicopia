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
