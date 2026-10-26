# Real CF standard names, and the abbreviations that should find them
examples <- list(
  chla = "mass_concentration_of_chlorophyll_a_in_sea_water",
  sst = c("sea_surface_temperature", "sea_surface_foundation_temperature",
          "sea_surface_subskin_temperature"),
  sbt = "sea_water_potential_temperature_at_sea_floor",
  sss = c("sea_surface_salinity", "sea_water_salinity"),
  ssh = c("sea_surface_height_above_geoid", "sea_surface_height_above_sea_level"),
  adt = "sea_surface_height_above_geoid",
  sla = "sea_surface_height_above_sea_level",
  mld = "ocean_mixed_layer_thickness_defined_by_sigma_theta",
  currents = c("eastward_sea_water_velocity", "surface_geostrophic_northward_sea_water_velocity"),
  do = "mole_concentration_of_dissolved_molecular_oxygen_in_sea_water",
  ph = "sea_water_ph_reported_on_total_scale",
  co2 = c("surface_partial_pressure_of_carbon_dioxide_in_sea_water",
          "surface_downward_mass_flux_of_carbon_dioxide_expressed_as_carbon"),
  pco2 = "surface_partial_pressure_of_carbon_dioxide_in_sea_water",
  dic = "mole_concentration_of_dissolved_inorganic_carbon_in_sea_water",
  talk = "sea_water_alkalinity_expressed_as_mole_equivalent",
  no3 = "mole_concentration_of_nitrate_in_sea_water",
  po4 = "mole_concentration_of_phosphate_in_sea_water",
  si = "mole_concentration_of_silicate_in_sea_water",
  fe = "mole_concentration_of_dissolved_iron_in_sea_water",
  nh4 = "mole_concentration_of_ammonium_in_sea_water",
  nutrients = c("mole_concentration_of_nitrate_in_sea_water",
                "mole_concentration_of_dissolved_iron_in_sea_water"),
  npp = "net_primary_production_of_biomass_expressed_as_carbon_per_unit_volume_in_sea_water",
  kd490 = "volume_attenuation_coefficient_of_downwelling_radiative_flux_in_sea_water",
  par = "downwelling_photosynthetic_photon_flux_in_sea_water",
  poc = "mole_concentration_of_particulate_organic_matter_expressed_as_carbon_in_sea_water",
  spm = "mass_concentration_of_suspended_matter_in_sea_water",
  rrs = "surface_ratio_of_upwelling_radiance_emerging_from_sea_water_to_downwelling_radiative_flux_in_air",
  swh = "sea_surface_wave_significant_height",
  tau = c("surface_downward_eastward_stress", "surface_downward_northward_stress"),
  u10 = "eastward_wind",
  sic = "sea_ice_area_fraction",
  sit = "sea_ice_thickness",
  qnet = "surface_downward_heat_flux_in_sea_water"
)

test_that("every abbreviation finds the right standard names", {
  for (term in names(examples)) {
    row <- copernicopia:::.find_search_term(term)
    expect_false(is.null(row), info = term)
    expect_true(all(grepl(row$pattern, examples[[term]], ignore.case = TRUE)), info = term)
  }
})

test_that("abbreviations do not pick up unrelated variables", {
  others <- unlist(examples)
  only <- function(term, allowed) {
    row <- copernicopia:::.find_search_term(term)
    found <- unique(others[grepl(row$pattern, others, ignore.case = TRUE)])
    expect_setequal(found, allowed)
  }
  # "ph" must not match phosphate, phytoplankton or photosynthetic
  only("ph", "sea_water_ph_reported_on_total_scale")
  only("sst", examples$sst)
  only("adt", "sea_surface_height_above_geoid")
  only("pco2", "surface_partial_pressure_of_carbon_dioxide_in_sea_water")
  only("do", "mole_concentration_of_dissolved_molecular_oxygen_in_sea_water")
  only("si", "mole_concentration_of_silicate_in_sea_water")
})

test_that("the table is well formed", {
  st <- copernicopia:::.search_terms()
  # No term may belong to two rows (spelling variants within a row are fine)
  terms <- unlist(lapply(strsplit(st$terms, ","), function(t) unique(copernicopia:::.normalise_term(t))))
  expect_false(any(duplicated(terms)))
  for (p in st$pattern) expect_silent(grepl(p, "x"))
})

test_that("spelling variants of an abbreviation all work", {
  for (v in c("chla", "Chl-a", "CHL A", "chl_a", " chla ")) {
    expect_equal(copernicopia:::.find_search_term(v)$finds, "Chlorophyll-a", info = v)
  }
  expect_equal(copernicopia:::.find_search_term("Wind Stress")$finds, "Wind stress")
  expect_null(copernicopia:::.find_search_term("chlorophyll"))
})

test_that("the wizard's search uses abbreviations, exact short names and plain text", {
  cat_df <- fake_catalogue()
  bgc <- cat_df[1, ]
  bgc$dataset_id <- "synth_bgc_P1D"
  bgc$variable <- "o2"
  bgc$standard_name <- ""
  cat_df <- rbind(cat_df, bgc)
  search <- function(keyword) {
    withr::local_options(copernicopia.input = function(prompt) "")
    out <- capture.output(res <- copernicopia:::.pick_standard_names(cat_df, keyword))
    out
  }
  out <- search("chla")
  expect_match(out[1], 'Searching for Chlorophyll-a ("chla").', fixed = TRUE)
  expect_match(out[2], "mass_concentration_of_chlorophyll_a_in_sea_water (CHL; 1 product)",
               fixed = TRUE)
  expect_match(search("SST")[2], "sea_surface_foundation_temperature (analysed_sst; 1 product)",
               fixed = TRUE)
  # A variable with no standard name is still found by its exact short name
  expect_match(paste(search("o2"), collapse = "\n"), "\\[1\\] o2 \\(o2; 1 product\\)")
  # Not in this catalogue: say so rather than match phosphate etc.
  expect_match(search("ph")[2], "No variables match that search.", fixed = TRUE)
  # Ordinary text search is unchanged
  expect_match(paste(search("thetao"), collapse = "\n"), "sea_water_potential_temperature")
  expect_match(paste(search("ice"), collapse = "\n"), "sea_ice_area_fraction")
})

test_that("every shortlist entry can be found by its full name", {
  groups <- cmems_variables()
  for (i in seq_len(nrow(groups))) {
    row <- copernicopia:::.find_search_term(groups$label[i])
    expect_equal(row$finds, groups$label[i], info = groups$label[i])
    expect_equal(row$pattern, groups$pattern[i], info = groups$label[i])
  }
})

test_that("shorter forms of shortlist names work too", {
  finds <- function(k) copernicopia:::.find_search_term(k)$finds
  expect_equal(finds("mixed layer depth"), "Mixed layer depth")
  expect_equal(finds("DISSOLVED OXYGEN"), "Dissolved oxygen")
  expect_equal(finds("sea level"), "Sea surface height / sea level")
  expect_equal(finds("sea surface height"), "Sea surface height / sea level")
  expect_equal(finds("sea water temperature"), "Sea water temperature (with depth)")
  expect_equal(finds("nutrients"), "Nutrients (nitrate, phosphate, silicate, iron, ammonium)")
  expect_equal(finds("carbonate system"), "Carbonate system (pH, CO2, alkalinity)")
  expect_equal(finds("photosynthetically available radiation"),
               "Photosynthetically available radiation (PAR)")
  expect_equal(finds("POC"), "Particulate organic carbon (POC)")
  # Full names of abbreviations that are not shortlist entries
  expect_equal(finds("sea level anomaly"), "Sea level anomaly")
  expect_equal(finds("Absolute dynamic topography"), "Absolute dynamic topography")
  expect_equal(finds("significant wave height"), "Significant wave height")
  # Single generic words are not names: they fall back to plain text search
  expect_null(copernicopia:::.find_search_term("type"))
  expect_null(copernicopia:::.find_search_term("with depth"))
  expect_null(copernicopia:::.find_search_term(""))
})

test_that("new shortlist entries and ammonium are matched", {
  g <- cmems_variables()
  expect_equal(nrow(g), 24)
  hit <- function(label, std) grepl(g$pattern[g$label == label], std, ignore.case = TRUE)
  expect_true(hit("Nutrients (nitrate, phosphate, silicate, iron, ammonium)",
                  "mole_concentration_of_ammonium_in_sea_water"))
  expect_true(hit("Bottom temperature", "sea_water_potential_temperature_at_sea_floor"))
  expect_false(hit("Sea water temperature (with depth)", "sea_water_potential_temperature_at_sea_floor"))
  expect_true(hit("Photosynthetically available radiation (PAR)",
                  "downwelling_photosynthetic_photon_flux_in_sea_water"))
  expect_true(hit("Particulate organic carbon (POC)",
                  "mole_concentration_of_particulate_organic_matter_expressed_as_carbon_in_sea_water"))
  expect_true(hit("Surface heat flux", "surface_downward_heat_flux_in_sea_water"))
  expect_true(hit("Wind and wind stress", "surface_downward_eastward_stress"))
})

test_that("searching by a shortlist name lists that entry's variables", {
  withr::local_options(copernicopia.input = function(prompt) "")
  out <- capture.output(copernicopia:::.pick_standard_names(fake_catalogue(), "Sea water temperature"))
  expect_equal(out[1], "Searching for Sea water temperature (with depth) (\"Sea water temperature\").")
  expect_match(out[2], "sea_water_potential_temperature (thetao; 1 product)", fixed = TRUE)
  out <- capture.output(copernicopia:::.pick_standard_names(fake_catalogue(), "Sea ice concentration"))
  expect_equal(out[1], "Searching for Sea ice concentration.")
  expect_match(out[2], "sea_ice_area_fraction (ice_conc; 2 products)", fixed = TRUE)
})

test_that("a variable name filed under several standard names can be picked as one", {
  cat_df <- fake_catalogue()
  err <- cat_df[cat_df$dataset_id == "synth_old_P1D", ][c(1, 1), ]
  err$dataset_id <- c("ostia_like_P1D", "regional_P1D")
  err$variable <- "analysis_error"
  err$standard_name <- c("sea_surface_foundation_temperature standard_error", "")
  cat_df <- rbind(cat_df, err)
  withr::local_options(copernicopia.input = function(prompt) "1")
  out <- capture.output(res <- copernicopia:::.pick_standard_names(cat_df, "analysis_error"))
  expect_match(out[1],
               "[1] All variables named analysis_error, whatever their standard name (2 products)",
               fixed = TRUE)
  expect_match(out[2], "[2] analysis_error (analysis_error; 1 product)", fixed = TRUE)
  expect_match(out[3], "[3] sea_surface_foundation_temperature standard_error (analysis_error; 1 product)",
               fixed = TRUE)
  expect_equal(res[[1]]$label, "analysis_error (any standard name)")
  expect_setequal(cat_df$dataset_id[res[[1]]$rows], c("ostia_like_P1D", "regional_P1D"))
  # A name under only one standard name needs no extra choice
  out2 <- capture.output(copernicopia:::.pick_standard_names(cat_df, "thetao"))
  expect_false(any(grepl("All variables named", out2)))
})
