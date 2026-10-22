# Synthetic catalogue and NetCDF files, so the package can be tested without
# a Copernicus Marine account or network access.

fake_catalogue <- function() {
  row <- function(dataset_id, variable, standard_name, units, step, has_depth,
                  start = "2025-01-01T00:00:00", end = "2025-12-31T00:00:00",
                  title = "Synthetic product", product = "SYNTH", doi = "") {
    data.frame(
      product_id = product, product_title = title, product_doi = doi, processing_level = "L4",
      production_center = "TEST", dataset_id = dataset_id, dataset_name = dataset_id,
      version = "202501", part = "default", variable = variable,
      standard_name = standard_name, units = units, time_start = start,
      time_end = end, time_step_hours = step, lon_min = -180, lon_max = 180,
      lat_min = -80, lat_max = 90, lon_step = 0.5, lat_step = 0.5,
      has_depth = has_depth, depth_min = if (has_depth) 0.5 else NA_real_,
      depth_max = if (has_depth) 10 else NA_real_, n_depth = if (has_depth) 3 else NA_real_,
      stringsAsFactors = FALSE
    )
  }
  rbind(
    row("synth_phy_P1D-m", "thetao", "sea_water_potential_temperature", "degC", 24, TRUE,
        title = "Synthetic ocean physics", product = "SYNTH_PHY_001", doi = "10.0000/test-phy"),
    row("synth_phy_P1D-m", "zos", "sea_surface_height_above_geoid", "m", 24, TRUE,
        title = "Synthetic ocean physics", product = "SYNTH_PHY_001", doi = "10.0000/test-phy"),
    row("synth_chl_P1M", "CHL", "mass_concentration_of_chlorophyll_a_in_sea_water",
        "mg m-3", 24 * 30.4, FALSE, title = "Synthetic ocean colour",
        product = "SYNTH_OC_002", doi = "https://doi.org/10.0000/test-oc"),
    row("synth_ice-north_P1D", "ice_conc", "sea_ice_area_fraction", "%", 24, FALSE,
        title = "Synthetic sea ice north"),
    row("synth_ice-south_P1D", "ice_conc", "sea_ice_area_fraction", "%", 24, FALSE,
        title = "Synthetic sea ice south"),
    row("synth_old_P1D", "analysed_sst", "sea_surface_foundation_temperature", "K", 24, FALSE,
        start = "1990-01-01T00:00:00", end = "2000-12-31T00:00:00", title = "Old SST")
  )
}

python_ready <- function() {
  reticulate::py_module_available("copernicusmarine") &&
    reticulate::py_module_available("h5netcdf")
}

skip_if_no_python <- function() {
  if (!python_ready()) skip("Python with copernicusmarine and h5netcdf not available")
}

# Writes a grid where each value is (days since 2025-01-01) + lat / 1000 for
# daily data, or month number + lat / 1000 for monthly data. 3D variables add
# 100 * depth index.
write_synthetic <- function(path, variables, start, end, monthly = FALSE, depth = FALSE) {
  reticulate::py_run_string("
import numpy as np, pandas as pd, xarray as xr
def _write_synthetic(path, variables, start, end, monthly, depth):
    lat = np.arange(-70, -29.9, 0.5); lon = np.arange(100, 160.01, 0.5)
    if monthly:
        t = pd.date_range(start, end, freq='MS')
        base = (t.month).values.astype(float)
    else:
        t = pd.date_range(start, end, freq='D') + pd.Timedelta(hours=12)
        base = (t - pd.Timestamp('2025-01-01')).days.values.astype(float)
    data_vars = {}
    for v in variables:
        if depth and v == 'thetao':
            d = np.array([0.5, 5.0, 10.0])
            arr = base[:, None, None, None] + np.arange(3)[None, :, None, None] * 100 \\
                + lat[None, None, :, None] / 1000 + 0 * lon[None, None, None, :]
            data_vars[v] = (('time', 'depth', 'latitude', 'longitude'), arr)
        else:
            arr = base[:, None, None] + lat[None, :, None] / 1000 + 0 * lon[None, None, :]
            data_vars[v] = (('time', 'latitude', 'longitude'), arr)
    coords = dict(time=t, latitude=lat, longitude=lon)
    if depth:
        coords['depth'] = np.array([0.5, 5.0, 10.0])
    xr.Dataset(data_vars, coords=coords).to_netcdf(path, engine='h5netcdf')
")
  reticulate::py$`_write_synthetic`(path, as.list(variables), start, end, monthly, depth)
}

# Expected value for a daily variable at a latitude (snapped to the 0.5 grid).
expected_daily <- function(date, lat, depth_index = 0) {
  as.numeric(as.Date(date) - as.Date("2025-01-01")) + depth_index * 100 +
    round(lat * 2) / 2 / 1000
}

script_input <- function(answers) {
  i <- 0
  function(prompt) {
    i <<- i + 1
    if (i > length(answers)) stop("Ran out of scripted answers at prompt: ", prompt)
    answers[[i]]
  }
}

# Keep the tests' control folders and caches out of the user's real cache
Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "user-cache"))

# A stand-in for copernicusmarine.subset(), run by the real worker process.
# Behaviour is set with environment variables (the worker inherits them):
#   FAKE_DELAY    seconds per write step (3 steps per file)
#   FAKE_FAIL_ON  raise an error for files whose name contains this text
#   FAKE_STOP_ON  for this file, create a stop file in every control folder
#                 (as cmems_abort() would) and then wait
#   FAKE_HANG_ON  for this file, block forever without releasing Python's lock
#   FAKE_RECORD   folder to save each call's arguments in, as JSON
use_fake_worker_toolbox <- function(env = parent.frame()) {
  dir <- file.path(tempdir(), "fake-toolbox")
  dir.create(dir, showWarnings = FALSE)
  writeLines(c(
    "import glob, json, os, time",
    "def _match(var, name):",
    "    v = os.environ.get(var, '')",
    "    return bool(v) and v in name",
    "def subset(**kw):",
    "    name = kw['output_filename']",
    "    final = os.path.join(kw['output_directory'], name)",
    "    rec = os.environ.get('FAKE_RECORD')",
    "    if rec:",
    "        with open(os.path.join(rec, name.split('_', 2)[2] + '.json'), 'w') as f:",
    "            json.dump(kw, f)",
    "    if _match('FAKE_FAIL_ON', name):",
    "        raise ValueError('server unavailable')",
    "    if _match('FAKE_STOP_ON', name):",
    "        root = os.path.join(os.environ['R_USER_CACHE_DIR'], 'R', 'copernicopia', 'running')",
    "        for d in glob.glob(os.path.join(root, '*')):",
    "            open(os.path.join(d, 'stop'), 'w').close()",
    "        time.sleep(30)",
    "    if _match('FAKE_HANG_ON', name):",
    "        import re",
    "        open(final + '.part', 'w').close()",
    "        re.match(r'(a+)+$', 'a' * 60 + 'b')  # runs for ages without releasing Python's lock",
    "    delay = float(os.environ.get('FAKE_DELAY', '0'))",
    "    with open(final + '.ab12', 'wb') as f:",
    "        for _ in range(3):",
    "            f.write(b'0' * 100000)",
    "            f.flush()",
    "            time.sleep(delay)",
    "    os.replace(final + '.ab12', final)"
  ), file.path(dir, "fake_toolbox.py"))
  withr::local_options(copernicopia.subset_function = "fake_toolbox:subset",
                       copernicopia.python_path = dir, .local_envir = env)
  withr::local_envvar(FAKE_DELAY = NA, FAKE_FAIL_ON = NA, FAKE_STOP_ON = NA,
                      FAKE_HANG_ON = NA, FAKE_RECORD = NA, .local_envir = env)
  invisible(dir)
}

worker_alive <- function(pid) {
  out <- suppressWarnings(system2("ps", c("-p", pid, "-o", "args="), stdout = TRUE, stderr = FALSE))
  any(grepl("copernicopia_worker", out, fixed = TRUE))
}
