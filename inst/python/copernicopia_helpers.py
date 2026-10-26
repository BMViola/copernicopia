"""
Python backend for the copernicopia R package.

The R side calls these functions through reticulate. They do the work that is
easier or faster in Python: flattening the Copernicus Marine catalogue into a
table, and extracting values from downloaded NetCDF files at point locations.
"""

import datetime as _dt
import re

import numpy as np
import pandas as pd
import xarray as xr

_GRID_SERVICES = ("arco-geo-series", "arco-time-series")
_LAT_NAMES = ("latitude", "lat", "nav_lat")
_LON_NAMES = ("longitude", "lon", "nav_lon")
_DEPTH_NAMES = ("depth", "deptht", "depthu", "depthv", "lev", "elevation")
_UNIT_MS = {
    "millisecond": 1.0,
    "second": 1000.0,
    "minute": 60_000.0,
    "hour": 3_600_000.0,
    "day": 86_400_000.0,
}


# ---------------------------------------------------------------------------
# Catalogue
# ---------------------------------------------------------------------------


# A date and optional time, with an optional time zone, anywhere in a string:
# "1970-01-01", "1970-01-01 00:00:00Z", "1950-01-01T00:00:00.000+01:00", ...
# Anything after it, such as "(no leap seconds)", is ignored.
_DATETIME_RE = re.compile(
    r"(\d{1,4})-(\d{1,2})-(\d{1,2})"
    r"(?:[T\s]+(\d{1,2}):(\d{1,2})(?::(\d{1,2}(?:\.\d*)?))?)?"
    r"\s*(Z|UTC|GMT|[+-]\d{1,2}(?::?\d{2})?)?",
    re.IGNORECASE,
)
_EPOCH = _dt.datetime(1970, 1, 1)


def _datetime_to_ms(text):
    """Milliseconds since 1970-01-01 UTC of a date-time written in `text`."""
    m = _DATETIME_RE.search(text or "")
    if not m:
        raise ValueError(f"no date found in {text!r}")
    day = _dt.datetime(int(m.group(1)), int(m.group(2)), int(m.group(3)))
    ms = (day - _EPOCH) / _dt.timedelta(milliseconds=1)
    ms += ((int(m.group(4) or 0) * 60 + int(m.group(5) or 0)) * 60
           + float(m.group(6) or 0)) * 1000.0
    zone = m.group(7)
    if zone and zone[0] in "+-":
        digits = zone[1:].replace(":", "")
        hours, minutes = (int(digits[:-2]), int(digits[-2:])) if len(digits) > 2 else (int(digits), 0)
        ms -= (1 if zone[0] == "+" else -1) * (hours * 60 + minutes) * 60_000.0
    return float(ms)


def _unit_factor_and_origin(unit):
    """Milliseconds per unit and the origin (ms since epoch) of a CF time unit."""
    m = re.match(r"\s*(\w+?)s?\s+since\s+(.+)$", unit or "")
    if not m:
        return 1.0, 0.0  # catalogue default: milliseconds since 1970-01-01
    factor = _UNIT_MS.get(m.group(1).lower(), 1.0)
    return factor, _datetime_to_ms(m.group(2))


def _time_to_ms(value, unit):
    if value is None:
        return None
    if isinstance(value, str):
        return _datetime_to_ms(value)
    factor, origin = _unit_factor_and_origin(unit)
    return float(value) * factor + origin


def _ms_to_iso(ms):
    if ms is None or not np.isfinite(ms):
        return None
    return (pd.Timestamp("1970-01-01") + pd.Timedelta(milliseconds=ms)).strftime(
        "%Y-%m-%dT%H:%M:%S"
    )


def _range_and_step(coord):
    """(min, max, step, n) of a numeric coordinate."""
    values = coord.values
    if values:
        arr = np.asarray([float(v) for v in values])
        step = float(np.median(np.abs(np.diff(arr)))) if arr.size > 1 else None
        return float(arr.min()), float(arr.max()), step, int(arr.size)
    lo = None if coord.minimum_value is None else float(coord.minimum_value)
    hi = None if coord.maximum_value is None else float(coord.maximum_value)
    step = None if coord.step is None else abs(float(coord.step))
    n = None
    if lo is not None and hi is not None and step:
        n = int(round((hi - lo) / step)) + 1
    return lo, hi, step, n


def _time_info(coord):
    """(start ms, end ms, step hours) of a time coordinate."""
    unit = coord.coordinate_unit
    if coord.values:
        ms = np.asarray([_time_to_ms(v, unit) for v in coord.values])
        ms.sort()
        step = float(np.median(np.diff(ms))) / 3_600_000.0 if ms.size > 1 else None
        return ms[0], ms[-1], step
    start = _time_to_ms(coord.minimum_value, unit)
    end = _time_to_ms(coord.maximum_value, unit)
    step = None
    if coord.step:
        factor, _ = _unit_factor_and_origin(unit)
        step = abs(float(coord.step)) * factor / 3_600_000.0
    return start, end, step


def _pick_part(version):
    parts = version.parts or []
    for part in parts:
        if part.name == "default":
            return part
    return parts[0] if parts else None


def _pick_service(part):
    by_name = {}
    for service in part.services or []:
        name = getattr(service.service_name, "value", service.service_name)
        by_name[name] = service
    for name in _GRID_SERVICES:
        if name in by_name:
            return by_name[name]
    return None


def _variable_row(product, dataset, version, part, var):
    """The catalogue row for one variable, or None if it is not on a lat/lon/time grid."""
    coords = {c.coordinate_id: c for c in var.coordinates}
    if not {"latitude", "longitude", "time"} <= set(coords):
        return None
    t0, t1, t_step = _time_info(coords["time"])
    lon0, lon1, lon_step, _ = _range_and_step(coords["longitude"])
    lat0, lat1, lat_step, _ = _range_and_step(coords["latitude"])
    depth = coords.get("depth") or coords.get("elevation")
    d0 = d1 = n_depth = None
    if depth is not None:
        d0, d1, _, n_depth = _range_and_step(depth)
    return dict(
        product_id=product.product_id,
        product_title=product.title,
        product_doi=product.digital_object_identifier or "",
        processing_level=product.processing_level,
        production_center=product.production_center,
        dataset_id=dataset.dataset_id,
        dataset_name=dataset.dataset_name,
        version=version.label,
        part=part.name,
        variable=var.short_name,
        standard_name=var.standard_name or "",
        units=var.units or "",
        time_start=_ms_to_iso(t0),
        time_end=_ms_to_iso(t1),
        time_step_hours=t_step,
        lon_min=lon0,
        lon_max=lon1,
        lat_min=lat0,
        lat_max=lat1,
        lon_step=lon_step,
        lat_step=lat_step,
        has_depth=depth is not None,
        depth_min=d0,
        depth_max=d1,
        n_depth=n_depth,
    )


def flatten_catalogue(catalogue, skipped=None):
    """
    One row per (dataset, variable) for gridded datasets that have latitude,
    longitude and time axes. Returns a pandas DataFrame.

    A dataset whose catalogue entry cannot be read (for example, a time unit
    written in a way not seen before) is left out rather than stopping the
    whole catalogue; its ID is added to the list `skipped`, if given.
    """
    rows = []
    for product in catalogue.products:
        for dataset in product.datasets:
            if not dataset.versions:
                continue
            version = dataset.versions[0]
            part = _pick_part(version)
            if part is None:
                continue
            service = _pick_service(part)
            if service is None:
                continue
            try:
                found = [_variable_row(product, dataset, version, part, var)
                         for var in service.variables]
            except Exception:
                if skipped is not None:
                    skipped.append(dataset.dataset_id)
                continue
            rows.extend(r for r in found if r is not None)
    return pd.DataFrame(rows)


def to_columns(df):
    """
    A DataFrame as a dict of plain Python lists (str, float or bool), which
    reticulate converts to R vectors regardless of pandas or numpy version.
    Missing numbers become NaN and missing text becomes "".
    """
    out = {}
    for col in df.columns:
        s = df[col]
        if pd.api.types.is_bool_dtype(s):
            out[col] = [bool(x) for x in s]
        elif pd.api.types.is_numeric_dtype(s):
            out[col] = [float(x) if x is not None and np.isfinite(x) else float("nan")
                        for x in s]
        else:
            out[col] = [
                "" if (x is None or (isinstance(x, float) and np.isnan(x))) else str(x)
                for x in s
            ]
    return out


def catalogue_table(disable_progress_bar=True, describe=None):
    """
    Fetch the full Copernicus Marine catalogue, flattened to columns. Returns
    {"columns": <dict of lists>, "skipped": <IDs of datasets left out>}.
    `describe` replaces copernicusmarine.describe (used in tests).
    """
    if describe is None:
        import copernicusmarine

        describe = copernicusmarine.describe
    catalogue = describe(disable_progress_bar=disable_progress_bar)
    skipped = []
    columns = to_columns(flatten_catalogue(catalogue, skipped))
    return {"columns": columns, "skipped": sorted(set(skipped))}


def read_json(path):
    """Read a JSON file written by copernicopia_catalogue.py."""
    import json

    with open(path, encoding="utf-8") as f:
        return json.load(f)


# ---------------------------------------------------------------------------
# Point extraction
# ---------------------------------------------------------------------------


def _find_name(names, candidates):
    lower = {n.lower(): n for n in names}
    for c in candidates:
        if c in lower:
            return lower[c]
    return None


def _nearest_index(coord, values, tolerance=None):
    """
    Index of the nearest coordinate value for each entry of `values`, or -1
    when the value is missing or further than `tolerance` from any coordinate.
    By default the tolerance is half a grid step, i.e. the point must fall
    inside a grid cell.
    """
    coord = np.asarray(coord, dtype=float)
    values = np.asarray(values, dtype=float)
    out = np.full(values.shape, -1, dtype=np.int64)
    ok = np.isfinite(values)
    if coord.size == 0 or not ok.any():
        return out
    order = np.argsort(coord)
    cs = coord[order]
    if cs.size == 1:
        idx = np.zeros(values.shape, dtype=np.int64)
        if tolerance is None:
            tolerance = np.inf
    else:
        pos = np.clip(np.searchsorted(cs, np.where(ok, values, cs[0])), 1, cs.size - 1)
        left, right = cs[pos - 1], cs[pos]
        v = np.where(ok, values, cs[0])
        choose = np.where(np.abs(v - left) <= np.abs(right - v), pos - 1, pos)
        idx = order[choose]
        if tolerance is None:
            tolerance = 0.5 * float(np.median(np.diff(cs))) * 1.001
    dist = np.abs(coord[idx] - np.where(ok, values, np.nan))
    good = ok & (dist <= tolerance)
    out[good] = idx[good]
    return out


def _nearest_lon_index(lons, plon):
    """
    Like _nearest_index for longitudes, whatever range the grid's longitudes
    are written in: -180 to 180, 0 to 360, or continuous across 180 (e.g. 170
    to 190, as the toolbox writes regions that cross 180). Points are moved to
    the grid's 360-degree window, and a point within half a cell of the window's
    far end is also tried one turn round, so the first and last columns of a
    global grid meet.
    """
    lons = np.asarray(lons, dtype=float)
    plon = np.asarray(plon, dtype=float)
    if lons.size == 0:
        return np.full(plon.shape, -1, dtype=np.int64)
    step = float(np.median(np.abs(np.diff(np.sort(lons))))) if lons.size > 1 else 0.0
    base = float(np.nanmin(lons)) - step / 2
    moved = base + np.mod(plon - base, 360.0)
    idx = _nearest_index(lons, moved)
    missing = idx < 0
    if missing.any():
        idx[missing] = _nearest_index(lons, moved[missing] - 360.0)
    return idx


def _period_keys(ms, kind):
    """Calendar keys used to match observations to dataset time steps."""
    t = pd.to_datetime(pd.Series(ms), unit="ms")
    if kind == "year":
        return t.dt.year.to_numpy(dtype=float, na_value=np.nan)
    if kind == "month":
        return (t.dt.year * 12 + t.dt.month - 1).to_numpy(dtype=float, na_value=np.nan)
    if kind == "day":
        return np.floor(np.asarray(ms, dtype=float) / 86_400_000.0)
    raise ValueError(kind)


def match_times(obs_ms, data_ms, time_step_hours=None, time_match="period"):
    """
    For each observation time, the index of the matching dataset time step,
    or -1 if none matches.

    time_match = "period": daily data are matched on the calendar day (UTC),
    monthly data on the calendar month and yearly data on the year. Other
    resolutions (hourly, 3-hourly, 8-day, ...) use the nearest time step.
    time_match = "nearest": always the nearest time step.
    In both cases a nearest match further away than one time step is rejected.
    """
    obs_ms = np.asarray(obs_ms, dtype=float)
    data_ms = np.asarray(data_ms, dtype=float)
    out = np.full(obs_ms.shape, -1, dtype=np.int64)
    if data_ms.size == 0:
        return out
    step_h = time_step_hours
    if step_h is None or not np.isfinite(step_h):
        step_h = (
            float(np.median(np.diff(np.sort(data_ms)))) / 3_600_000.0
            if data_ms.size > 1
            else 24.0
        )
    kind = None
    if time_match == "period":
        if step_h >= 24 * 360:
            kind = "year"
        elif step_h >= 24 * 27:
            kind = "month"
        elif 20 <= step_h <= 28:
            kind = "day"
    if kind is not None:
        dkeys = _period_keys(data_ms, kind)
        okeys = _period_keys(obs_ms, kind)
        lookup = {}
        for i, k in enumerate(dkeys):
            lookup.setdefault(k, i)
        for j, k in enumerate(okeys):
            if np.isfinite(k):
                out[j] = lookup.get(k, -1)
        return out
    return _nearest_index(data_ms, obs_ms, tolerance=step_h * 3_600_000.0)


def _file_times(path):
    with xr.open_dataset(path) as ds:
        name = "time" if "time" in ds.coords else None
        if name is None:
            return np.array([], dtype=float)
        t = ds[name].values.astype("datetime64[ms]").astype("int64")
        return t.astype(float)


def extract_points(
    files,
    variables,
    lon,
    lat,
    time_ms,
    depth=None,
    time_step_hours=None,
    time_match="period",
):
    """
    Values of `variables` at each (lon, lat, time[, depth]) point, taken from
    the nearest grid cell of the matching time step across `files`.

    Returns a dict of plain lists of floats: one per variable plus `matched_time_ms`,
    the dataset time step used for each point (NaN when the point could not be
    matched in time).
    """
    if isinstance(files, str):
        files = [files]
    if isinstance(variables, str):
        variables = [variables]
    lon = np.asarray(lon, dtype=float).ravel()
    lat = np.asarray(lat, dtype=float).ravel()
    time_ms = np.asarray(time_ms, dtype=float).ravel()
    n = lon.size
    obs_depth = (
        np.full(n, np.nan) if depth is None else np.asarray(depth, dtype=float).ravel()
    )
    result = {v: np.full(n, np.nan) for v in variables}
    matched = np.full(n, np.nan)

    # Global time axis across all files, so "nearest" works across file edges
    file_idx, local_idx, all_ms = [], [], []
    for fi, path in enumerate(files):
        t = _file_times(path)
        file_idx.extend([fi] * t.size)
        local_idx.extend(range(t.size))
        all_ms.extend(t.tolist())
    if not all_ms:
        result["matched_time_ms"] = matched
        return {k: v.tolist() for k, v in result.items()}
    file_idx = np.asarray(file_idx)
    local_idx = np.asarray(local_idx)
    all_ms = np.asarray(all_ms, dtype=float)
    order = np.argsort(all_ms, kind="stable")
    file_idx, local_idx, all_ms = file_idx[order], local_idx[order], all_ms[order]

    g = match_times(time_ms, all_ms, time_step_hours, time_match)
    has_time = g >= 0
    matched[has_time] = all_ms[g[has_time]]

    for fi, path in enumerate(files):
        sel = np.where(has_time & (file_idx[np.where(has_time, g, 0)] == fi))[0]
        if sel.size == 0:
            continue
        ti = local_idx[g[sel]]
        with xr.open_dataset(path) as ds:
            names = list(ds.dims) + list(ds.coords)
            lat_name = _find_name(names, _LAT_NAMES)
            lon_name = _find_name(names, _LON_NAMES)
            if lat_name is None or lon_name is None:
                raise ValueError(f"{path}: no latitude/longitude coordinates found")
            if ds[lat_name].ndim != 1 or ds[lon_name].ndim != 1:
                raise ValueError(
                    f"{path}: latitude/longitude are not 1D; only regular "
                    "lat/lon grids are supported"
                )
            lons = ds[lon_name].values.astype(float)
            yi = _nearest_index(ds[lat_name].values, lat[sel])
            xi = _nearest_lon_index(lons, lon[sel])

            depth_name = _find_name(list(ds.dims), _DEPTH_NAMES)
            di = None
            if depth_name is not None:
                dvals = np.abs(ds[depth_name].values.astype(float))
                d_obs = obs_depth[sel]
                di = _nearest_index(dvals, np.abs(d_obs), tolerance=np.inf)
                di = np.where(np.isfinite(d_obs), di, int(np.argmin(dvals)))

            good = (yi >= 0) & (xi >= 0)
            if di is not None:
                good &= di >= 0
            if not good.any():
                continue
            pts = sel[good]
            for v in variables:
                if v not in ds.data_vars:
                    continue
                da = ds[v]
                idx = {}
                for dim in da.dims:
                    if dim == "time":
                        idx[dim] = xr.DataArray(ti[good], dims="points")
                    elif dim == lat_name:
                        idx[dim] = xr.DataArray(yi[good], dims="points")
                    elif dim == lon_name:
                        idx[dim] = xr.DataArray(xi[good], dims="points")
                    elif dim == depth_name:
                        idx[dim] = xr.DataArray(di[good], dims="points")
                    elif da.sizes[dim] == 1:
                        idx[dim] = 0
                    else:
                        raise ValueError(f"{path}: {v} has unsupported dimension {dim}")
                vals = da.isel(idx).values.astype(float)
                result[v][pts] = vals

    result["matched_time_ms"] = matched
    return {k: v.tolist() for k, v in result.items()}


# ---------------------------------------------------------------------------
# Download jobs
# ---------------------------------------------------------------------------


def write_job(path, job):
    """Write a job file for copernicopia_worker.py."""
    import json

    with open(path, "w", encoding="utf-8") as f:
        json.dump(job, f)
