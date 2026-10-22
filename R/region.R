# Longitude ranges, including ones that cross the 180 degree meridian.
#
# A range is c(west, east): the region runs EAST from the first longitude to
# the second. So c(170, -170) is the 20-degree strip across 180, and
# c(-170, 170) is everything except that strip. Ranges are stored with both
# edges between -180 and 180; the eastern edge is less than the western edge
# when the range crosses 180. All longitudes, c(-180, 180), is the one range
# whose edges are the same place.

# Longitude in [-180, 180)
.lon_mod <- function(x) ((x + 180) %% 360) - 180

# Width of a range in degrees, 0 to 360
.lon_width <- function(lon) {
  if (lon[1] == -180 && lon[2] == 180) return(360)
  (lon[2] - lon[1]) %% 360
}

.lon_crosses_180 <- function(lon) .lon_width(lon) < 360 && lon[2] < lon[1]

# Checks a longitude range given by the user (either -180 to 180 or 0 to 360
# style) and returns it as c(west, east) with both edges in -180 to 180.
.normalise_lon <- function(lon) {
  lon <- as.numeric(lon)
  if (length(lon) != 2 || anyNA(lon) || any(!is.finite(lon)) || any(abs(lon) > 360)) {
    stop("`lon` must be c(west, east), two longitudes in degrees.", call. = FALSE)
  }
  width <- lon[2] - lon[1]
  if (width >= 360 || (width <= -360)) return(c(-180, 180))
  width <- width %% 360
  if (isTRUE(all.equal(width, 0))) {
    stop("The western and eastern edges of `lon` must differ.", call. = FALSE)
  }
  west <- .lon_mod(lon[1])
  east <- .lon_mod(west + width)
  if (east == -180) east <- 180
  c(west, east)
}

# The eastern edge as a number greater than the western edge (190 rather than
# -170), for arithmetic across 180.
.lon_east_unwrapped <- function(lon) lon[1] + .lon_width(lon)

# "170E", "170W", "0", "180", with a degree sign where the console allows
.lon_deg <- function(x, ascii = FALSE) {
  deg <- if (ascii) "" else trimws(.deg())
  if (ascii || deg == "deg") deg <- ""
  hemi <- if (x == 0 || abs(x) == 180) "" else if (x > 0) "E" else "W"
  sprintf("%s%s%s", format(abs(x)), deg, hemi)
}

# A plain description of a longitude range, e.g.
# "170°E to 170°W, crossing 180° (20° wide)"
.lon_label <- function(lon) {
  w <- .lon_width(lon)
  deg <- if (trimws(.deg()) == "deg") " degrees" else trimws(.deg())
  if (w == 360) return(sprintf("all longitudes (360%s)", deg))
  sprintf("%s to %s%s (%s%s wide)", .lon_deg(lon[1]), .lon_deg(lon[2]),
          if (.lon_crosses_180(lon)) sprintf(", crossing 180%s", deg) else "",
          format(round(w, 4)), deg)
}

# The same in plain ASCII for text files: "170 to -170 (crossing 180 degrees;
# 20 degrees wide)". Ranges that do not cross 180 stay "100 to 160".
.lon_text <- function(lon) {
  w <- .lon_width(lon)
  if (w == 360) return("-180 to 180 (all longitudes)")
  base <- sprintf("%s to %s", format(lon[1]), format(lon[2]))
  if (.lon_crosses_180(lon)) {
    base <- sprintf("%s (crossing 180 degrees; %s degrees wide)", base, format(round(w, 4)))
  }
  base
}

# The smallest range covering every longitude in `x`, crossing 180 if that is
# shorter: points at 179 and -179 give c(179, -179), not c(-179, 179).
.lon_extent <- function(x) {
  x <- sort(unique(.lon_mod(x[is.finite(x)])))
  if (length(x) == 0) return(c(NA_real_, NA_real_))
  if (length(x) == 1) return(c(x, x))
  gaps <- c(diff(x), x[1] + 360 - x[length(x)])
  i <- which.max(gaps)
  if (i == length(x)) return(range(x))
  c(x[i + 1], x[i])
}

# A range widened by `pad` degrees on each side; all longitudes if that would
# wrap all the way round. Works for single points (c(x, x)) too.
.lon_pad <- function(lon, pad) {
  width <- if (lon[1] == lon[2]) 0 else .lon_width(lon)
  if (width + 2 * pad >= 360) return(c(-180, 180))
  west <- lon[1] - pad
  c(west, west + width + 2 * pad)
}
