Copernicopia validation
================

This report checks the values Copernicopia appends to a data frame
against an independent extraction of the same Copernicus Marine data. It
covers nine products, which between them use every way Copernicopia
matches data to locations and times: daily, hourly and monthly time
steps, the surface level of 3D products, flag variables, four grid
resolutions, and regions that cross 180°.

It was made by knitting [`validation.Rmd`](validation.Rmd) on the
author’s computer, where the tracking data and the independent
extraction are kept. The paths to them are set as parameters at the top
of the file.

## Summary

Across 10 downloads and 33 variables, 707,723 values were present in
both extractions. Of these, **707,657 (99.9907%) were identical**. The
66 that differed were all at points less than 0.0001 degrees from the
edge of a grid cell, where either neighbouring cell is equally close.
107 values were present only in Copernicopia’s extraction, all at points
within half a grid cell of the 180 degree meridian: the independent
extraction’s grid stops just short of 180 degrees there, while
Copernicopia joins the grid’s two edges.

## What was compared

**Points.** 22,304 hourly tracking locations from 01 January 2026 to 31
January 2026, in the Southern Ocean from 76.8°E east across 180° to
71.8°W. Individual locations are not shown.

**Independent extraction.** The data were downloaded with the
`copernicusmarine` Python toolbox by
[`download_cmems.py`](https://github.com/BMViola/Copernicus-Bulk-Files)
(longitude -180 to 180, latitude -80 to -15, monthly files), and values
were extracted in R with `terra`: each location takes the value of the
grid cell containing it, from the time step on the same UTC day (daily
data), the nearest hour (hourly data) or the same calendar month
(monthly data). That code is shown [at the
end](#independent-extraction-code).

**Copernicopia.** The same datasets, variables and depths were
downloaded with `cmems_download()` and appended with `cmems_extract()`,
using the defaults. SST was run twice: once as the independent download
was made (the full band, one monthly file), and once as the wizard
suggested for these points (their area, crossing 180°, one file per
day). Every other product used the points’ area.

| Download | Dataset | Variables | Region | Files | MB | Downloaded |
|:---|:---|:---|:---|---:|---:|:---|
| SST (band, monthly file) | METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2 | analysed_sst, analysis_error, sea_ice_fraction | band | 1 | 216 | yes |
| SST (data’s area, daily files) | METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2 | analysed_sst, analysis_error, sea_ice_fraction | data’s area | 31 | 94 | yes |
| GLORYS12 physics | cmems_mod_glo_phy_my_0.083deg_P1D-m | thetao, so, mlotst, zos | data’s area | 1 | 134 | yes |
| Chlorophyll-a (gap-free) | cmems_obs-oc_glo_bgc-plankton_my_l4-gapfree-multi-4km_P1D | CHL, CHL_uncertainty | data’s area | 1 | 519 | yes |
| Phytoplankton groups | cmems_obs-oc_glo_bgc-plankton_my_l4-multi-4km_P1M | CHL, DIATO, DINO, GREEN, HAPTO, MICRO, NANO, PICO, PROCHLO, PROKAR | data’s area | 1 | 119 | yes |
| Zooplankton | cmems_mod_glo_bgc-plankton_anfc_0.25deg_P1D-m | zooc | data’s area | 1 | 10 | yes |
| Wind | cmems_obs-wind_glo_phy_nrt_l4_0.125deg_PT1H | eastward_wind, northward_wind | data’s area | 1 | 808 | yes |
| Sea level and geostrophic currents | cmems_obs-sl_glo_phy-ssh_nrt_allsat-l4-duacs-0.125deg_P1D | adt, sla, ugos, vgos, ugosa, vgosa | data’s area | 1 | 136 | yes |
| Sea ice concentration | osisaf_obs-si_glo_phy-sic-south_nrt_amsr2_l4_P1D-m | ice_conc | data’s area | 1 | 3 | yes |
| Sea ice edge | osisaf_obs-si_glo_phy-siedge_nrt_sh-P1D | ice_edge | data’s area | 1 | 1 | yes |

## Results

One row per variable. *values*: present in both extractions;
*only_independent*, *only_copernicopia*: present in that extraction but
not the other; *differ*: values more than 0.000001 apart;
*max_abs_diff*: largest difference, in the variable’s units; *r*:
correlation between the two.

| test | variable | values | only_independent | only_copernicopia | differ | max_abs_diff | r |
|:---|:---|---:|---:|---:|---:|---:|---:|
| SST (band, monthly file) | analysed_sst | 22113 | 0 | 0 | 2 | 0.05000 | 1.000000 |
| SST (band, monthly file) | analysis_error | 22113 | 0 | 0 | 1 | 0.06000 | 0.999997 |
| SST (band, monthly file) | sea_ice_fraction | 22113 | 0 | 0 | 0 | 0.00000 | 1.000000 |
| SST (data’s area, daily files) | analysed_sst | 22113 | 0 | 0 | 2 | 0.05000 | 1.000000 |
| SST (data’s area, daily files) | analysis_error | 22113 | 0 | 0 | 1 | 0.06000 | 0.999997 |
| SST (data’s area, daily files) | sea_ice_fraction | 22113 | 0 | 0 | 0 | 0.00000 | 1.000000 |
| GLORYS12 physics | thetao | 21887 | 0 | 11 | 0 | 0.00000 | 1.000000 |
| GLORYS12 physics | so | 21887 | 0 | 11 | 0 | 0.00000 | 1.000000 |
| GLORYS12 physics | mlotst | 21887 | 0 | 11 | 0 | 0.00000 | 1.000000 |
| GLORYS12 physics | zos | 21887 | 0 | 11 | 0 | 0.00000 | 1.000000 |
| Chlorophyll-a (gap-free) | CHL | 21942 | 0 | 0 | 5 | 0.23500 | 0.999993 |
| Chlorophyll-a (gap-free) | CHL_uncertainty | 21942 | 0 | 0 | 5 | 2.09000 | 0.999999 |
| Phytoplankton groups | CHL | 21945 | 0 | 0 | 5 | 0.06520 | 0.999999 |
| Phytoplankton groups | DIATO | 21431 | 0 | 0 | 5 | 0.01980 | 0.999996 |
| Phytoplankton groups | DINO | 21431 | 0 | 0 | 5 | 0.00123 | 1.000000 |
| Phytoplankton groups | GREEN | 21431 | 0 | 0 | 5 | 0.09990 | 0.999980 |
| Phytoplankton groups | HAPTO | 21431 | 0 | 0 | 5 | 0.07220 | 0.999979 |
| Phytoplankton groups | MICRO | 21431 | 0 | 0 | 5 | 0.02040 | 0.999998 |
| Phytoplankton groups | NANO | 21431 | 0 | 0 | 5 | 0.07220 | 0.999979 |
| Phytoplankton groups | PICO | 21431 | 0 | 0 | 5 | 0.10200 | 0.999981 |
| Phytoplankton groups | PROCHLO | 21431 | 0 | 0 | 5 | 0.04390 | 0.999987 |
| Phytoplankton groups | PROKAR | 21431 | 0 | 0 | 5 | 0.00193 | 0.999998 |
| Zooplankton | zooc | 22013 | 0 | 37 | 0 | 0.00000 | 1.000000 |
| Wind | eastward_wind | 22304 | 0 | 0 | 0 | 0.00000 | 1.000000 |
| Wind | northward_wind | 22304 | 0 | 0 | 0 | 0.00000 | 1.000000 |
| Sea level and geostrophic currents | adt | 22218 | 0 | 0 | 0 | 0.00000 | 1.000000 |
| Sea level and geostrophic currents | sla | 22218 | 0 | 0 | 0 | 0.00000 | 1.000000 |
| Sea level and geostrophic currents | ugos | 22218 | 0 | 0 | 0 | 0.00000 | 1.000000 |
| Sea level and geostrophic currents | vgos | 22218 | 0 | 0 | 0 | 0.00000 | 1.000000 |
| Sea level and geostrophic currents | ugosa | 22218 | 0 | 0 | 0 | 0.00000 | 1.000000 |
| Sea level and geostrophic currents | vgosa | 22218 | 0 | 0 | 0 | 0.00000 | 1.000000 |
| Sea ice concentration | ice_conc | 7802 | 0 | 10 | 0 | 0.00000 | 1.000000 |
| Sea ice edge | ice_edge | 21058 | 0 | 16 | 0 | 0.00000 | 1.000000 |

### Values that differ

*difference*: Copernicopia minus independent, in the variable’s units;
*deg_to_cell_edge*: how far the point is from the nearest edge of a grid
cell, in degrees.

| test | variable | time_utc | independent | copernicopia | difference | deg_to_cell_edge |
|:---|:---|:---|---:|---:|---:|---:|
| SST (band, monthly file) | analysed_sst | 2026-01-27 15:02 | 289.1300000 | 289.1800000 | 5.00e-02 | 6.0e-07 |
| SST (band, monthly file) | analysed_sst | 2026-01-25 16:00 | 273.7500000 | 273.7200000 | -3.00e-02 | 1.8e-06 |
| SST (band, monthly file) | analysis_error | 2026-01-27 15:02 | 0.5400000 | 0.4800000 | -6.00e-02 | 6.0e-07 |
| SST (data’s area, daily files) | analysed_sst | 2026-01-27 15:02 | 289.1300000 | 289.1800000 | 5.00e-02 | 6.0e-07 |
| SST (data’s area, daily files) | analysed_sst | 2026-01-25 16:00 | 273.7500000 | 273.7200000 | -3.00e-02 | 1.8e-06 |
| SST (data’s area, daily files) | analysis_error | 2026-01-27 15:02 | 0.5400000 | 0.4800000 | -6.00e-02 | 6.0e-07 |
| Chlorophyll-a (gap-free) | CHL | 2026-01-19 08:02 | 0.1632790 | 0.1650660 | 1.79e-03 | 5.1e-06 |
| Chlorophyll-a (gap-free) | CHL | 2026-01-10 04:02 | 1.7504100 | 1.5153700 | -2.35e-01 | 8.2e-06 |
| Chlorophyll-a (gap-free) | CHL | 2026-01-03 00:02 | 0.5222760 | 0.5047880 | -1.75e-02 | 1.0e-07 |
| Chlorophyll-a (gap-free) | CHL | 2026-01-24 01:10 | 0.2215890 | 0.2316240 | 1.00e-02 | 1.5e-06 |
| Chlorophyll-a (gap-free) | CHL | 2026-01-02 16:00 | 0.0912075 | 0.0946574 | 3.45e-03 | 9.0e-07 |
| Chlorophyll-a (gap-free) | CHL_uncertainty | 2026-01-19 08:02 | 35.6900000 | 35.6300000 | -6.00e-02 | 5.1e-06 |
| Chlorophyll-a (gap-free) | CHL_uncertainty | 2026-01-10 04:02 | 40.0600000 | 37.9700000 | -2.09e+00 | 8.2e-06 |
| Chlorophyll-a (gap-free) | CHL_uncertainty | 2026-01-03 00:02 | 25.6000000 | 25.7800000 | 1.80e-01 | 1.0e-07 |
| Chlorophyll-a (gap-free) | CHL_uncertainty | 2026-01-24 01:10 | 29.1200000 | 28.8900000 | -2.30e-01 | 1.5e-06 |
| Chlorophyll-a (gap-free) | CHL_uncertainty | 2026-01-02 16:00 | 38.0300000 | 37.8400000 | -1.90e-01 | 9.0e-07 |
| Phytoplankton groups | CHL | 2026-01-19 08:02 | 0.2169530 | 0.2277920 | 1.08e-02 | 5.1e-06 |
| Phytoplankton groups | CHL | 2026-01-10 04:02 | 1.9788100 | 2.0440300 | 6.52e-02 | 8.2e-06 |
| Phytoplankton groups | CHL | 2026-01-03 00:02 | 0.4381730 | 0.4458070 | 7.63e-03 | 1.0e-07 |
| Phytoplankton groups | CHL | 2026-01-24 01:10 | 0.1834050 | 0.1964890 | 1.31e-02 | 1.5e-06 |
| Phytoplankton groups | CHL | 2026-01-02 16:00 | 0.1103580 | 0.1126420 | 2.28e-03 | 9.0e-07 |
| Phytoplankton groups | DIATO | 2026-01-19 08:02 | 0.0361186 | 0.0361127 | -5.90e-06 | 5.1e-06 |
| Phytoplankton groups | DIATO | 2026-01-10 04:02 | 0.1543730 | 0.1346090 | -1.98e-02 | 8.2e-06 |
| Phytoplankton groups | DIATO | 2026-01-03 00:02 | 0.0821708 | 0.0865502 | 4.38e-03 | 1.0e-07 |
| Phytoplankton groups | DIATO | 2026-01-24 01:10 | 0.0204939 | 0.0212299 | 7.36e-04 | 1.5e-06 |
| Phytoplankton groups | DIATO | 2026-01-02 16:00 | 0.0850086 | 0.0911064 | 6.10e-03 | 9.0e-07 |
| Phytoplankton groups | DINO | 2026-01-19 08:02 | 0.0150878 | 0.0151284 | 4.07e-05 | 5.1e-06 |
| Phytoplankton groups | DINO | 2026-01-10 04:02 | 0.1079010 | 0.1072800 | -6.21e-04 | 8.2e-06 |
| Phytoplankton groups | DINO | 2026-01-03 00:02 | 0.0310589 | 0.0318275 | 7.69e-04 | 1.0e-07 |
| Phytoplankton groups | DINO | 2026-01-24 01:10 | 0.0099417 | 0.0102916 | 3.50e-04 | 1.5e-06 |
| Phytoplankton groups | DINO | 2026-01-02 16:00 | 0.0176927 | 0.0189271 | 1.23e-03 | 9.0e-07 |
| Phytoplankton groups | GREEN | 2026-01-19 08:02 | 0.0589569 | 0.0593907 | 4.34e-04 | 5.1e-06 |
| Phytoplankton groups | GREEN | 2026-01-10 04:02 | 0.3649030 | 0.4648460 | 9.99e-02 | 8.2e-06 |
| Phytoplankton groups | GREEN | 2026-01-03 00:02 | 0.0925362 | 0.0978915 | 5.36e-03 | 1.0e-07 |
| Phytoplankton groups | GREEN | 2026-01-24 01:10 | 0.0291621 | 0.0297515 | 5.89e-04 | 1.5e-06 |
| Phytoplankton groups | GREEN | 2026-01-02 16:00 | 0.0555579 | 0.0555755 | 1.77e-05 | 9.0e-07 |
| Phytoplankton groups | HAPTO | 2026-01-19 08:02 | 0.0634478 | 0.0641703 | 7.22e-04 | 5.1e-06 |
| Phytoplankton groups | HAPTO | 2026-01-10 04:02 | 0.2315080 | 0.1593340 | -7.22e-02 | 8.2e-06 |
| Phytoplankton groups | HAPTO | 2026-01-03 00:02 | 0.1287470 | 0.1226880 | -6.06e-03 | 1.0e-07 |
| Phytoplankton groups | HAPTO | 2026-01-24 01:10 | 0.0446242 | 0.0454796 | 8.55e-04 | 1.5e-06 |
| Phytoplankton groups | HAPTO | 2026-01-02 16:00 | 0.0396871 | 0.0423578 | 2.67e-03 | 9.0e-07 |
| Phytoplankton groups | MICRO | 2026-01-19 08:02 | 0.0512064 | 0.0512411 | 3.47e-05 | 5.1e-06 |
| Phytoplankton groups | MICRO | 2026-01-10 04:02 | 0.2622740 | 0.2418890 | -2.04e-02 | 8.2e-06 |
| Phytoplankton groups | MICRO | 2026-01-03 00:02 | 0.1132300 | 0.1183780 | 5.15e-03 | 1.0e-07 |
| Phytoplankton groups | MICRO | 2026-01-24 01:10 | 0.0304356 | 0.0315215 | 1.09e-03 | 1.5e-06 |
| Phytoplankton groups | MICRO | 2026-01-02 16:00 | 0.1027010 | 0.1100330 | 7.33e-03 | 9.0e-07 |
| Phytoplankton groups | NANO | 2026-01-19 08:02 | 0.0634478 | 0.0641703 | 7.22e-04 | 5.1e-06 |
| Phytoplankton groups | NANO | 2026-01-10 04:02 | 0.2315080 | 0.1593340 | -7.22e-02 | 8.2e-06 |
| Phytoplankton groups | NANO | 2026-01-03 00:02 | 0.1287470 | 0.1226880 | -6.06e-03 | 1.0e-07 |
| Phytoplankton groups | NANO | 2026-01-24 01:10 | 0.0446242 | 0.0454796 | 8.55e-04 | 1.5e-06 |
| Phytoplankton groups | NANO | 2026-01-02 16:00 | 0.0396871 | 0.0423578 | 2.67e-03 | 9.0e-07 |
| Phytoplankton groups | PICO | 2026-01-19 08:02 | 0.0756031 | 0.0761341 | 5.31e-04 | 5.1e-06 |
| Phytoplankton groups | PICO | 2026-01-10 04:02 | 0.3984840 | 0.5003540 | 1.02e-01 | 8.2e-06 |
| Phytoplankton groups | PICO | 2026-01-03 00:02 | 0.1098070 | 0.1146180 | 4.81e-03 | 1.0e-07 |
| Phytoplankton groups | PICO | 2026-01-24 01:10 | 0.0471233 | 0.0475855 | 4.62e-04 | 1.5e-06 |
| Phytoplankton groups | PICO | 2026-01-02 16:00 | 0.0625378 | 0.0624753 | -6.25e-05 | 9.0e-07 |
| Phytoplankton groups | PROCHLO | 2026-01-19 08:02 | 0.0139647 | 0.0136450 | -3.20e-04 | 5.1e-06 |
| Phytoplankton groups | PROCHLO | 2026-01-10 04:02 | 0.0267296 | 0.0706745 | 4.39e-02 | 8.2e-06 |
| Phytoplankton groups | PROCHLO | 2026-01-03 00:02 | 0.0064761 | 0.0067559 | 2.80e-04 | 1.0e-07 |
| Phytoplankton groups | PROCHLO | 2026-01-24 01:10 | 0.0185656 | 0.0190874 | 5.22e-04 | 1.5e-06 |
| Phytoplankton groups | PROCHLO | 2026-01-02 16:00 | 0.0151366 | 0.0152482 | 1.12e-04 | 9.0e-07 |
| Phytoplankton groups | PROKAR | 2026-01-19 08:02 | 0.0166462 | 0.0167435 | 9.72e-05 | 5.1e-06 |
| Phytoplankton groups | PROKAR | 2026-01-10 04:02 | 0.0335802 | 0.0355087 | 1.93e-03 | 8.2e-06 |
| Phytoplankton groups | PROKAR | 2026-01-03 00:02 | 0.0172708 | 0.0167262 | -5.45e-04 | 1.0e-07 |
| Phytoplankton groups | PROKAR | 2026-01-24 01:10 | 0.0179612 | 0.0178340 | -1.27e-04 | 1.5e-06 |
| Phytoplankton groups | PROKAR | 2026-01-02 16:00 | 0.0069800 | 0.0068998 | -8.01e-05 | 9.0e-07 |

### Values present in only one extraction

*max_deg_to_180*: the furthest of these points from the 180 degree
meridian; *half_cell_deg*: half the grid’s longitude step. A point
closer to 180 than half a cell is in the grid’s first or last column.

| test | variable | only_copernicopia | only_independent | max_deg_to_180 | half_cell_deg | all_within_half_cell_of_180 |
|:---|:---|---:|---:|---:|---:|:---|
| GLORYS12 physics | thetao | 11 | 0 | 0.037 | 0.042 | TRUE |
| GLORYS12 physics | so | 11 | 0 | 0.037 | 0.042 | TRUE |
| GLORYS12 physics | mlotst | 11 | 0 | 0.037 | 0.042 | TRUE |
| GLORYS12 physics | zos | 11 | 0 | 0.037 | 0.042 | TRUE |
| Zooplankton | zooc | 37 | 0 | 0.100 | 0.120 | TRUE |
| Sea ice concentration | ice_conc | 10 | 0 | 0.048 | 0.050 | TRUE |
| Sea ice edge | ice_edge | 16 | 0 | 0.049 | 0.050 | TRUE |

## Code

The code that made this report:

``` r
library(copernicopia)
#Python with copernicusmarine: RETICULATE_PYTHON if it is set (e.g. in .Renviron), otherwise the
#python parameter, either a conda environment's name or the path to its python executable.
#R often can't find conda itself on Windows, so a named environment is first looked for where
#Miniconda, Anaconda, Miniforge and conda usually keep them.
use_env <- function(x) {
  if (file.exists(x)) return(reticulate::use_python(x, required = TRUE))
  homes <- unique(c(Sys.getenv("USERPROFILE"), Sys.getenv("HOME"), path.expand("~"), Sys.getenv("ProgramData")))
  roots <- c("miniconda3", "anaconda3", "miniforge3", "mambaforge", ".conda")
  exe <- if (.Platform$OS.type == "windows") "python.exe" else file.path("bin", "python")
  found <- file.path(rep(homes[nzchar(homes)], each = length(roots)), roots, "envs", x, exe)
  found <- found[file.exists(found)]
  if (length(found)) reticulate::use_python(found[1], required = TRUE)
  else reticulate::use_condaenv(x, required = TRUE)
}
if (!nzchar(Sys.getenv("RETICULATE_PYTHON")) && nzchar(params$python)) use_env(params$python)

#Points: every tracking location in January 2026
xyt <- readRDS(params$points_file)
jan <- format(xyt$datetime, "%Y-%m", tz = "UTC") == "2026-01"
pts <- data.frame(rid = xyt$rid[jan], lon = xyt$lon[jan], lat = xyt$lat[jan], date = xyt$datetime[jan])
#The region of the independent download, and the region the wizard suggested for these points
band <- list(lon = c(-180, 180), lat = c(-80, -15))
area <- list(lon = c(76.5, -71.5), lat = c(-69.5, -33))

#Same datasets, variables and depths (surface, 0-1 m, for 3D products) as the independent download.
#prefix: the column prefix used in the independent extraction
tests <- list(
  sst_band = list(label = "SST (band, monthly file)", prefix = "sst", dataset = "METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2",
                  vars = c("analysed_sst", "analysis_error", "sea_ice_fraction"), region = band, chunk = "month"),
  sst_area = list(label = "SST (data's area, daily files)", prefix = "sst", dataset = "METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2",
                  vars = c("analysed_sst", "analysis_error", "sea_ice_fraction"), region = area, chunk = "day"),
  glorys   = list(label = "GLORYS12 physics", prefix = "glorys", dataset = "cmems_mod_glo_phy_my_0.083deg_P1D-m",
                  vars = c("thetao", "so", "mlotst", "zos"), region = area, chunk = "month"),
  chl      = list(label = "Chlorophyll-a (gap-free)", prefix = "chl", dataset = "cmems_obs-oc_glo_bgc-plankton_my_l4-gapfree-multi-4km_P1D",
                  vars = c("CHL", "CHL_uncertainty"), region = area, chunk = "month"),
  pft      = list(label = "Phytoplankton groups", prefix = "pft", dataset = "cmems_obs-oc_glo_bgc-plankton_my_l4-multi-4km_P1M",
                  vars = c("CHL", "DIATO", "DINO", "GREEN", "HAPTO", "MICRO", "NANO", "PICO", "PROCHLO", "PROKAR"),
                  region = area, chunk = "month"),
  zoo      = list(label = "Zooplankton", prefix = "zoo", dataset = "cmems_mod_glo_bgc-plankton_anfc_0.25deg_P1D-m",
                  vars = "zooc", region = area, chunk = "month"),
  wind     = list(label = "Wind", prefix = "wind", dataset = "cmems_obs-wind_glo_phy_nrt_l4_0.125deg_PT1H",
                  vars = c("eastward_wind", "northward_wind"), region = area, chunk = "month"),
  ssh      = list(label = "Sea level and geostrophic currents", prefix = "ssh",
                  dataset = "cmems_obs-sl_glo_phy-ssh_nrt_allsat-l4-duacs-0.125deg_P1D",
                  vars = c("adt", "sla", "ugos", "vgos", "ugosa", "vgosa"), region = area, chunk = "month"),
  sic      = list(label = "Sea ice concentration", prefix = "sic", dataset = "osisaf_obs-si_glo_phy-sic-south_nrt_amsr2_l4_P1D-m",
                  vars = "ice_conc", region = area, chunk = "month"),
  siedge   = list(label = "Sea ice edge", prefix = "siedge", dataset = "osisaf_obs-si_glo_phy-siedge_nrt_sh-P1D",
                  vars = "ice_edge", region = area, chunk = "month"))

reqs <- lapply(tests, function(p) suppressMessages(
  cmems_request(datasets = setNames(list(p$vars), p$dataset),
                lon = p$region$lon, lat = p$region$lat,
                time = c("2026-01-01", "2026-01-31"),
                chunk = p$chunk, out_dir = params$out_dir)))
#Files that are already downloaded are skipped, so run this chunk interactively first to see the downloads' progress
downloads <- lapply(reqs, cmems_download)
#Downloads that did not complete are reported, not compared
ok <- function(d) d$result %in% c("ok", "exists")
failed <- vapply(downloads, function(d) !all(ok(d)), logical(1))

#One row per variable: values present in both, present in only one, and how far apart
compare_var <- function(o, n) {
  if (is.null(n)) n <- rep(NA_real_, length(o))
  both <- !is.na(o) & !is.na(n)
  d <- n[both] - o[both]
  data.frame(values = sum(both),
             only_independent = sum(!is.na(o) & is.na(n)),
             only_copernicopia = sum(is.na(o) & !is.na(n)),
             differ = sum(abs(d) > 1e-6),
             max_abs_diff = if (any(both)) signif(max(abs(d)), 3) else NA_real_,
             r = if (sum(both) > 2 && stats::sd(o[both]) > 0) round(stats::cor(o[both], n[both]), 6) else NA_real_)}

merged <- list()
results <- do.call(rbind, lapply(names(tests)[!failed], function(k) {
  p <- tests[[k]]
  old <- as.data.frame(readRDS(file.path(params$old_dir, paste0(p$prefix, ".rds"))))
  old <- old[old$rid %in% pts$rid, c("rid", paste0(p$prefix, "_", p$vars)), drop = FALSE]
  new <- suppressMessages(cmems_extract(pts, reqs[[k]]))
  m <- merge(old, as.data.frame(new), by = "rid")
  merged[[k]] <<- m
  do.call(rbind, lapply(p$vars, function(v) cbind(test = p$label, variable = v,
                                                   compare_var(m[[paste0(p$prefix, "_", v)]], m[[v]]))))}))
```

### Independent extraction code

For reference, the code used for the independent extraction (not run
here; its results were saved and are read in above):

``` r
#Time keys: daily means on the UTC date, hourly fields on the hour, monthly composites on the month
tkey <- function(t, step) switch(step,
                                 hour = as.character(round(as.numeric(t)/3600)),
                                 day = format(t, "%Y-%m-%d", tz = "UTC"),
                                 month = format(t, "%Y-%m", tz = "UTC"))

#Extract one product at each location's cell and matching time step
extract_product <- function(pts, spec, prefix, lag_days = 0) {
  out <- data.table(rid = pts$rid)
  cols <- paste0(prefix, "_", spec$vars)
  for (cl in cols) set(out, j = cl, value = NA_real_)
  pkey  <- tkey(pts$t - lag_days * 86400, spec$step)
  files <- list.files(env_dir, pattern = spec$pattern, full.names = TRUE)
  for (f in files) {
    r1 <- rast(f, subds = spec$vars[1])
    lt <- layer_times(r1, f)
    stopifnot(length(lt) == nlyr(r1)) #one layer per time step
    lyr <- match(pkey, tkey(lt, spec$step))
    has <- which(!is.na(lyr))
    if (!length(has)) next
    cells <- cellFromXY(r1, cbind(pts$lon180[has], pts$lat[has]))
    use <- has[!is.na(cells)]; cells <- cells[!is.na(cells)] #outside the grid's extent stays NA
    if (!length(use)) next
    ucells <- unique(cells); ri <- match(cells, ucells)
    for (k in seq_along(spec$vars)) {
      r <- if (k == 1) r1 else rast(f, subds = spec$vars[k])
      vv <- as.matrix(r[ucells]) #values at each needed cell, all layers in the file
      set(out, i = use, j = cols[k], value = vv[cbind(ri, lyr[use])])}
  }
  out
}
```

## Software

    Run on 12 November 2026
    Copernicopia 1.1.0
    copernicusmarine 2.5.0
    R version 4.5.2 (2025-10-31 ucrt)
