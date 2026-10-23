# Copernicopia

Download [Copernicus Marine](https://marine.copernicus.eu/) data from R and append it to your own data, one guided step at a time.

Copernicopia (Copernicus cornucopia) walks you through logging in, choosing variables from the full Copernicus Marine catalogue, picking a product for each, setting a region and time period, downloading the NetCDF files and adding the values to a data frame of locations and times. I built this (clarified/debugged/tested it with Claude, before testing it with some of the REC Laboratory (Monash University) members: Peter Allen) so that marine ecologists and biological oceanographers had an easier time dealing with satellite data. Many of us spend hours headbutting walls in this space... And I had already written a py script ([BMViola/Copernicus-Bulk-Files](https://github.com/BMViola/Copernicus-Bulk-Files))... So I figured we could work it in to a package and make it work for any dataset with xyt values. In the background it uses the official [`copernicusmarine`](https://pypi.org/project/copernicusmarine/) Python toolbox through [reticulate](https://rstudio.github.io/reticulate/).

## Citing Copernicopia

If Copernicopia helps your work, please cite it:

> Viola, B. (2026). *Copernicopia: Download Copernicus Marine Data and Append It to Your Own Data*. R package version 0.1.0. https://github.com/BMViola/copernicopia

In BibTeX:

```bibtex
@Manual{Copernicopia,
  title  = {Copernicopia: Download Copernicus Marine Data and Append It to Your Own Data},
  author = {Benjamin Viola},
  year   = {2026},
  note   = {R package version 0.1.0},
  url    = {https://github.com/BMViola/copernicopia},
}
```

You can also get this citation in R with `citation("copernicopia")`.

Please also cite the Copernicus Marine products you used; see [Citing the data](#citing-the-data).

## Installation

```r
# install.packages("remotes")
remotes::install_github("BMViola/copernicopia")
```

You also need a free [Copernicus Marine account](https://data.marine.copernicus.eu/register).

### Python

Copernicopia needs Python with `copernicusmarine` version 2 or later.

- **reticulate 1.41 or later:** there's nothing you need to do. The Python environment is set up automatically the first time it's needed.
- **Older reticulate:** run `copernicopia::cmems_install_python()` once, then restart R.
- **Existing conda environment** (for example one made with `conda create -n cmems -c conda-forge copernicusmarine`): run `reticulate::use_condaenv("cmems")` at the start of your R session, before any Copernicopia function.

## Quick start

```r
library(copernicopia)

obs <- data.frame(
  lon  = c(110.2, 140.5, 75.3),
  lat  = c(-50.1, -60.3, -45.8),
  date = as.Date(c("2025-07-15", "2025-08-02", "2025-09-20"))
)

obs <- cmems_wizard(obs)
```

The wizard asks you to:

| Step | What you do |
|------|-------------|
| **A** | Log in (first time only; your credentials are saved; the password will be blank as you enter it, but type it as you would and press enter) |
| **B** | Choose variables from a shortlist (SST, sea level, chlorophyll, sea ice...) or search every variable in the catalogue, using plain words or common abbreviations (see [Choosing variables](#choosing-variables)) |
| **C** | Pick a product for each variable from a numbered list. Type text to filter the list, e.g. `nrt`, `P1D` or `GLO` |
| **D** | Set the region's edges. It can cross the 180° meridian (see [Regions that cross 180°](#regions-that-cross-180)) |
| **E** | Set the time period, how to split the download into files (one per day, month or year, or a single file), and a depth range for 3D products |
| **F** | Check the download size, download, and append the values to your data |

When you pass your own data, the region and dates default to its extent and products that cover your dates are listed first. Press Enter to accept any default shown in `[brackets]`.

Run `cmems_wizard()` with no data to just download files.

A session looks something like this (product counts and listings depend on the current catalogue):

```
B. Choose variables
-------------------
Common variables:
[ 1] Sea surface temperature                          (34 products)
[ 2] Sea water temperature (with depth)               (28 products)
...
[ S] Search all variables by keyword
[ A] List every variable in the catalogue

Enter number(s) separated by commas, S to search or A to list all: 1, 14

C. Choose a product for each variable
-------------------------------------
Products with Sea surface temperature (34):
[1] METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2  [covers your dates]
     analysed_sst | daily | 2007-01-01 to 2026-10-20 | 0.05° 
     Global Ocean OSTIA Sea Surface Temperature and Sea Ice Analysis
...
Enter a number, or type text to filter the list (Enter shows all):
```

## Choosing variables

In step B, pick from the shortlist by number, or type `S` to search every variable in the catalogue. Search understands:

- **A shortlist entry's name**, in full or in part, such as `mixed layer depth`, `dissolved oxygen`, `sea level` or `carbonate system`. This finds the same variables as picking the entry by number.
- **The abbreviations below**, and the full names of the abbreviated variables, such as `sea level anomaly` or `significant wave height`.
- **Plain words**, matched against any part of a variable's name, such as `temperature`, `ice` or `thetao`.

Case, spaces and punctuation are ignored, so `chla`, `Chl-a` and `chl a` are the same. Type `A` to list every variable in the catalogue.

| # | Shortlist entry | Includes | Search with |
|---|---|---|---|
| 1 | Sea surface temperature | SST, including foundation, subskin and skin temperature | `sst` |
| 2 | Sea water temperature (with depth) | Temperature through the water column, including potential and conservative temperature | `temperature`, `thetao` |
| 3 | Salinity | Sea water and sea surface salinity, including practical and absolute salinity | `sss`, `salinity` |
| 4 | Sea surface height / sea level | Absolute dynamic topography and sea level anomaly | `ssh` (both), `adt`, `sla` |
| 5 | Mixed layer depth | Mixed layer thickness | `mld` |
| 6 | Ocean currents | Eastward and northward velocities, including surface and geostrophic currents | `current`, `currents`, `uv` |
| 7 | Chlorophyll-a | Any chlorophyll variable | `chla`, `chl-a`, `chlorophyll-a`, `chl` |
| 8 | Phytoplankton groups | Phytoplankton, diatoms, dinophytes, haptophytes, *Prochlorococcus*, prokaryotes, green algae | `phyto`, `diatom` |
| 9 | Zooplankton | Zooplankton | `zoo` |
| 10 | Primary production | Primary production or productivity | `pp`, `npp`, `nppv` |
| 11 | Nutrients (nitrate, phosphate, silicate, iron, ammonium) | Nitrate, phosphate, silicate, dissolved iron, ammonium | `nutrients`, `no3`, `po4`, `si`, `sio4`, `fe`, `nh4` |
| 12 | Dissolved oxygen | Oxygen in sea water | `do`, `o2`, `dox` |
| 13 | Carbonate system (pH, CO2, alkalinity) | pH, CO2 (partial pressure and air-sea flux), dissolved inorganic carbon, alkalinity | `ph`, `co2`, `pco2`, `spco2`, `dic`, `ta`, `talk` |
| 14 | Sea ice concentration | Sea ice area fraction | `sic` |
| 15 | Sea ice thickness | Sea ice thickness | `sit` |
| 16 | Sea ice edge / type | Sea ice classification, edge, type and extent | `ice` |
| 17 | Sea ice drift | Sea ice velocity, displacement and drift | `ice` |
| 18 | Wind and wind stress | Wind speed and components, and wind stress | `wind`, `u10`, `v10`, `wind stress`, `tau` |
| 19 | Waves | Wave height, period and direction | `wave`, `swh`, `hs` |
| 20 | Water clarity and optics | Light attenuation (e.g. Kd490), turbidity, Secchi depth, suspended matter, remote-sensing reflectance | `kd`, `kd490`, `spm`, `tsm`, `rrs`, `turbidity`, `secchi` |
| 21 | Bottom temperature | Temperature at the sea floor | `sbt`, `bottom temperature` |
| 22 | Photosynthetically available radiation (PAR) | Photosynthetically available radiation | `par` |
| 23 | Particulate organic carbon (POC) | Particulate organic carbon | `poc` |
| 24 | Surface heat flux | Net surface heat flux | `qnet`, `heat flux` |

An abbreviation also finds any variable whose short name is exactly that term, such as `o2` or `no3` in biogeochemistry models. Short abbreviations like `ph`, `do` and `pp` only find their own variables, so `ph` doesn't bring up phosphate or phytoplankton.

## Reproducible scripts

At the end, the wizard prints the code for the request it built, for example:

```r
req <- cmems_request(
  datasets = list(
    "METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2" = "analysed_sst",
    "cmems_mod_glo_phy_my_0.083deg_P1D-m" = c("thetao", "zos")
  ),
  lon = c(70, 150), lat = c(-65, -40),
  time = c("2025-07-01", "2025-09-30"),
  depth = c(0, 1),
  chunk = "month",
  out_dir = "cmems_data"
)
```

Put that in a script to rerun the same download without any prompts:

```r
cmems_download(req, dry_run = TRUE)   # check products and estimate size
cmems_download(req)                   # download
obs <- cmems_extract(obs, req)        # append values to your data
cmems_sources(obs)                    # which dataset each new column came from
```

## Regions that cross 180°

Longitudes are given as the region's western and eastern edges, `lon = c(west, east)`, and the region runs **east** from the first to the second. So a region can cross the 180° meridian:

| `lon` | Region |
|-------|--------|
| `c(170, -170)` | The 20° strip from 170°E east across 180° to 170°W |
| `c(170, 190)` | The same strip, written 0–360 style |
| `c(-170, 170)` | Everything except that strip (340° wide) |

The wizard asks for the western and eastern edges in the same way, and describes the region back to you, e.g. "170°E to 170°W, crossing 180° (20° wide)". If your data straddle 180°, its suggested region crosses 180° too. A very wide region that crosses 180° is usually two edges swapped by mistake, so the wizard asks you to confirm it, and `cmems_request()` prints a note.

Regions that cross 180° are downloaded as one continuous file, with longitudes running past 180 (e.g. 170 to 190). Values are appended to your data as usual, whichever way your longitudes are written.

## How values are matched

- **Space:** each row gets the value of the grid cell it falls in. Rows outside the downloaded region, or on land, get `NA`.
- **Time:** with the default `time_match = "period"`, daily data are matched on the calendar day (UTC), monthly data on the calendar month and yearly data on the year. Hourly and other resolutions use the nearest time step. Use `time_match = "nearest"` to always take the nearest time step. Rows outside the downloaded period get `NA`.
- **Depth:** for 3D products, rows are matched to the nearest depth level if you name a depth column (`depth = "my_depth_col"`), otherwise to the shallowest downloaded level.
- If a variable name is already a column in your data, the new column gets the dataset ID as a suffix.

Times can be `Date`, `POSIXct` or text like `"2025-07-15 13:00"`. `Date` and text values are read as UTC.

## Downloads

- Downloads are split into one file per month by default. Choose another split in the wizard, or with `chunk` in `cmems_request()`:

  | `chunk` | Files |
  |---------|-------|
  | `"month"` (default) | One per calendar month, e.g. `..._202507.nc` |
  | `"day"` | One per day, e.g. `..._20250715.nc` |
  | `"year"` | One per calendar year, e.g. `..._2025.nc` |
  | `"all"` | A single file for the whole period |

  Partial months or years at the ends of the period are named by their exact dates, e.g. `..._20250710-20250731.nc`. Products are never split more finely than their own time step: monthly data asked for by day are split by month, and yearly data by year.
- Products coarser than daily are downloaded for whole months (or years), so every date in your period has its value.
- The region is widened by one grid cell on each side, so observations right at its edge still get values.
- While downloading, each file gets a line as it starts and finishes, with the overall progress and an estimate of the time left:

  ```
  Downloading 6 file(s) for 2 product(s).
  [1/6] cmems_mod_glo_phy_my_0.083deg_P1D-m_202507.nc ... done (18.2 MB in 6s)  [17%, about 32s left]
  [2/6] cmems_mod_glo_phy_my_0.083deg_P1D-m_202508.nc ... downloading / 4s, 10.1 MB so far
  ```

  In interactive sessions, the line for the file in progress shows a running timer and the amount downloaded so far, updated twice a second, so you can see a large file is still coming in. Turn this off with `options(copernicopia.live_progress = FALSE)`.
- Files that already exist are skipped, so an interrupted download can simply be run again.
- Downloads run in a separate Python process, so they can always be cancelled cleanly (see below).
- Each file is written under a temporary name and renamed only when complete.
- Each product's files go in their own subfolder of `out_dir`, named after the dataset ID plus a short code for the variables, region, depth and file split, so different requests never mix. A `request.txt` in each subfolder records what it holds.
- Every attempt is logged to `cmems_download_log.csv` in `out_dir`.

## Citing the data

Every download records what you got from each product, and keeps a ready-to-use `README_sources.txt` in the download folder. To read it in R, or save a copy elsewhere:

```r
cmems_getreadme()                            # everything in "cmems_data"
cmems_getreadme(obs)                         # only the products behind obs
cmems_getreadme(file = "data_sources.txt")   # save it as a text file
```

It contains:

- **An acknowledgement** for your paper, with every product's DOI.
- **A reference for each product** in the format Copernicus Marine asks for, with the date you downloaded it:

  ```
  Global Ocean Physics Reanalysis. E.U. Copernicus Marine Service Information (CMEMS).
    Marine Data Store (MDS). DOI: 10.48670/moi-00021 (Accessed on 22 Oct 2026)
  ```
- **The details of each download:** product and dataset IDs, dataset version, producer, variables with their standard names and units, time step, grid resolution, region, depth range, period, number of files, download dates and software versions.

Copernicus Marine products are cited by their DOI; datasets do not have DOIs of their own, so the dataset ID is given in the details. See [how to cite Copernicus Marine products](https://help.marine.copernicus.eu/en/articles/4444611-how-to-cite-or-reference-copernicus-marine-products-and-services).

## Cancelling downloads

- **Press Esc** in the R session that is downloading. This stops R and closes the Python download process straight away.
- **If R is not responding**, open a new R session (or a terminal) and run:

  ```r
  copernicopia::cmems_abort()
  ```

  or, from a terminal, `Rscript -e "copernicopia::cmems_abort()"`. This cancels every running Copernicopia download on your computer. Each Python download process checks for the stop signal twice a second and closes itself; one that is too stuck to respond is force-stopped after 5 seconds.

Either way, files that finished downloading are kept and partly downloaded files are removed. Run `cmems_download()` again to resume where it stopped.

## Functions

| Function | Purpose |
|----------|---------|
| `cmems_wizard()` | The guided, step-by-step workflow |
| `cmems_login()` | Log in, or check saved credentials |
| `cmems_catalogue()` | The full catalogue as a data frame (cached for 30 days) |
| `cmems_variables()` | The shortlist of common variables used in step B |
| `cmems_request()` | Describe a download in code |
| `cmems_download()` | Download a request, or estimate its size with `dry_run = TRUE` |
| `cmems_abort()` | Cancel all running downloads, from any R session |
| `cmems_extract()` | Append values to a data frame |
| `cmems_sources()` | List where each appended column came from |
| `cmems_getreadme()` | Sources and citations for everything you downloaded |
| `cmems_install_python()` | Install the Python backend (older reticulate only) |

## Limitations

- Only gridded products on regular latitude/longitude grids are listed. In-situ (platform) data, static products such as bathymetry, and products only available on projected grids are not included.
- Download size estimates come from the toolbox and can be off for compressed NetCDF.
- If this works well, I will eventually try to include terrestrial products.

## Data acknowledgement

Data are provided by the Copernicus Marine Service. Run `cmems_getreadme()` for the acknowledgement and references for the products you downloaded.

## Author

Benjamin Viola, Institute for Marine and Antarctic Studies, University of Tasmania, and Monash University, Australia
