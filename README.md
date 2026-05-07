# climatekit

[![CRAN status](https://www.r-pkg.org/badges/version/climatekit)](https://CRAN.R-project.org/package=climatekit) [![CRAN downloads](https://cranlogs.r-pkg.org/badges/climatekit)](https://CRAN.R-project.org/package=climatekit) [![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/climatekit)](https://CRAN.R-project.org/package=climatekit) [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) [![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

An R package for computing climate indices from daily weather observations. Takes vectors of temperature, precipitation, humidity, and wind data and returns tidy data frames: no file wrangling, no class coercion, no API calls.

Coverage in v0.2.0:

- **Full canonical ETCCDI 27** (Alexander et al. 2006; Zhang et al. 2011), with optional Zhang (2005) in-base bootstrap
- **ET-SCI heatwave / cold-wave family** (`HWN`, `HWF`, `HWD`, `HWM`, `HWA` and cold-wave duals)
- **EHF** (Excess Heat Factor, Nairn & Fawcett 2013) — Australian Bureau of Meteorology operational metric
- **SPI / SPEI** with multiple distributions (gamma / Pearson III; log-logistic / GEV)
- **FAO-56 Penman-Monteith** reference evapotranspiration alongside Hargreaves
- **Agroclimatic** (Huglin, Winkler, Branas, frost dates) with hemisphere awareness
- **Comfort** (heat index, humidex, wind chill, fire-danger proxy)
- **Discovery surfaces**: `ck_etccdi_27()` audit table, `ck_catalogue()` / `ck_browse()` filter
- **Gridded** support via `ck_apply_grid()` over a `terra::SpatRaster`

## What are climate indices?

Climate indices are standardised summary statistics that reduce daily weather observations into meaningful measures of climate conditions. A single year of weather data for one station is 365 rows of temperature, precipitation, wind, and humidity readings. Climate indices compress that into interpretable numbers: how many frost days occurred, how long the growing season lasted, whether the region is in drought.

These indices matter because they are how climate science connects with the real economy. Energy companies use heating and cooling degree days to forecast demand. Agricultural ministries track growing degree days and frost dates. Water authorities monitor SPI and SPEI drought indices. Urban planners measure heat index exceedances. Insurance actuaries count extreme precipitation events. Viticulturists use Huglin and Winkler indices to assess grape-growing potential. Fire services monitor fire weather indices.

The definitions come from international standards bodies - the WMO Expert Team on Climate Change Detection and Indices (ETCCDI) defines 27 core indices, the Expert Team on Sector-specific Climate Indices (ET-SCI) extends these into health, agriculture, and energy domains, and individual research communities have added domain-specific measures like SPEI for drought and Huglin for viticulture.

## Getting started: where to get the data

`climatekit` computes indices from weather data that you provide. It doesn't download anything itself: you bring the data, it does the maths.

**If you already have data** (a CSV, a database export, an Excel file), all you need is a numeric vector of observations and a date vector:

```r
library(climatekit)

# Read your own data
weather <- read.csv("my_weather_station.csv")

# Compute frost days
ck_frost_days(weather$tmin, weather$date)
```

**If you don't have data yet**, the easiest way to get started is with [`readnoaa`](https://github.com/charlescoverdale/readnoaa), which downloads free daily weather observations from NOAA's global archive of 100,000+ stations. No API key needed:

```r
install.packages("readnoaa")  # or devtools::install_github("charlescoverdale/readnoaa")
library(readnoaa)
library(climatekit)

# Step 1: Find a station near you
noaa_nearby(lat = 51.5, lon = -0.1, radius_km = 25)
#>        station                    name latitude longitude distance_km
#>   UKE00105915     LONDON WEATHER CENTRE   51.517    -0.117        1.4

# Step 2: Download daily data
weather <- noaa_daily("UKE00105915", "2020-01-01", "2024-12-31",
                      datatypes = c("TMAX", "TMIN", "PRCP"))

# Step 3: Compute indices
ck_frost_days(weather$tmin, weather$date, period = "annual")
ck_spi(weather$prcp, weather$date, scale = 3)
ck_heating_degree_days((weather$tmax + weather$tmin) / 2, weather$date)
```

As long as you have a numeric vector and a date vector, `climatekit` will work with it.

### Common data sources

| Region | Source | Coverage | Access |
|---|---|---|---|
| Global | [NOAA GHCNd](https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily) | 100,000+ stations worldwide | Free, no key. Use [`readnoaa`](https://github.com/charlescoverdale/readnoaa) |
| Global | [ERA5 reanalysis](https://cds.climate.copernicus.eu/) | Gridded, 0.25° resolution, 1940–present | Free, requires CDS account |
| UK | [Met Office MIDAS](https://catalogue.ceda.ac.uk/uuid/dbd451271eb04662beade68da43546e1) | ~1,000 UK stations, daily | Free via CEDA, requires registration |
| Europe | [ECA&D](https://www.ecad.eu/) | 20,000+ stations across Europe | Free download |
| US | [ACIS (RCC)](https://www.rcc-acis.org/) | All US cooperative & ASOS stations | Free, no key |
| Australia | [Bureau of Meteorology](https://www.bom.gov.au/climate/data/) | All BoM stations, daily | Free download |

---

## Why does this package exist?

R has the methods, but they are scattered across half a dozen packages with incompatible interfaces:

| Package | Coverage | Limitation |
|---|---|---|
| `ClimInd` | 138 indices (SPI, SPEI, heat/cold waves) | Returns raw vectors with no metadata, no dates, no units |
| `climdex.pcic` | 27 ETCCDI core indices | Requires a custom `climdexInput` S4 object; locked to ETCCDI standard |
| `SPEI` | SPI + SPEI drought indices | Single-purpose; only does drought |
| `heatwaveR` | Marine + atmospheric heatwaves | Single-purpose; only does heatwaves |
| `weathermetrics` | Unit conversions + heat index | No climate indices |

If you want frost days, degree days, SPI, and the Huglin index in the same analysis, you currently need four packages with four different input formats and four different output structures. One wants an S4 object, another wants a matrix, a third wants separate vectors, and none of them return a data frame with dates attached.

`climatekit` replaces all of that with a single interface: vectors in, data frames out. Every function takes the same kind of input (numeric vector + date vector), every function returns the same kind of output (a data frame with `period`, `value`, `index`, and `unit` columns), and the 50+ indices span temperature, precipitation, drought, agroclimatic, and comfort categories.

```r
# Without climatekit: four packages, four input formats, four output structures
library(climdex.pcic)
ci <- climdexInput.raw(tmax = ..., tmin = ..., prec = ..., ...)  # S4 object
fd <- climdex.fd(ci)  # returns named numeric vector, no dates

library(SPEI)
spi_result <- spi(ts(monthly_precip, frequency = 12), 3)  # returns S4, needs ts()

library(ClimInd)
gdd <- gdd(tavg_vector, 10)  # returns raw numeric, no metadata

# With climatekit: one package, one interface
library(climatekit)
ck_frost_days(tmin, dates)                     # → data.frame
ck_spi(precip, dates, scale = 3)               # → data.frame
ck_growing_degree_days(tavg, dates, base = 10)  # → data.frame
ck_huglin(tmin, tmax, dates, lat = 45)          # → data.frame
```

---

## Installation

```r
install.packages("climatekit")

# Or install the development version from GitHub
# install.packages("devtools")
devtools::install_github("charlescoverdale/climatekit")
```

---

## Functions

### ETCCDI canonical 27

The full ETCCDI core set (Alexander et al. 2006; Zhang et al. 2011) is implemented. `ck_etccdi_27()` returns an audit table mapping every code to its `climatekit` function.

| Code | Function | Description |
|---|---|---|
| FD | `ck_frost_days()` | Days where Tmin < 0 °C |
| ID | `ck_ice_days()` | Days where Tmax < 0 °C |
| SU | `ck_summer_days()` | Days where Tmax > 25 °C |
| TR | `ck_tropical_nights()` | Days where Tmin > 20 °C |
| TXx | `ck_txx()` | Annual / monthly max of Tmax |
| TNx | `ck_tnx()` | Annual / monthly max of Tmin (warmest night) |
| TXn | `ck_txn()` | Annual / monthly min of Tmax (coldest day) |
| TNn | `ck_tnn()` | Annual / monthly min of Tmin (coldest night) |
| DTR | `ck_diurnal_range()` | Mean daily temperature range |
| GSL | `ck_growing_season()` | Growing season length |
| TX10p | `ck_tx10p()` | % cool days (calendar-day base, optional Zhang 2005 bootstrap) |
| TN10p | `ck_tn10p()` | % cool nights (calendar-day base, optional bootstrap) |
| TX90p | `ck_tx90p()` | % warm days (calendar-day base, optional bootstrap) |
| TN90p | `ck_tn90p()` | % warm nights (calendar-day base, optional bootstrap) |
| WSDI | `ck_wsdi()` | Warm spell duration index |
| CSDI | `ck_csdi()` | Cold spell duration index |
| RX1day | `ck_max_1day_precip()` | Max 1-day precipitation |
| RX5day | `ck_max_5day_precip()` | Max 5-day precipitation |
| SDII | `ck_precip_intensity()` | Simple daily intensity index |
| R10mm | `ck_heavy_precip()` (default 10) | Days with precip >= 10 mm |
| R20mm | `ck_very_heavy_precip()` (default 20) | Days with precip >= 20 mm |
| Rnnmm | `ck_heavy_precip(threshold = nn)` | Days with precip >= nn mm |
| CDD | `ck_dry_days()` | Max consecutive dry days |
| CWD | `ck_wet_days()` | Max consecutive wet days |
| R95p | `ck_r95p()` | Total precip on very-wet days |
| R99p | `ck_r99p()` | Total precip on extremely-wet days |
| PRCPTOT | `ck_total_precip()` | Annual wet-day precip total |

### ET-SCI heatwave family

Period of >= 3 consecutive days with TX above the calendar-day 90th percentile, plus cold-wave duals (TN below 10th percentile).

| Code | Function | Description |
|---|---|---|
| HWN | `ck_hwn()` | Number of distinct heatwave events |
| HWF | `ck_hwf()` | Total days inside heatwave events |
| HWD | `ck_hwd()` | Longest heatwave duration |
| HWM | `ck_hwm(mode = "excess" / "absolute")` | Mean magnitude across event days |
| HWA | `ck_hwa(mode = "excess" / "absolute")` | Peak magnitude across event days |
| CWN | `ck_cwn()` | Cold-wave number |
| CWF | `ck_cwf()` | Cold-wave frequency |
| CWD | `ck_cwd()` | Cold-wave duration (note: ETCCDI also uses "CWD" for consecutive wet days, which is `ck_wet_days`) |
| CWM | `ck_cwm()` | Cold-wave magnitude |
| CWA | `ck_cwa()` | Cold-wave amplitude |
| EHF | `ck_ehf()` | Excess Heat Factor (Nairn & Fawcett 2013) |

### Drought, evapotranspiration

| Function | Description |
|---|---|
| `ck_spi(distribution = "gamma" / "pearsonIII")` | Standardized Precipitation Index |
| `ck_spei(distribution = "log-logistic" / "gev")` | Standardized Precipitation-Evapotranspiration Index |
| `ck_pet()` | Reference evapotranspiration (Hargreaves) |
| `ck_pet_pm()` | Reference evapotranspiration (FAO-56 Penman-Monteith) |

### Agroclimatic, comfort, energy

| Function | Description |
|---|---|
| `ck_huglin(lat)` | Huglin heliothermal index (viticulture) |
| `ck_winkler()` | Winkler index (wine region classification) |
| `ck_branas(lat)` | Branas hydrothermal index (disease pressure) |
| `ck_first_frost(lat)` | First autumn frost date (NH or SH) |
| `ck_last_frost(lat)` | Last spring frost date (NH or SH) |
| `ck_growing_degree_days()` | Accumulated GDD above base |
| `ck_heating_degree_days()` | Heating degree days |
| `ck_cooling_degree_days()` | Cooling degree days |
| `ck_warm_spell()` | Warm-spell days (series-quantile, simpler variant of WSDI) |
| `ck_wind_chill()` | Wind chill (Environment Canada / NWS) |
| `ck_heat_index()` | Heat index (Rothfusz / NWS) |
| `ck_humidex()` | Canadian humidex |
| `ck_fire_danger()` | Simplified fire-danger proxy (use `cffdrs` for full FWI) |

### Discovery, dispatch, gridded

| Function | Description |
|---|---|
| `ck_etccdi_27()` | Canonical ETCCDI 27 audit table |
| `ck_catalogue()` | Full implementation catalogue |
| `ck_browse(sector, standard, search)` | Filter the catalogue |
| `ck_compute(data, index, ...)` | Dispatch any index by name |
| `ck_available()`, `ck_metadata()` | Lightweight registry queries |
| `ck_convert_temp()` | Celsius / Fahrenheit / Kelvin |
| `ck_apply_grid(x, fun, dates, ...)` | Apply any function over a `terra::SpatRaster` |
| `ck_from_netcdf(path, var)` | Thin reader for netCDF input |
| `clear_cache()` | Clear the user-data cache |

---

## Examples

### How many frost days does a location get?

```r
library(climatekit)

# Daily minimum temperatures for a year
dates <- as.Date("2024-01-01") + 0:364
set.seed(42)
tmin <- sin(seq(0, 2 * pi, length.out = 365)) * 15 + 2

# Annual frost days
ck_frost_days(tmin, dates)
#>       period value      index unit
#>   2024-01-01   132 frost_days days

# Monthly breakdown
ck_frost_days(tmin, dates, period = "monthly")
#>       period value      index unit
#>   2024-01-01    25 frost_days days
#>   2024-02-01    17 frost_days days
#>   2024-03-01     4 frost_days days
#>   ...
```

---

### How much heating energy does a building need?

```r
# Heating degree days tell energy companies how much heating demand to expect.
# Each degree below the base temperature (default 18C) for each day adds to the total.

tavg <- sin(seq(0, 2 * pi, length.out = 365)) * 12 + 10
ck_heating_degree_days(tavg, dates, period = "monthly")
#>       period  value                index        unit
#>   2024-01-01 481.10 heating_degree_days degree-days
#>   2024-02-01 378.49 heating_degree_days degree-days
#>   2024-03-01 244.53 heating_degree_days degree-days
#>   ...

# Cooling degree days for air conditioning demand
ck_cooling_degree_days(tavg, dates, base = 22)
```

---

### Is a region in drought?

```r
# The Standardized Precipitation Index (SPI) fits a gamma distribution to
# monthly precipitation totals over a rolling window, then transforms to
# standard normal deviates. Values below -1 indicate moderate drought,
# below -1.5 severe drought, below -2 extreme drought.

dates_long <- seq(as.Date("2015-01-01"), as.Date("2024-12-31"), by = "day")
set.seed(42)
precip <- rgamma(length(dates_long), shape = 2, rate = 0.5)

spi <- ck_spi(precip, dates_long, scale = 3)
head(spi)
#>       period      value index        unit
#>   2015-03-01 -0.2891577   spi dimensionless
#>   2015-04-01  0.4458927   spi dimensionless
#>   ...

# SPEI adds evapotranspiration to capture temperature-driven drought
pet <- ck_pet(tmin, tmax, lat = 51.5, dates = dates)
```

---

### What wine regions does a climate support?

```r
# The Huglin heliothermal index classifies grape-growing potential:
# < 1500: too cool for viticulture
# 1500-1800: cool climate (Champagne, Mosel)
# 1800-2100: temperate (Burgundy, Oregon)
# 2100-2400: warm (Bordeaux, Napa)
# > 2400: hot (Barossa, Southern Spain)

dates_gs <- seq(as.Date("2024-04-01"), as.Date("2024-09-30"), by = "day")
set.seed(42)
tmin_gs <- rnorm(length(dates_gs), mean = 12, sd = 3)
tmax_gs <- tmin_gs + runif(length(dates_gs), 8, 15)

ck_huglin(tmin_gs, tmax_gs, dates_gs, lat = 45)
#>       period    value  index        unit
#>   2024-01-01 2129.284 huglin degree-days

# Winkler index (wine region classification)
tavg_gs <- (tmin_gs + tmax_gs) / 2
ck_winkler(tavg_gs, dates_gs)
```

---

### When did frost season start and end?

```r
# First and last frost dates matter for agriculture, construction, and transport.

dates_year <- as.Date("2024-01-01") + 0:364
set.seed(42)
tmin_year <- -10 + seq_along(dates_year) * 0.08 + rnorm(365, sd = 4)

ck_last_frost(tmin_year, dates_year)
#>       period value       date      index       unit
#>   2024-01-01   120 2024-04-29 last_frost day of year

ck_first_frost(tmin_year, dates_year)
```

---

### How dangerous is a heatwave?

```r
# The heat index combines temperature and humidity to estimate
# how hot it actually feels. Values above 40C are dangerous.

ck_heat_index(tavg = c(30, 33, 36, 39), humidity = c(60, 65, 70, 75))
#>      value      index unit
#>   32.94844 heat_index   °C
#>   38.67052 heat_index   °C
#>   47.57163 heat_index   °C
#>   60.56858 heat_index   °C

# Wind chill for cold conditions
ck_wind_chill(tavg = c(-5, -10, -15), wind_speed = c(20, 30, 40))

# Fire weather risk
ck_fire_danger(tavg = 35, humidity = 15, wind_speed = 30, precip = 0)
```

---

### Removing the in-base bias with the Zhang (2005) bootstrap

```r
# The percentile-day indices (TX10p, TN10p, TX90p, TN90p) compute
# thresholds from a reference period (default 1961-1990). For analysis
# years inside the reference period, the year being assessed contributes
# to its own threshold and biases the result toward 10% / 90%. Set
# bootstrap = TRUE to apply Zhang et al. (2005) leave-one-out resampling
# (the canonical climdex.pcic / climpact behaviour). Costs roughly
# N^2 percentile fits for an N-year reference; opt in for attribution
# work spanning the base.

ck_tx10p(tmax, dates, ref_start = 1961L, ref_end = 1990L, bootstrap = TRUE)
```

### Operational heatwave intensity (Excess Heat Factor)

```r
# EHF combines a 3-day mean temperature anomaly above the 95th
# percentile with an acclimatisation term. Positive EHF days are
# heatwave conditions; larger values indicate more severe events.

ck_ehf(tmax, tmin, dates, ref_start = 1961L, ref_end = 1990L,
       stat = "max")           # peak EHF in year
ck_ehf(tmax, tmin, dates, stat = "n_positive")    # count heatwave-condition days
ck_ehf(tmax, tmin, dates, stat = "sum_positive")  # severity-weighted total
```

### FAO-56 Penman-Monteith reference evapotranspiration

```r
# ck_pet() is the Hargreaves estimator (Tmin / Tmax / lat only).
# ck_pet_pm() is the international FAO-56 Penman-Monteith standard,
# with optional humidity, wind, solar-radiation, and elevation inputs.
# Sensible FAO-56 fallbacks are used where data are missing.

ck_pet_pm(tmin, tmax, lat = 45, dates = dates,
          elev = 200, wind = 2.5,
          rh_min = rh_min, rh_max = rh_max)
```

### Computing indices programmatically

```r
# If you are computing many indices over the same dataset, use ck_compute()
# with the index name as a string. This is useful in loops, Shiny apps,
# or any workflow where the index is selected at runtime.

weather <- data.frame(
  dates = as.Date("2024-01-01") + 0:364,
  tmin = sin(seq(0, 2 * pi, length.out = 365)) * 15 + 2,
  tmax = sin(seq(0, 2 * pi, length.out = 365)) * 15 + 12,
  precip = rgamma(365, shape = 0.5, rate = 0.2)
)

# Compute any index by name
ck_compute(weather, "frost_days")
ck_compute(weather, "total_precip", period = "monthly")

# See all available indices
ck_available()
#>                 index      category          unit
#>            frost_days   temperature          days
#>              ice_days   temperature          days
#>          summer_days    temperature          days
#>    tropical_nights     temperature          days
#>    ...
```

---

## Input / output contract

Every function follows the same pattern:

**Input:** Numeric vectors + a date vector. No special objects, no S4 classes, no preprocessing required.

```r
ck_frost_days(
  tmin = c(-2, 3, -1, 5, -3),
  dates = as.Date("2024-01-01") + 0:4
)
```

**Output:** A tidy data frame with consistent columns.

```r
# Period-aggregated indices return:
#>   period (Date) | value (numeric) | index (character) | unit (character)

# Daily indices (PET, wind chill, heat index) return:
#>   date (Date) | value (numeric) | index (character) | unit (character)
```

All outputs join cleanly on `period` or `date` columns, so you can compute multiple indices and merge them into a single analysis data frame.

---

## Design decisions

- **`ck_` prefix** - short, distinctive, won't collide with other packages. Easy to type and easy to autocomplete.
- **Vectors in, data frames out** - the simplest possible interface. No custom S4 objects to construct, no `ts()` coercion, no `zoo` or `xts` dependencies. If you have a column of temperatures and a column of dates, you can use this package.
- **No API calls** - this is a pure computation package. It does not download data. Pair it with [`readnoaa`](https://github.com/charlescoverdale/readnoaa) or any other data source. This separation keeps the package fast, testable, and CRAN-friendly.
- **No heavy dependencies** - depends only on `cli`, `stats`, and `tools`. No tidyverse, no Rcpp, no external system libraries.
- **Period aggregation** - most indices are naturally period-aggregated (e.g. "how many frost days this year?"). All aggregated functions accept `period = "annual"` (default) or `period = "monthly"`.
- **NA handling** - all functions handle missing values gracefully. NAs are excluded from counts and aggregations, matching the behaviour researchers expect.

---

## Related packages

| Package | Description |
|---|---|
| [`readnoaa`](https://github.com/charlescoverdale/readnoaa) | NOAA weather and climate data (pairs with climatekit for data acquisition) |
| [`carbondata`](https://github.com/charlescoverdale/carbondata) | Carbon market data (EU/UK ETS, voluntary registries) |
| [`cer`](https://github.com/charlescoverdale/cer) | Clean Energy Regulator data (Australia) |
| [`aemo`](https://github.com/charlescoverdale/aemo) | Australian Energy Market Operator data |

---

## Migrating from `climdex.pcic`

`climdex.pcic` (Pacific Climate Impacts Consortium) was for many years the standard R implementation of the canonical ETCCDI 27. It was archived from CRAN in 2023. `climatekit` covers the same set with a simpler interface:

```r
# climdex.pcic
ci <- climdexInput.raw(tmax, tmin, prec, ..., base.range = c(1961, 1990))
fd <- climdex.fd(ci)        # named numeric vector

# climatekit
fd <- ck_frost_days(tmin, dates)   # tidy data frame
```

See `vignette("climdex-migration", package = "climatekit")` for the full function-name crosswalk and interface-shift notes.

## Citation

```r
citation("climatekit")
```

If you use the package in academic work, please also cite Alexander et al. (2006) and Zhang et al. (2011) (the canonical ETCCDI references), and Zhang et al. (2005) if you use the in-base bootstrap. `inst/CITATION` and the root-level `CITATION.cff` provide the bibentries.

## Issues

Please report bugs or requests at <https://github.com/charlescoverdale/climatekit/issues>.

## Keywords

climate indices, ETCCDI, frost days, degree days, growing season, SPI, SPEI, drought, precipitation, heat index, wind chill, Huglin, Winkler, fire weather, agroclimatic, viticulture, climate change, weather data, R package
