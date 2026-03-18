# climatekit

[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) [![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

An R package for computing climate indices from daily weather observations. Takes vectors of temperature, precipitation, humidity, and wind data and returns tidy data frames - no file wrangling, no class coercion, no API calls.

## What are climate indices?

Climate indices are standardised summary statistics that reduce daily weather observations into meaningful measures of climate conditions. A single year of weather data for one station is 365 rows of temperature, precipitation, wind, and humidity readings. Climate indices compress that into interpretable numbers: how many frost days occurred, how long the growing season lasted, whether the region is in drought.

These indices matter because they are how climate science connects with the real economy. Energy companies use heating and cooling degree days to forecast demand. Agricultural ministries track growing degree days and frost dates. Water authorities monitor SPI and SPEI drought indices. Urban planners measure heat index exceedances. Insurance actuaries count extreme precipitation events. Viticulturists use Huglin and Winkler indices to assess grape-growing potential. Fire services monitor fire weather indices.

The definitions come from international standards bodies - the WMO Expert Team on Climate Change Detection and Indices (ETCCDI) defines 27 core indices, the Expert Team on Sector-specific Climate Indices (ET-SCI) extends these into health, agriculture, and energy domains, and individual research communities have added domain-specific measures like SPEI for drought and Huglin for viticulture.

## Where does the data come from?

**You supply the data.** This package does not download anything - it is a pure computation package. You give it vectors of daily temperatures, precipitation, or other weather variables, and it computes indices from them.

The data can come from anywhere: NOAA's Global Historical Climatology Network, ERA5 reanalysis, national weather services, your own station observations, or a CSV on your desktop. As long as you have a numeric vector and a date vector, `climatekit` will compute the index.

If you need to download weather data from within R, the [`readnoaa`](https://github.com/charlescoverdale/readnoaa) package provides daily station data from NOAA's Global Historical Climatology Network-daily (GHCNd) and pairs directly with `climatekit`:

```r
library(readnoaa)
library(climatekit)

# Download daily data for London Heathrow (2020-2024)
weather <- noaa_daily("UKE00105915", from = "2020-01-01", to = "2024-12-31")

# Compute frost days from the downloaded data
ck_frost_days(weather$tmin, weather$date, period = "annual")

# Compute drought index
ck_spi(weather$precip, weather$date, scale = 3)
```

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

`climatekit` replaces all of that with a single interface: vectors in, data frames out. Every function takes the same kind of input (numeric vector + date vector), every function returns the same kind of output (a data frame with `period`, `value`, `index`, and `unit` columns), and the 35 indices span temperature, precipitation, drought, agroclimatic, and comfort categories.

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
# install.packages("devtools")
devtools::install_github("charlescoverdale/climatekit")
```

---

## Functions

| Category | Function | Description |
|---|---|---|
| Temperature | `ck_frost_days()` | Days where Tmin < 0 degrees C |
| Temperature | `ck_ice_days()` | Days where Tmax < 0 degrees C |
| Temperature | `ck_summer_days()` | Days where Tmax > 25 degrees C |
| Temperature | `ck_tropical_nights()` | Days where Tmin > 20 degrees C |
| Temperature | `ck_growing_season()` | Growing season length (first to last 5-day spell > 5 degrees C) |
| Temperature | `ck_heating_degree_days()` | Sum of (base - Tavg) for days below base temperature |
| Temperature | `ck_cooling_degree_days()` | Sum of (Tavg - base) for days above base temperature |
| Temperature | `ck_growing_degree_days()` | Accumulated growing degree days above base |
| Temperature | `ck_diurnal_range()` | Mean daily temperature range (Tmax - Tmin) |
| Temperature | `ck_warm_spell()` | Warm spell duration index (spells >= 6 days above 90th percentile) |
| Precipitation | `ck_dry_days()` | Maximum consecutive dry days |
| Precipitation | `ck_wet_days()` | Maximum consecutive wet days |
| Precipitation | `ck_total_precip()` | Total precipitation by period |
| Precipitation | `ck_heavy_precip()` | Days with precipitation >= 10 mm |
| Precipitation | `ck_very_heavy_precip()` | Days with precipitation >= 20 mm |
| Precipitation | `ck_max_1day_precip()` | Maximum 1-day precipitation |
| Precipitation | `ck_max_5day_precip()` | Maximum 5-day precipitation total |
| Precipitation | `ck_precip_intensity()` | Mean precipitation on wet days (SDII) |
| Drought | `ck_spi()` | Standardized Precipitation Index |
| Drought | `ck_spei()` | Standardized Precipitation-Evapotranspiration Index |
| Drought | `ck_pet()` | Potential evapotranspiration (Hargreaves method) |
| Agroclimatic | `ck_huglin()` | Huglin heliothermal index (viticulture) |
| Agroclimatic | `ck_winkler()` | Winkler index (wine region classification) |
| Agroclimatic | `ck_branas()` | Branas hydrothermal index (disease pressure) |
| Agroclimatic | `ck_first_frost()` | Date of first autumn frost |
| Agroclimatic | `ck_last_frost()` | Date of last spring frost |
| Comfort | `ck_wind_chill()` | Wind chill temperature (Environment Canada / NWS) |
| Comfort | `ck_heat_index()` | Heat index (Rothfusz / NWS) |
| Comfort | `ck_humidex()` | Canadian humidex |
| Comfort | `ck_fire_weather()` | Simplified fire weather index |
| Infrastructure | `ck_compute()` | Generic dispatcher - pass index name as string |
| Infrastructure | `ck_available()` | List all available indices with descriptions |
| Infrastructure | `ck_metadata()` | Get metadata (units, reference, description) for an index |
| Infrastructure | `ck_convert_temp()` | Convert between Celsius, Fahrenheit, and Kelvin |
| Infrastructure | `clear_cache()` | Clear cached reference data |

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
ck_fire_weather(tavg = 35, humidity = 15, wind_speed = 30, precip = 0)
```

---

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

| Package | What it covers |
|---|---|
| [`readnoaa`](https://github.com/charlescoverdale/readnoaa) | NOAA weather and climate data (pairs with climatekit for data acquisition) |

---

## Issues

Please report bugs or requests at <https://github.com/charlescoverdale/climatekit/issues>.

## Keywords

climate indices, ETCCDI, frost days, degree days, growing season, SPI, SPEI, drought, precipitation, heat index, wind chill, Huglin, Winkler, fire weather, agroclimatic, viticulture, climate change, weather data, R package
