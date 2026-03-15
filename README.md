# climatekit

<!-- badges: start -->
<!-- badges: end -->

Unified climate indices for R. Compute 35+ standard climate indices from daily weather observations with a consistent, tidy interface.

## Installation

Install from GitHub:

```r
# install.packages("pak")
pak::pak("charlescoverdale/climatekit")
```

## Quick start

```r
library(climatekit)

# Daily temperature data
dates <- as.Date("2024-01-01") + 0:364
set.seed(42)
tmin <- sin(seq(0, 2 * pi, length.out = 365)) * 10 + 5
tmax <- tmin + runif(365, 5, 15)

# Frost days per year
ck_frost_days(tmin, dates)

# Monthly heating degree days
tavg <- (tmin + tmax) / 2
ck_heating_degree_days(tavg, dates, period = "monthly")

# Use the generic dispatcher
d <- data.frame(dates = dates, tmin = tmin, tmax = tmax)
ck_compute(d, "diurnal_range")
```

## Available indices

```r
ck_available()
```

| Category | Indices |
|----------|---------|
| Temperature | Frost days, ice days, summer days, tropical nights, growing season length, heating/cooling/growing degree days, diurnal range, warm spell duration |
| Precipitation | Consecutive dry/wet days, total precipitation, heavy/very heavy precipitation, max 1-day/5-day precipitation, precipitation intensity |
| Drought | SPI, SPEI, potential evapotranspiration (Hargreaves) |
| Agroclimatic | Huglin, Winkler, Branas, first/last frost |
| Comfort | Wind chill, heat index, humidex, fire weather index |

## Design

- **Vectors in, data frames out** — all functions take numeric vectors + dates and return tidy data frames
- **`ck_` prefix** — short, distinctive, avoids namespace collisions
- **No API calls** — pure computation on user-supplied data
- **No heavy dependencies** — only `cli`, `stats`, `tools`
- **Period aggregation** — most indices aggregate to annual or monthly

## Related packages

| Package | Description |
|---------|-------------|
| [readnoaa](https://github.com/charlescoverdale/readnoaa) | Download NOAA weather data (pairs with climatekit for analysis) |
| [ons](https://github.com/charlescoverdale/ons) | UK Office for National Statistics data |
| [boe](https://github.com/charlescoverdale/boe) | Bank of England data |

## License

MIT
