# Branas Hydrothermal Index

The Branas index combines temperature and precipitation during the
growing season to estimate disease pressure (especially downy mildew) in
vineyards. It is the sum of (monthly mean temperature) times (monthly
precipitation total) over the five months of the growing season:
April-August in the Northern Hemisphere; October-February in the
Southern Hemisphere. The Southern Hemisphere season spans two calendar
years and is reported under the year in which it starts.

## Usage

``` r
ck_branas(precip, tavg, dates, lat = 50)
```

## Arguments

- precip:

  Numeric vector of daily precipitation (mm).

- tavg:

  Numeric vector of daily mean temperatures (degrees C).

- dates:

  Date vector of the same length as `precip`.

- lat:

  Numeric. Latitude in decimal degrees, used to select the hemisphere
  convention. Default 50 (Northern Hemisphere).

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## References

Branas, J., Bernon, G., & Levadoux, L. (1946). Elements de viticulture
generale.

## Examples

``` r
dates <- seq(as.Date("2024-04-01"), as.Date("2024-08-31"), by = "day")
set.seed(42)
tavg <- rnorm(length(dates), mean = 12, sd = 3)
precip <- rgamma(length(dates), shape = 0.5, rate = 0.2)
ck_branas(precip, tavg, dates)
#>       period    value  index  unit
#> 1 2024-01-01 4246.801 branas mm·°C
```
