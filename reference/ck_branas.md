# Branas Hydrothermal Index

The Branas index combines temperature and precipitation during the
growing season to estimate disease pressure (especially downy mildew) in
vineyards. It is the sum of the product of monthly mean temperature and
monthly precipitation for April-August.

## Usage

``` r
ck_branas(precip, tavg, dates)
```

## Arguments

- precip:

  Numeric vector of daily precipitation (mm).

- tavg:

  Numeric vector of daily mean temperatures (degrees C).

- dates:

  Date vector of the same length as `precip`.

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
