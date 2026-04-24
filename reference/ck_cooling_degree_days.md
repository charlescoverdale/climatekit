# Cooling Degree Days

Sum of `(Tavg - base)` for all days where daily average temperature is
above the base temperature (default 18 degrees C).

## Usage

``` r
ck_cooling_degree_days(tavg, dates, base = 18, period = "annual")
```

## Arguments

- tavg:

  Numeric vector of daily average temperatures (degrees C).

- dates:

  Date vector of the same length as `tavg`.

- base:

  Numeric. Base temperature in degrees C (default 18).

- period:

  Character. Aggregation period: `"annual"` (default) or `"monthly"`.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Examples

``` r
dates <- as.Date("2024-07-01") + 0:9
tavg <- c(25, 30, 22, 20, 28, 19, 32, 17, 35, 27)
ck_cooling_degree_days(tavg, dates)
#>       period value               index        unit
#> 1 2024-01-01    76 cooling_degree_days degree-days
```
