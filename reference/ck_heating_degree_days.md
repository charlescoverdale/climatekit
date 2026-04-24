# Heating Degree Days

Sum of `(base - Tavg)` for all days where daily average temperature is
below the base temperature (default 18 degrees C).

## Usage

``` r
ck_heating_degree_days(tavg, dates, base = 18, period = "annual")
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
dates <- as.Date("2024-01-01") + 0:9
tavg <- c(5, 10, 15, 20, 8, 12, 18, 3, 25, 7)
ck_heating_degree_days(tavg, dates)
#>       period value               index        unit
#> 1 2024-01-01    66 heating_degree_days degree-days
```
