# Growing Degree Days

Sum of `(Tavg - base)` for all days where daily average temperature is
above the base temperature (default 10 degrees C).

## Usage

``` r
ck_growing_degree_days(tavg, dates, base = 10, period = "annual")
```

## Arguments

- tavg:

  Numeric vector of daily average temperatures (degrees C).

- dates:

  Date vector of the same length as `tavg`.

- base:

  Numeric. Base temperature in degrees C (default 10).

- period:

  Character. Aggregation period: `"annual"` (default) or `"monthly"`.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Examples

``` r
dates <- as.Date("2024-07-01") + 0:9
tavg <- c(15, 20, 8, 12, 25, 9, 30, 11, 22, 18)
ck_growing_degree_days(tavg, dates)
#>       period value               index        unit
#> 1 2024-01-01    73 growing_degree_days degree-days
```
