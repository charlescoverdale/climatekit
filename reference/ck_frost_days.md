# Frost Days

Count the number of days where minimum temperature is below 0 degrees C.

## Usage

``` r
ck_frost_days(tmin, dates, period = "annual")
```

## Arguments

- tmin:

  Numeric vector of daily minimum temperatures (degrees C).

- dates:

  Date vector of the same length as `tmin`.

- period:

  Character. Aggregation period: `"annual"` (default) or `"monthly"`.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Examples

``` r
dates <- as.Date("2024-01-01") + 0:9
tmin <- c(-2, 3, -1, 5, -3, 0, 2, -4, 1, -1)
ck_frost_days(tmin, dates)
#>       period value      index unit
#> 1 2024-01-01     5 frost_days days
```
