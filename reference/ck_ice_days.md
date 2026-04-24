# Ice Days

Count the number of days where maximum temperature is below 0 degrees C.

## Usage

``` r
ck_ice_days(tmax, dates, period = "annual")
```

## Arguments

- tmax:

  Numeric vector of daily maximum temperatures (degrees C).

- dates:

  Date vector of the same length as `tmax`.

- period:

  Character. Aggregation period: `"annual"` (default) or `"monthly"`.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Examples

``` r
dates <- as.Date("2024-01-01") + 0:9
tmax <- c(-2, 3, -1, 5, -3, 0, 2, -4, 1, -1)
ck_ice_days(tmax, dates)
#>       period value    index unit
#> 1 2024-01-01     5 ice_days days
```
