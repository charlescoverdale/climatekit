# Summer Days

Count the number of days where maximum temperature exceeds 25 degrees C.

## Usage

``` r
ck_summer_days(tmax, dates, period = "annual")
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
dates <- as.Date("2024-07-01") + 0:9
tmax <- c(22, 26, 28, 24, 30, 25, 27, 23, 31, 29)
ck_summer_days(tmax, dates)
#>       period value       index unit
#> 1 2024-01-01     6 summer_days days
```
