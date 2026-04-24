# Maximum 1-Day Precipitation

Maximum precipitation recorded in a single day per period.

## Usage

``` r
ck_max_1day_precip(precip, dates, period = "annual")
```

## Arguments

- precip:

  Numeric vector of daily precipitation (mm).

- dates:

  Date vector of the same length as `precip`.

- period:

  Character. Aggregation period: `"annual"` (default) or `"monthly"`.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Examples

``` r
dates <- as.Date("2024-01-01") + 0:9
precip <- c(0, 5, 22, 0, 15, 25, 0, 11, 4, 30)
ck_max_1day_precip(precip, dates)
#>       period value           index unit
#> 1 2024-01-01    30 max_1day_precip   mm
```
