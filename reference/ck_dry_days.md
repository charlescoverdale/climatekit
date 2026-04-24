# Maximum Consecutive Dry Days

Maximum number of consecutive days with precipitation below a threshold.

## Usage

``` r
ck_dry_days(precip, dates, threshold = 1, period = "annual")
```

## Arguments

- precip:

  Numeric vector of daily precipitation (mm).

- dates:

  Date vector of the same length as `precip`.

- threshold:

  Numeric. Dry day threshold in mm (default 1).

- period:

  Character. Aggregation period: `"annual"` (default) or `"monthly"`.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Examples

``` r
dates <- as.Date("2024-01-01") + 0:9
precip <- c(0, 0, 5, 0, 0, 0, 2, 0, 0, 0)
ck_dry_days(precip, dates)
#>       period value    index unit
#> 1 2024-01-01     3 dry_days days
```
