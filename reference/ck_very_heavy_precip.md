# Very Heavy Precipitation Days

Count of days with precipitation at or above a threshold (default 20
mm).

## Usage

``` r
ck_very_heavy_precip(precip, dates, threshold = 20, period = "annual")
```

## Arguments

- precip:

  Numeric vector of daily precipitation (mm).

- dates:

  Date vector of the same length as `precip`.

- threshold:

  Numeric. Threshold in mm (default 20).

- period:

  Character. Aggregation period: `"annual"` (default) or `"monthly"`.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Examples

``` r
dates <- as.Date("2024-01-01") + 0:9
precip <- c(0, 5, 22, 0, 15, 25, 0, 11, 4, 30)
ck_very_heavy_precip(precip, dates)
#>       period value             index unit
#> 1 2024-01-01     3 very_heavy_precip days
```
