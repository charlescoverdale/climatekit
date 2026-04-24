# Heavy Precipitation Days

Count of days with precipitation at or above a threshold (default 10
mm).

## Usage

``` r
ck_heavy_precip(precip, dates, threshold = 10, period = "annual")
```

## Arguments

- precip:

  Numeric vector of daily precipitation (mm).

- dates:

  Date vector of the same length as `precip`.

- threshold:

  Numeric. Threshold in mm (default 10).

- period:

  Character. Aggregation period: `"annual"` (default) or `"monthly"`.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Examples

``` r
dates <- as.Date("2024-01-01") + 0:9
precip <- c(0, 5, 12, 0, 15, 2, 0, 11, 4, 0)
ck_heavy_precip(precip, dates)
#>       period value        index unit
#> 1 2024-01-01     3 heavy_precip days
```
