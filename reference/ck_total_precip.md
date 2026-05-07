# Total Wet-Day Precipitation (PRCPTOT)

Annual or monthly total precipitation in wet days, where a wet day is a
day with precipitation at or above `wet_day_threshold` mm (default 1 mm,
the ETCCDI standard). This is the canonical ETCCDI 'PRCPTOT' definition
(Alexander et al. 2006; Zhang et al. 2011).

## Usage

``` r
ck_total_precip(precip, dates, period = "annual", wet_day_threshold = 1)
```

## Arguments

- precip:

  Numeric vector of daily precipitation (mm).

- dates:

  Date vector of the same length as `precip`.

- period:

  Character. Aggregation period: `"annual"` (default) or `"monthly"`.

- wet_day_threshold:

  Numeric (mm). Days with precipitation strictly below this threshold
  are excluded from the sum. Default 1.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Details

Sub-threshold trace amounts are excluded. Pass `wet_day_threshold = 0`
to recover the previous behaviour of summing all daily values.

## Examples

``` r
dates <- as.Date("2024-01-01") + 0:9
precip <- c(0, 5, 3, 0, 8, 2, 0, 1, 4, 0)
ck_total_precip(precip, dates)
#>       period value        index unit
#> 1 2024-01-01    23 total_precip   mm
```
