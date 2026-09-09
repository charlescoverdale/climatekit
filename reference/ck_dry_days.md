# Maximum Consecutive Dry Days

Maximum number of consecutive days with precipitation below a threshold.
This is the ETCCDI **CDD** index.

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

## Details

Note: `CDD` also denotes climpact's cooling degree days, which is
unrelated and is implemented in
[`ck_cooling_degree_days()`](https://charlescoverdale.github.io/climatekit/reference/ck_cooling_degree_days.md).
In
[`ck_compute()`](https://charlescoverdale.github.io/climatekit/reference/ck_compute.md)
the name `"cdd"` resolves here, to consecutive dry days;
`"consecutive_dry_days"` is accepted as an unambiguous alias.

## Examples

``` r
dates <- as.Date("2024-01-01") + 0:9
precip <- c(0, 0, 5, 0, 0, 0, 2, 0, 0, 0)
ck_dry_days(precip, dates)
#>       period value    index unit
#> 1 2024-01-01     3 dry_days days
```
