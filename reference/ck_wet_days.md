# Maximum Consecutive Wet Days

Maximum number of consecutive days with precipitation at or above a
threshold. This is the ETCCDI **CWD** index.

## Usage

``` r
ck_wet_days(precip, dates, threshold = 1, period = "annual")
```

## Arguments

- precip:

  Numeric vector of daily precipitation (mm).

- dates:

  Date vector of the same length as `precip`.

- threshold:

  Numeric. Wet day threshold in mm (default 1).

- period:

  Character. Aggregation period: `"annual"` (default) or `"monthly"`.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Details

Note: the same letters CWD also denote the ET-SCI **cold-wave duration**
index, which is unrelated and is implemented in
[`ck_cwd()`](https://charlescoverdale.github.io/climatekit/reference/ck_cwd.md).
In
[`ck_compute()`](https://charlescoverdale.github.io/climatekit/reference/ck_compute.md)
the name `"cwd"` resolves to cold-wave duration, not to this function;
use `"wet_days"` or the unambiguous alias `"consecutive_wet_days"` for
consecutive wet days.

## Examples

``` r
dates <- as.Date("2024-01-01") + 0:9
precip <- c(5, 3, 0, 2, 8, 1, 0, 0, 4, 6)
ck_wet_days(precip, dates)
#>       period value    index unit
#> 1 2024-01-01     3 wet_days days
```
