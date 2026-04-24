# Warm Spell Days

Count the number of days in warm spells, where a warm spell is defined
as at least 6 consecutive days with Tmax above the `threshold` quantile
of the full series. This computes warm spell days using a quantile
threshold from the input series. It does not implement the ETCCDI WSDI,
which requires calendar-day percentiles from a 1961-1990 reference
period.

## Usage

``` r
ck_warm_spell(tmax, dates, threshold = 0.9, period = "annual")
```

## Arguments

- tmax:

  Numeric vector of daily maximum temperatures (degrees C).

- dates:

  Date vector of the same length as `tmax`.

- threshold:

  Numeric. Quantile threshold (default 0.9, i.e. 90th percentile).

- period:

  Character. Aggregation period: `"annual"` (default) or `"monthly"`.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Examples

``` r
dates <- as.Date("2024-01-01") + 0:364
set.seed(42)
tmax <- rnorm(365, mean = 20, sd = 5)
ck_warm_spell(tmax, dates)
#>       period value      index unit
#> 1 2024-01-01     0 warm_spell days
```
