# Warm Spell Days (Series-Quantile Approximation)

Count the number of days in warm spells, where a warm spell is defined
as at least six consecutive days with Tmax above the `threshold`
quantile of the full input series. This is a quick approximation driven
by a single series-wide quantile, and does not require a reference
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

## Details

For the canonical ETCCDI WSDI definition (1961-1990 calendar-day base,
six-day spell rule), use
[`ck_wsdi()`](https://charlescoverdale.github.io/climatekit/reference/ck_wsdi.md).

The quantile is estimated with the Hyndman and Fan type 8 estimator,
matching every other percentile in `climatekit`. Before v0.2.1 this
function alone used R's default type 7. The two estimators differ by
order 1/n, so counts rarely change on multi-year daily series.

## Examples

``` r
dates <- as.Date("2024-01-01") + 0:364
set.seed(42)
tmax <- rnorm(365, mean = 20, sd = 5)
ck_warm_spell(tmax, dates)
#>       period value      index unit
#> 1 2024-01-01     0 warm_spell days
```
