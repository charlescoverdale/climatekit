# Percentage of Warm Nights (TN90p)

ETCCDI canonical index TN90p. Percentage of days where daily Tmin
exceeds the 90th percentile of the calendar-day distribution from a
reference period (default 1961 to 1990). Computation follows the same
convention as
[`ck_tx10p()`](https://charlescoverdale.github.io/climatekit/reference/ck_tx10p.md)
and supports the same `bootstrap` argument.

## Usage

``` r
ck_tn90p(
  tmin,
  dates,
  ref_start = 1961L,
  ref_end = 1990L,
  period = "annual",
  bootstrap = FALSE
)
```

## Arguments

- tmin:

  Numeric vector of daily minimum temperatures (degrees C).

- dates:

  Date vector of the same length as `tmax`. Must contain data covering
  the reference period.

- ref_start, ref_end:

  Integer. Reference period boundary years (inclusive). Defaults to 1961
  and 1990.

- period:

  Character. Aggregation period: `"annual"` (default) or `"monthly"`.

- bootstrap:

  Logical. If `TRUE`, apply the Zhang (2005) in-base bootstrap
  correction. Default `FALSE` for backward compatibility and speed.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Examples

``` r
set.seed(1)
dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
tmin <- 5 + 8 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
        rnorm(length(dates))
tail(ck_tn90p(tmin, dates))
#>        period     value index unit
#> 26 1986-01-01 10.958904 tn90p    %
#> 27 1987-01-01  9.315068 tn90p    %
#> 28 1988-01-01  6.284153 tn90p    %
#> 29 1989-01-01 10.136986 tn90p    %
#> 30 1990-01-01  6.301370 tn90p    %
#> 31 1991-01-01 10.958904 tn90p    %
```
