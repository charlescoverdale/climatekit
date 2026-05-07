# Percentage of Cool Days (TX10p)

ETCCDI canonical index TX10p. Percentage of days where daily Tmax falls
below the 10th percentile of the calendar-day distribution from a
reference period (default 1961 to 1990). The threshold is computed using
a 5-day window centred on each calendar day, pooled across the reference
period.

## Usage

``` r
ck_tx10p(tmax, dates, ref_start = 1961L, ref_end = 1990L, period = "annual")
```

## Arguments

- tmax:

  Numeric vector of daily maximum temperatures (degrees C).

- dates:

  Date vector of the same length as `tmax`. Must contain data covering
  the reference period.

- ref_start, ref_end:

  Integer. Reference period boundary years (inclusive). Defaults to 1961
  and 1990.

- period:

  Character. Aggregation period: `"annual"` (default) or `"monthly"`.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Details

This implementation does not apply the Zhang et al. (2005) in-base
bootstrap correction, so years inside the reference period have a small
self-inclusion bias. For climate-change attribution, restrict
interpretation to years outside the reference window.

## Examples

``` r
set.seed(1)
dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
tmax <- 15 + 10 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
        rnorm(length(dates))
tail(ck_tx10p(tmax, dates))
#>        period     value index unit
#> 26 1986-01-01  8.219178 tx10p    %
#> 27 1987-01-01  9.589041 tx10p    %
#> 28 1988-01-01  6.830601 tx10p    %
#> 29 1989-01-01  9.041096 tx10p    %
#> 30 1990-01-01  7.671233 tx10p    %
#> 31 1991-01-01 12.602740 tx10p    %
```
