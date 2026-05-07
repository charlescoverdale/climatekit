# Very Wet Days Total (R95p)

ETCCDI canonical index R95p. Annual total precipitation on days where
daily precipitation exceeds the 95th percentile of wet-day precipitation
in a reference period (default 1961 to 1990). A wet day is one with
precipitation at or above 1 mm. The threshold is a single value derived
from all wet days in the reference period (not calendar-day specific).

## Usage

``` r
ck_r95p(precip, dates, ref_start = 1961L, ref_end = 1990L, period = "annual")
```

## Arguments

- precip:

  Numeric vector of daily precipitation (mm).

- dates:

  Date vector of the same length as `precip`. Must contain data covering
  the reference period.

- ref_start, ref_end:

  Integer. Reference period boundary years (inclusive). Defaults to 1961
  and 1990.

- period:

  Character. Aggregation period: `"annual"` (default) or `"monthly"`.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Examples

``` r
set.seed(1)
dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
precip <- pmax(rgamma(length(dates), shape = 0.4, scale = 8) - 1, 0)
tail(ck_r95p(precip, dates))
#>        period    value index unit
#> 26 1986-01-01 109.5165  r95p   mm
#> 27 1987-01-01 208.6359  r95p   mm
#> 28 1988-01-01 192.0007  r95p   mm
#> 29 1989-01-01 130.8226  r95p   mm
#> 30 1990-01-01 186.8586  r95p   mm
#> 31 1991-01-01 168.1754  r95p   mm
```
