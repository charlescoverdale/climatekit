# Cold Spell Duration Index (CSDI)

ETCCDI canonical index CSDI. Annual count of days in spans of at least
six consecutive days where daily Tmin falls below the 10th percentile of
the calendar-day distribution from a reference period (default 1961 to
1990). Cold-side counterpart to
[`ck_wsdi()`](https://charlescoverdale.github.io/climatekit/reference/ck_wsdi.md).

## Usage

``` r
ck_csdi(tmin, dates, ref_start = 1961L, ref_end = 1990L, min_spell = 6L)
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

- min_spell:

  Integer. Minimum spell length in days (default 6, the ETCCDI
  standard).

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Examples

``` r
set.seed(1)
dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
tmin <- 5 + 8 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
        rnorm(length(dates))
tail(ck_csdi(tmin, dates))
#>        period value index unit
#> 26 1986-01-01     0  csdi days
#> 27 1987-01-01     0  csdi days
#> 28 1988-01-01     0  csdi days
#> 29 1989-01-01     0  csdi days
#> 30 1990-01-01     0  csdi days
#> 31 1991-01-01     0  csdi days
```
