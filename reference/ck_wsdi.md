# Warm Spell Duration Index (WSDI)

ETCCDI canonical index WSDI. Annual count of days in spans of at least
six consecutive days where daily Tmax exceeds the 90th percentile of the
calendar-day distribution from a reference period (default 1961 to
1990). The threshold is computed using a 5-day window centred on each
calendar day, pooled across the reference period.

## Usage

``` r
ck_wsdi(tmax, dates, ref_start = 1961L, ref_end = 1990L, min_spell = 6L)
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

- min_spell:

  Integer. Minimum spell length in days (default 6, the ETCCDI
  standard).

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Details

Days inside qualifying spells are counted into the year they fall in; a
spell that crosses a year boundary contributes to both years
proportionally. Annual aggregation only.

## Examples

``` r
set.seed(1)
dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
tmax <- 15 + 10 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
        rnorm(length(dates))
tail(ck_wsdi(tmax, dates))
#>        period value index unit
#> 26 1986-01-01     0  wsdi days
#> 27 1987-01-01     0  wsdi days
#> 28 1988-01-01     0  wsdi days
#> 29 1989-01-01     0  wsdi days
#> 30 1990-01-01     0  wsdi days
#> 31 1991-01-01     0  wsdi days
```
