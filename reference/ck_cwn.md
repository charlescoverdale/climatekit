# Cold-Wave Number (CWN)

ET-SCI cold-wave family index. Annual count of distinct cold-wave
events, where a cold wave is a span of at least three consecutive days
with daily Tmin below the 10th percentile of the calendar-day
distribution from a reference period (default 1961 to 1990).

## Usage

``` r
ck_cwn(tmin, dates, ref_start = 1961L, ref_end = 1990L, min_spell = 3L)
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

  Integer. Minimum spell length in days (default 3, the ET-SCI
  standard).

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Examples

``` r
set.seed(1)
dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
tmin <- 5 + 8 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
        rnorm(length(dates))
tail(ck_cwn(tmin, dates))
#>        period value index   unit
#> 26 1986-01-01     0   cwn events
#> 27 1987-01-01     0   cwn events
#> 28 1988-01-01     1   cwn events
#> 29 1989-01-01     0   cwn events
#> 30 1990-01-01     0   cwn events
#> 31 1991-01-01     0   cwn events
```
