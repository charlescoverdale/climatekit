# Cold-Wave Amplitude (CWA)

ET-SCI cold-wave family index. `mode = "excess"` (default) returns the
peak (threshold - Tmin) across cold-wave days, expressed as a positive
magnitude. `mode = "absolute"` returns the minimum raw Tmin across
cold-wave days (the coldest event-day value). Returns `NA` for years
with no cold waves.

## Usage

``` r
ck_cwa(
  tmin,
  dates,
  ref_start = 1961L,
  ref_end = 1990L,
  min_spell = 3L,
  mode = c("excess", "absolute")
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

- min_spell:

  Integer. Minimum spell length in days (default 3, the ET-SCI
  standard).

- mode:

  One of `"excess"` (default) or `"absolute"`. See details.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Examples

``` r
set.seed(1)
dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
tmin <- 5 + 8 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
        rnorm(length(dates))
tail(ck_cwa(tmin, dates))
#>        period     value index unit
#> 26 1986-01-01        NA   cwa   °C
#> 27 1987-01-01        NA   cwa   °C
#> 28 1988-01-01 0.5085637   cwa   °C
#> 29 1989-01-01        NA   cwa   °C
#> 30 1990-01-01        NA   cwa   °C
#> 31 1991-01-01        NA   cwa   °C
```
