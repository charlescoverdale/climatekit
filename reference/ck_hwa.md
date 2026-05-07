# Heatwave Amplitude (HWA)

ET-SCI heatwave family index. Reports the peak magnitude of daily Tmax
across all heatwave days in the year. `mode = "excess"` (default) gives
the maximum of (Tmax - threshold). `mode = "absolute"` gives the maximum
raw Tmax across heatwave days (matching Perkins-Alexander 2013). Returns
`NA` for years with no heatwaves.

## Usage

``` r
ck_hwa(
  tmax,
  dates,
  ref_start = 1961L,
  ref_end = 1990L,
  min_spell = 3L,
  mode = c("excess", "absolute")
)
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
tmax <- 15 + 10 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
        rnorm(length(dates))
tail(ck_hwa(tmax, dates))
#>        period     value index unit
#> 26 1986-01-01        NA   hwa   °C
#> 27 1987-01-01 0.7421246   hwa   °C
#> 28 1988-01-01        NA   hwa   °C
#> 29 1989-01-01        NA   hwa   °C
#> 30 1990-01-01        NA   hwa   °C
#> 31 1991-01-01 0.7761449   hwa   °C
```
