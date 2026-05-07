# Cold-Wave Frequency (CWF)

ET-SCI cold-wave family index. Annual total number of days inside any
cold-wave event (see
[`ck_cwn()`](https://charlescoverdale.github.io/climatekit/reference/ck_cwn.md)
for the cold-wave definition).

## Usage

``` r
ck_cwf(tmin, dates, ref_start = 1961L, ref_end = 1990L, min_spell = 3L)
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
tail(ck_cwf(tmin, dates))
#>        period value index unit
#> 26 1986-01-01     0   cwf days
#> 27 1987-01-01     0   cwf days
#> 28 1988-01-01     3   cwf days
#> 29 1989-01-01     0   cwf days
#> 30 1990-01-01     0   cwf days
#> 31 1991-01-01     0   cwf days
```
