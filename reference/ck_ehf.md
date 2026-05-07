# Excess Heat Factor (EHF, Nairn & Fawcett 2013)

Annual summary of the daily Excess Heat Factor heatwave intensity
metric. EHF combines a 3-day mean daily temperature anomaly above the
95th percentile of the reference period with an acclimatisation term
(3-day mean minus previous 30-day mean). Positive EHF days indicate
heatwave conditions; larger values indicate more severe or
less-acclimatised events. This is the operational heatwave metric used
by the Australian Bureau of Meteorology.

## Usage

``` r
ck_ehf(
  tmax,
  tmin,
  dates,
  ref_start = 1961L,
  ref_end = 1990L,
  stat = c("max", "n_positive", "sum_positive")
)
```

## Arguments

- tmax:

  Numeric vector of daily maximum temperatures (degrees C).

- tmin:

  Numeric vector of daily minimum temperatures (degrees C).

- dates:

  Date vector of the same length as `tmax`. Must contain data covering
  the reference period.

- ref_start, ref_end:

  Integer. Reference period boundary years (inclusive). Defaults to 1961
  and 1990.

- stat:

  One of `"max"`, `"n_positive"`, or `"sum_positive"`.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Details

Three annual summaries are exposed via the `stat` argument:

- `"max"` (default): peak EHF in the year. Strongest single-day
  intensity.

- `"n_positive"`: count of days with `EHF > 0`. A
  frequency-of-heatwave-conditions measure.

- `"sum_positive"`: sum of EHF on days with `EHF > 0`. A
  severity-weighted total.

## References

Nairn, J. R., & Fawcett, R. J. B. (2013). Defining heatwaves: heatwave
defined as a heat-impact event servicing all community and business
sectors in Australia. *CAWCR Technical Report No. 060*.

Perkins, S. E., & Alexander, L. V. (2013). On the measurement of
heatwaves. *Journal of Climate*, 26(13), 4500-4517.
[doi:10.1175/JCLI-D-12-00383.1](https://doi.org/10.1175/JCLI-D-12-00383.1)
.

## Examples

``` r
set.seed(1)
dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
s <- 2 * pi * as.integer(format(dates, "%j")) / 365
tmax <- 20 + 10 * sin(s) + rnorm(length(dates))
tmin <- 10 +  8 * sin(s) + rnorm(length(dates))
tail(ck_ehf(tmax, tmin, dates))
#>        period    value   index unit
#> 26 1986-01-01 3.351686 ehf_max °C^2
#> 27 1987-01-01 3.760517 ehf_max °C^2
#> 28 1988-01-01 3.311125 ehf_max °C^2
#> 29 1989-01-01 3.024947 ehf_max °C^2
#> 30 1990-01-01 3.067609 ehf_max °C^2
#> 31 1991-01-01 3.301835 ehf_max °C^2
```
