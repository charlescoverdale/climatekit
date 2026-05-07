# Percentage of Cool Days (TX10p)

ETCCDI canonical index TX10p. Percentage of days where daily Tmax falls
below the 10th percentile of the calendar-day distribution from a
reference period (default 1961 to 1990). The threshold is computed using
a 5-day window centred on each calendar day, pooled across the reference
period.

## Usage

``` r
ck_tx10p(
  tmax,
  dates,
  ref_start = 1961L,
  ref_end = 1990L,
  period = "annual",
  bootstrap = FALSE
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

- period:

  Character. Aggregation period: `"annual"` (default) or `"monthly"`.

- bootstrap:

  Logical. If `TRUE`, apply the Zhang (2005) in-base bootstrap
  correction. Default `FALSE` for backward compatibility and speed.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Details

Set `bootstrap = TRUE` to apply the Zhang et al. (2005) in-base
leave-one-out bootstrap, which removes the self-inclusion bias for years
inside the reference period. The bootstrap is computationally expensive
(roughly N^2 percentile fits for an N-year reference) but is the
canonical climdex.pcic / climpact behaviour and is required for
climate-change attribution work that spans the base.

## References

Zhang, X., Hegerl, G. C., Zwiers, F. W., & Kenyon, J. (2005). Avoiding
inhomogeneity in percentile-based indices of temperature extremes.
*Journal of Climate*, 18(11), 1641-1651.
[doi:10.1175/JCLI3366.1](https://doi.org/10.1175/JCLI3366.1) .

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
