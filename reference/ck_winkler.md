# Winkler Index

The Winkler index (also called growing degree days for viticulture)
accumulates daily mean temperature above 10 degrees C during the growing
season (April-October in NH, October-April in SH).

## Usage

``` r
ck_winkler(tavg, dates)
```

## Arguments

- tavg:

  Numeric vector of daily average temperatures (degrees C).

- dates:

  Date vector of the same length as `tavg`.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## References

Amerine, M. A., & Winkler, A. J. (1944). Composition and quality of
musts and wines of California grapes.

## Examples

``` r
dates <- seq(as.Date("2024-04-01"), as.Date("2024-10-31"), by = "day")
set.seed(42)
tavg <- rnorm(length(dates), mean = 18, sd = 4)
ck_winkler(tavg, dates)
#>       period    value   index        unit
#> 1 2024-01-01 1689.349 winkler degree-days
```
