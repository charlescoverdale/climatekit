# Huglin Heliothermal Index

The Huglin index is used in viticulture to characterise the thermal
potential of a region for grape growing. It is computed over the growing
season (April 1 to September 30 in the Northern Hemisphere; October 1 to
March 31 in the Southern Hemisphere).

## Usage

``` r
ck_huglin(tmin, tmax, dates, lat)
```

## Arguments

- tmin:

  Numeric vector of daily minimum temperatures (degrees C).

- tmax:

  Numeric vector of daily maximum temperatures (degrees C).

- dates:

  Date vector of the same length as `tmin`.

- lat:

  Numeric. Latitude in decimal degrees (used to determine hemisphere and
  day-length coefficient).

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## References

Huglin, P. (1978). Nouveau mode d'evaluation des possibilites
heliothermiques d'un milieu viticole. *Comptes Rendus de l'Academie
d'Agriculture de France*, 64, 1117-1126.

## Examples

``` r
dates <- seq(as.Date("2024-04-01"), as.Date("2024-09-30"), by = "day")
set.seed(42)
tmin <- rnorm(length(dates), mean = 12, sd = 3)
tmax <- tmin + runif(length(dates), 8, 15)
ck_huglin(tmin, tmax, dates, lat = 45)
#>       period    value  index        unit
#> 1 2024-01-01 1976.338 huglin degree-days
```
