# Wind Chill Temperature

Compute wind chill using the North American Wind Chill Index formula
(Environment Canada / US NWS). Valid for temperatures at or below 10
degrees C and wind speeds above 4.8 km/h.

## Usage

``` r
ck_wind_chill(tavg, wind_speed)
```

## Arguments

- tavg:

  Numeric vector of temperatures (degrees C).

- wind_speed:

  Numeric vector of wind speeds (km/h).

## Value

A data frame with columns `value`, `index`, and `unit`.

## Examples

``` r
ck_wind_chill(tavg = c(-5, -10, 0), wind_speed = c(20, 30, 15))
#>        value      index unit
#> 1 -11.551404 wind_chill   °C
#> 2 -19.520498 wind_chill   °C
#> 3  -4.416183 wind_chill   °C
```
