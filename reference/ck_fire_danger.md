# Fire Danger Index (Simplified)

A simplified fire danger proxy based on temperature, humidity, wind
speed, and recent precipitation. This is NOT the Canadian Forest Fire
Weather Index (Van Wagner 1987); for the full FWI system, use the cffdrs
package.

## Usage

``` r
ck_fire_danger(tavg, humidity, wind_speed, precip)
```

## Arguments

- tavg:

  Numeric vector of temperatures (degrees C).

- humidity:

  Numeric vector of relative humidity (percent, 0-100).

- wind_speed:

  Numeric vector of wind speeds (km/h).

- precip:

  Numeric vector of daily precipitation (mm).

## Value

A data frame with columns `value`, `index`, and `unit`.

## Examples

``` r
ck_fire_danger(
  tavg = c(30, 25, 35),
  humidity = c(20, 40, 15),
  wind_speed = c(25, 10, 30),
  precip = c(0, 5, 0)
)
#>       value       index     unit
#> 1 14.666667 fire_danger unitless
#> 2  3.333333 fire_danger unitless
#> 3 19.833333 fire_danger unitless
```
