# Heat Index

Compute the heat index (apparent temperature) using the Rothfusz
regression equation used by the US National Weather Service.

## Usage

``` r
ck_heat_index(tavg, humidity)
```

## Arguments

- tavg:

  Numeric vector of temperatures (degrees C).

- humidity:

  Numeric vector of relative humidity (percent, 0-100).

## Value

A data frame with columns `value`, `index`, and `unit`.

## References

Rothfusz, L. P. (1990). The heat index equation. NWS Technical
Attachment SR 90-23.

## Examples

``` r
ck_heat_index(tavg = c(30, 35, 40), humidity = c(60, 70, 50))
#>      value      index unit
#> 1 32.83203 heat_index   °C
#> 2 50.34058 heat_index   °C
#> 3 54.76795 heat_index   °C
```
