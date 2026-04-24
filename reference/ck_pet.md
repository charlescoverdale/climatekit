# Potential Evapotranspiration (Hargreaves Method)

Estimate daily PET using the Hargreaves-Samani equation, which requires
only daily temperature extremes and latitude.

## Usage

``` r
ck_pet(tmin, tmax, lat, dates)
```

## Arguments

- tmin:

  Numeric vector of daily minimum temperatures (degrees C).

- tmax:

  Numeric vector of daily maximum temperatures (degrees C).

- lat:

  Numeric. Latitude in decimal degrees.

- dates:

  Date vector of the same length as `tmin`.

## Value

A data frame with columns `date`, `value`, `index`, and `unit`.

## References

Hargreaves, G. H., & Samani, Z. A. (1985). Reference crop
evapotranspiration from temperature. *Applied Engineering in
Agriculture*, 1(2), 96-99.

## Examples

``` r
dates <- as.Date("2024-07-01") + 0:9
tmin <- c(15, 16, 14, 17, 15, 13, 16, 14, 15, 16)
tmax <- c(30, 32, 28, 33, 31, 27, 34, 29, 30, 32)
ck_pet(tmin, tmax, lat = 45, dates = dates)
#>          date    value index unit
#> 1  2024-07-01 6.096361   pet   mm
#> 2  2024-07-02 6.522357   pet   mm
#> 3  2024-07-03 5.655463   pet   mm
#> 4  2024-07-04 6.659415   pet   mm
#> 5  2024-07-05 6.338231   pet   mm
#> 6  2024-07-06 5.483717   pet   mm
#> 7  2024-07-07 7.027908   pet   mm
#> 8  2024-07-08 5.879855   pet   mm
#> 9  2024-07-09 6.017510   pet   mm
#> 10 2024-07-10 6.432733   pet   mm
```
