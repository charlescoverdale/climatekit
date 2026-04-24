# Humidex

Compute the Canadian humidex from temperature and dewpoint.

## Usage

``` r
ck_humidex(tavg, dewpoint)
```

## Arguments

- tavg:

  Numeric vector of temperatures (degrees C).

- dewpoint:

  Numeric vector of dewpoint temperatures (degrees C).

## Value

A data frame with columns `value`, `index`, and `unit`.

## References

Masterson, J., & Richardson, F. A. (1979). Humidex: A method of
quantifying human discomfort due to excessive heat and humidity.
Environment Canada.

## Examples

``` r
ck_humidex(tavg = c(30, 35), dewpoint = c(20, 25))
#>      value   index     unit
#> 1 37.57053 humidex unitless
#> 2 47.33964 humidex unitless
```
