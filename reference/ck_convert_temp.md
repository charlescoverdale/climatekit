# Convert Temperature Units

Convert between Celsius, Fahrenheit, and Kelvin.

## Usage

``` r
ck_convert_temp(x, from, to)
```

## Arguments

- x:

  Numeric vector of temperatures.

- from:

  Character. Source unit: `"C"`, `"F"`, or `"K"`.

- to:

  Character. Target unit: `"C"`, `"F"`, or `"K"`.

## Value

Numeric vector of converted temperatures.

## Examples

``` r
ck_convert_temp(c(0, 100), from = "C", to = "F")
#> [1]  32 212
ck_convert_temp(32, from = "F", to = "C")
#> [1] 0
```
