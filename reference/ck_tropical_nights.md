# Tropical Nights

Count the number of days where minimum temperature exceeds 20 degrees C.

## Usage

``` r
ck_tropical_nights(tmin, dates, period = "annual")
```

## Arguments

- tmin:

  Numeric vector of daily minimum temperatures (degrees C).

- dates:

  Date vector of the same length as `tmin`.

- period:

  Character. Aggregation period: `"annual"` (default) or `"monthly"`.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Examples

``` r
dates <- as.Date("2024-07-01") + 0:9
tmin <- c(18, 21, 22, 19, 25, 20, 23, 17, 24, 21)
ck_tropical_nights(tmin, dates)
#>       period value           index unit
#> 1 2024-01-01     6 tropical_nights days
```
