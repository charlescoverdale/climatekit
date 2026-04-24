# Diurnal Temperature Range

Mean daily temperature range (Tmax - Tmin) per period.

## Usage

``` r
ck_diurnal_range(tmin, tmax, dates, period = "annual")
```

## Arguments

- tmin:

  Numeric vector of daily minimum temperatures (degrees C).

- tmax:

  Numeric vector of daily maximum temperatures (degrees C).

- dates:

  Date vector of the same length as `tmin` and `tmax`.

- period:

  Character. Aggregation period: `"annual"` (default) or `"monthly"`.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Examples

``` r
dates <- as.Date("2024-01-01") + 0:9
tmin <- c(-2, 3, -1, 5, -3, 0, 2, -4, 1, -1)
tmax <- c(5, 10, 6, 12, 4, 8, 9, 3, 7, 6)
ck_diurnal_range(tmin, tmax, dates)
#>       period value         index unit
#> 1 2024-01-01     7 diurnal_range   °C
```
