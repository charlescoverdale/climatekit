# Annual or Monthly Maximum of Daily Maximum Temperature (TXx)

ETCCDI canonical index TXx. The maximum value of daily maximum
temperature (Tmax) within each reporting period.

## Usage

``` r
ck_txx(tmax, dates, period = "annual")
```

## Arguments

- tmax:

  Numeric vector of daily maximum temperatures (degrees C).

- dates:

  Date vector of the same length as `tmax`.

- period:

  Character. Aggregation period: `"annual"` (default) or `"monthly"`.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Examples

``` r
dates <- as.Date("2024-01-01") + 0:9
tmax <- c(5, 10, 18, 12, 4, 8, 22, 3, 7, 6)
ck_txx(tmax, dates)
#>       period value index unit
#> 1 2024-01-01    22   txx   °C
```
