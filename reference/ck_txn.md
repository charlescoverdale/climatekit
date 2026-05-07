# Annual or Monthly Minimum of Daily Maximum Temperature (TXn)

ETCCDI canonical index TXn. The minimum value of daily maximum
temperature (Tmax) within each reporting period (coldest day).

## Usage

``` r
ck_txn(tmax, dates, period = "annual")
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
tmax <- c(5, 10, -3, 12, 4, 8, 22, -8, 7, 6)
ck_txn(tmax, dates)
#>       period value index unit
#> 1 2024-01-01    -8   txn   °C
```
