# Annual or Monthly Minimum of Daily Minimum Temperature (TNn)

ETCCDI canonical index TNn. The minimum value of daily minimum
temperature (Tmin) within each reporting period (coldest night).

## Usage

``` r
ck_tnn(tmin, dates, period = "annual")
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
dates <- as.Date("2024-01-01") + 0:9
tmin <- c(-2, 3, -1, 5, -8, 0, 2, -12, 1, -1)
ck_tnn(tmin, dates)
#>       period value index unit
#> 1 2024-01-01   -12   tnn   °C
```
