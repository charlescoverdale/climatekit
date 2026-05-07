# Annual or Monthly Maximum of Daily Minimum Temperature (TNx)

ETCCDI canonical index TNx. The maximum value of daily minimum
temperature (Tmin) within each reporting period (warmest night).

## Usage

``` r
ck_tnx(tmin, dates, period = "annual")
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
tmin <- c(15, 18, 22, 19, 14, 21, 23, 17, 20, 19)
ck_tnx(tmin, dates)
#>       period value index unit
#> 1 2024-01-01    23   tnx   °C
```
