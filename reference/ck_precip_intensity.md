# Precipitation Intensity (SDII)

Mean precipitation on wet days (days with precipitation \>= 1 mm). Also
known as the Simple Daily Intensity Index.

## Usage

``` r
ck_precip_intensity(precip, dates, period = "annual")
```

## Arguments

- precip:

  Numeric vector of daily precipitation (mm).

- dates:

  Date vector of the same length as `precip`.

- period:

  Character. Aggregation period: `"annual"` (default) or `"monthly"`.

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Examples

``` r
dates <- as.Date("2024-01-01") + 0:9
precip <- c(0, 5, 12, 0, 15, 2, 0, 11, 4, 0)
ck_precip_intensity(precip, dates)
#>       period    value            index   unit
#> 1 2024-01-01 8.166667 precip_intensity mm/day
```
