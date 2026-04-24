# Last Frost Date

Date of the last spring frost (Tmin \< 0 degrees C) before July 1 in
each year.

## Usage

``` r
ck_last_frost(tmin, dates)
```

## Arguments

- tmin:

  Numeric vector of daily minimum temperatures (degrees C).

- dates:

  Date vector of the same length as `tmin`.

## Value

A data frame with columns `period`, `value` (day of year), `date` (the
frost date), `index`, and `unit`.

## Examples

``` r
dates <- seq(as.Date("2024-01-01"), as.Date("2024-06-30"), by = "day")
set.seed(42)
tmin <- -10 + seq_along(dates) * 0.12 + rnorm(length(dates), sd = 3)
ck_last_frost(tmin, dates)
#>       period value       date      index        unit
#> 1 2024-01-01   128 2024-05-07 last_frost day of year
```
