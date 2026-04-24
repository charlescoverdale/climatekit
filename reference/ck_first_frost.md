# First Frost Date

Date of the first autumn frost (Tmin \< 0 degrees C) after July 1 in
each year.

## Usage

``` r
ck_first_frost(tmin, dates)
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
dates <- seq(as.Date("2024-07-01"), as.Date("2024-12-31"), by = "day")
set.seed(42)
tmin <- 15 - seq_along(dates) * 0.15 + rnorm(length(dates), sd = 3)
ck_first_frost(tmin, dates)
#>       period value       date       index        unit
#> 1 2024-01-01   241 2024-08-28 first_frost day of year
```
