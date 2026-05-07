# First Frost Date

Day of year of the first autumn frost (Tmin \< 0 degrees C) in each
year. Hemisphere is selected by `lat`: in the Northern Hemisphere
(`lat >= 0`) the search starts at July 1 (DOY 183); in the Southern
Hemisphere it starts at March 1 (DOY 60), matching the autumn entry for
each.

## Usage

``` r
ck_first_frost(tmin, dates, lat = 50)
```

## Arguments

- tmin:

  Numeric vector of daily minimum temperatures (degrees C).

- dates:

  Date vector of the same length as `tmin`.

- lat:

  Numeric. Latitude in decimal degrees, used to select the hemisphere
  convention. Default 50 (Northern Hemisphere).

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
