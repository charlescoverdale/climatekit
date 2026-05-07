# Last Frost Date

Day of year of the last spring frost (Tmin \< 0 degrees C) in each year.
Hemisphere is selected by `lat`: in the Northern Hemisphere the search
runs up to July 1 (DOY 183); in the Southern Hemisphere up to October 1
(DOY 274), matching the spring boundary for each.

## Usage

``` r
ck_last_frost(tmin, dates, lat = 50)
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
dates <- seq(as.Date("2024-01-01"), as.Date("2024-06-30"), by = "day")
set.seed(42)
tmin <- -10 + seq_along(dates) * 0.12 + rnorm(length(dates), sd = 3)
ck_last_frost(tmin, dates)
#>       period value       date      index        unit
#> 1 2024-01-01   128 2024-05-07 last_frost day of year
```
