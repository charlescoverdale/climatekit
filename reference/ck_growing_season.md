# Growing Season Length

Compute the growing season length following the ETCCDI definition: the
number of days between the first occurrence of at least 6 consecutive
days with daily mean temperature above 5 degrees C and the first span of
6 consecutive days with Tmean below 5 degrees C after July 1 (Northern
Hemisphere) or January 1 (Southern Hemisphere). Calculated per year.

## Usage

``` r
ck_growing_season(tavg, dates, lat = 50)
```

## Arguments

- tavg:

  Numeric vector of daily mean temperatures (degrees C).

- dates:

  Date vector of the same length as `tavg`.

- lat:

  Numeric. Latitude in decimal degrees (used to determine hemisphere for
  end-of-season rule). Default 50 (Northern Hemisphere).

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## Examples

``` r
dates <- as.Date("2024-01-01") + 0:364
set.seed(42)
tavg <- sin(seq(0, 2 * pi, length.out = 365)) * 15 + 5
ck_growing_season(tavg, dates)
#>       period value          index unit
#> 1 2024-01-01   182 growing_season days
```
