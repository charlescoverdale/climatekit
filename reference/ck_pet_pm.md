# Reference Evapotranspiration (FAO-56 Penman-Monteith)

Compute reference evapotranspiration ETo using the FAO-56
Penman-Monteith equation (Allen et al. 1998), the international standard
for ETo estimation. Required inputs are daily Tmin and Tmax; optional
inputs (humidity, wind speed, incoming solar radiation, elevation)
increase accuracy. Where humidity, wind, or solar radiation are missing,
FAO-56 fallback estimators are used.

## Usage

``` r
ck_pet_pm(
  tmin,
  tmax,
  lat,
  dates,
  elev = 0,
  wind = 2,
  rh_min = NULL,
  rh_max = NULL,
  rs = NULL,
  albedo = 0.23,
  krs = 0.16
)
```

## Arguments

- tmin:

  Numeric vector of daily minimum temperatures (degrees C).

- tmax:

  Numeric vector of daily maximum temperatures (degrees C), same length
  as `tmin`.

- lat:

  Numeric. Latitude in decimal degrees.

- dates:

  Date vector of the same length as `tmin`.

- elev:

  Numeric. Elevation above sea level in metres (default 0).

- wind:

  Numeric vector or single value. 2-m wind speed (m/s). Default `2`
  (FAO-56 fallback for unmeasured wind).

- rh_min, rh_max:

  Optional numeric vectors of daily minimum and maximum relative
  humidity in percent. Both must be supplied together; otherwise vapour
  pressure falls back to `e0(tmin)`.

- rs:

  Optional numeric vector of daily incoming solar radiation (MJ m^-2
  day^-1). If `NULL`, Hargreaves-Samani estimate is used.

- albedo:

  Numeric. Surface albedo (default 0.23).

- krs:

  Numeric. Hargreaves-Samani coefficient for the Rs fallback (default
  0.16 for inland sites; 0.19 for coastal).

## Value

A data frame with columns `date`, `value`, `index` (`"pet_pm"`), and
`unit`.

## Details

Inputs and units:

- `tmin`, `tmax`: daily minimum and maximum temperature (degrees C).

- `lat`: latitude in decimal degrees, used for the
  extraterrestrial-radiation calculation.

- `elev`: elevation above sea level in metres (default 0).

- `wind`: 2-metre wind speed (m/s). Default 2 m/s (the FAO-56 fallback
  value when wind data are unavailable).

- `rh_min`, `rh_max`: minimum and maximum daily relative humidity (\\
  pressure is estimated as `e0(tmin)` (FAO-56 Eq. 48).

- `rs`: daily incoming solar radiation (MJ m^-2 day^-1). If NULL,
  estimated by Hargreaves-Samani: `Rs = krs * Ra * sqrt(Tmax - Tmin)`
  with `krs = 0.16` for inland sites.

- `albedo`: surface albedo (default 0.23, the FAO grass reference).

## References

Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop
evapotranspiration: guidelines for computing crop water requirements.
*FAO Irrigation and Drainage Paper 56*.

## Examples

``` r
dates <- as.Date("2024-07-01") + 0:9
tmin <- c(15, 16, 14, 17, 15, 13, 16, 14, 15, 16)
tmax <- c(30, 32, 28, 33, 31, 27, 34, 29, 30, 32)
ck_pet_pm(tmin, tmax, lat = 45, dates = dates)
#>          date    value  index unit
#> 1  2024-07-01 5.582353 pet_pm   mm
#> 2  2024-07-02 6.006386 pet_pm   mm
#> 3  2024-07-03 5.150850 pet_pm   mm
#> 4  2024-07-04 6.136760 pet_pm   mm
#> 5  2024-07-05 5.839817 pet_pm   mm
#> 6  2024-07-06 4.996958 pet_pm   mm
#> 7  2024-07-07 6.545339 pet_pm   mm
#> 8  2024-07-08 5.391620 pet_pm   mm
#> 9  2024-07-09 5.520137 pet_pm   mm
#> 10 2024-07-10 5.936037 pet_pm   mm
```
