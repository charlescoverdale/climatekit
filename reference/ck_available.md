# List All Available Climate Indices

Returns a data frame listing every index that `climatekit` can compute,
along with its category, unit, and a short description.

## Usage

``` r
ck_available()
```

## Value

A data frame with columns `index`, `category`, `unit`, and
`description`.

## Examples

``` r
ck_available()
#>                  index      category          unit
#> 1           frost_days   temperature          days
#> 2             ice_days   temperature          days
#> 3          summer_days   temperature          days
#> 4      tropical_nights   temperature          days
#> 5       growing_season   temperature          days
#> 6  heating_degree_days   temperature   degree-days
#> 7  cooling_degree_days   temperature   degree-days
#> 8  growing_degree_days   temperature   degree-days
#> 9        diurnal_range   temperature            °C
#> 10          warm_spell   temperature          days
#> 11            dry_days precipitation          days
#> 12            wet_days precipitation          days
#> 13        total_precip precipitation            mm
#> 14        heavy_precip precipitation          days
#> 15   very_heavy_precip precipitation          days
#> 16     max_1day_precip precipitation            mm
#> 17     max_5day_precip precipitation            mm
#> 18    precip_intensity precipitation        mm/day
#> 19                 spi       drought dimensionless
#> 20                spei       drought dimensionless
#> 21                 pet       drought            mm
#> 22              huglin  agroclimatic   degree-days
#> 23             winkler  agroclimatic   degree-days
#> 24              branas  agroclimatic         mm·°C
#> 25         first_frost  agroclimatic   day of year
#> 26          last_frost  agroclimatic   day of year
#> 27          wind_chill       comfort            °C
#> 28          heat_index       comfort            °C
#> 29             humidex       comfort      unitless
#> 30         fire_danger       comfort      unitless
#>                                                    description
#> 1                               Count of days where Tmin < 0°C
#> 2                               Count of days where Tmax < 0°C
#> 3                              Count of days where Tmax > 25°C
#> 4                              Count of days where Tmin > 20°C
#> 5  Growing season length (ETCCDI: 6-day spells of Tmean > 5°C)
#> 6         Sum of (base - Tavg) for days below base temperature
#> 7         Sum of (Tavg - base) for days above base temperature
#> 8         Sum of (Tavg - base) for days above base temperature
#> 9                   Mean daily temperature range (Tmax - Tmin)
#> 10               Warm spell days (simplified, not ETCCDI WSDI)
#> 11           Maximum consecutive dry days (precip < threshold)
#> 12          Maximum consecutive wet days (precip >= threshold)
#> 13                               Total precipitation by period
#> 14               Count of days with precipitation >= threshold
#> 15               Count of days with precipitation >= threshold
#> 16                                 Maximum 1-day precipitation
#> 17                           Maximum 5-day precipitation total
#> 18                       Mean precipitation on wet days (SDII)
#> 19                            Standardized Precipitation Index
#> 20         Standardized Precipitation-Evapotranspiration Index
#> 21            Potential evapotranspiration (Hargreaves method)
#> 22                   Huglin heliothermal index for viticulture
#> 23        Winkler index (growing degree days for wine regions)
#> 24                                   Branas hydrothermal index
#> 25                     Date of first autumn frost (Tmin < 0°C)
#> 26                      Date of last spring frost (Tmin < 0°C)
#> 27                                      Wind chill temperature
#> 28                           Heat index (apparent temperature)
#> 29                                            Canadian humidex
#> 30                      Simplified fire danger proxy (not FWI)
```
