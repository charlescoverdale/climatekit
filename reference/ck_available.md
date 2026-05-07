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
#> 6                  txx   temperature            °C
#> 7                  tnx   temperature            °C
#> 8                  txn   temperature            °C
#> 9                  tnn   temperature            °C
#> 10               tx10p   temperature             %
#> 11               tn10p   temperature             %
#> 12               tx90p   temperature             %
#> 13               tn90p   temperature             %
#> 14 heating_degree_days   temperature   degree-days
#> 15 cooling_degree_days   temperature   degree-days
#> 16 growing_degree_days   temperature   degree-days
#> 17       diurnal_range   temperature            °C
#> 18          warm_spell   temperature          days
#> 19                wsdi   temperature          days
#> 20                csdi   temperature          days
#> 21            dry_days precipitation          days
#> 22            wet_days precipitation          days
#> 23        total_precip precipitation            mm
#> 24        heavy_precip precipitation          days
#> 25   very_heavy_precip precipitation          days
#> 26     max_1day_precip precipitation            mm
#> 27     max_5day_precip precipitation            mm
#> 28    precip_intensity precipitation        mm/day
#> 29                r95p precipitation            mm
#> 30                r99p precipitation            mm
#> 31                 spi       drought dimensionless
#> 32                spei       drought dimensionless
#> 33                 pet       drought            mm
#> 34              huglin  agroclimatic   degree-days
#> 35             winkler  agroclimatic   degree-days
#> 36              branas  agroclimatic         mm·°C
#> 37         first_frost  agroclimatic   day of year
#> 38          last_frost  agroclimatic   day of year
#> 39          wind_chill       comfort            °C
#> 40          heat_index       comfort            °C
#> 41             humidex       comfort      unitless
#> 42         fire_danger       comfort      unitless
#>                                                                                             description
#> 1                                                                        Count of days where Tmin < 0°C
#> 2                                                                        Count of days where Tmax < 0°C
#> 3                                                                       Count of days where Tmax > 25°C
#> 4                                                                       Count of days where Tmin > 20°C
#> 5                                           Growing season length (ETCCDI: 6-day spells of Tmean > 5°C)
#> 6                                                  Annual or monthly maximum of daily Tmax (ETCCDI TXx)
#> 7                                   Annual or monthly maximum of daily Tmin (ETCCDI TNx, warmest night)
#> 8                                     Annual or monthly minimum of daily Tmax (ETCCDI TXn, coldest day)
#> 9                                   Annual or monthly minimum of daily Tmin (ETCCDI TNn, coldest night)
#> 10                      Percentage of cool days (ETCCDI TX10p, Tmax below calendar-day 10th percentile)
#> 11                    Percentage of cool nights (ETCCDI TN10p, Tmin below calendar-day 10th percentile)
#> 12                      Percentage of warm days (ETCCDI TX90p, Tmax above calendar-day 90th percentile)
#> 13                    Percentage of warm nights (ETCCDI TN90p, Tmin above calendar-day 90th percentile)
#> 14                                                 Sum of (base - Tavg) for days below base temperature
#> 15                                                 Sum of (Tavg - base) for days above base temperature
#> 16                                                 Sum of (Tavg - base) for days above base temperature
#> 17                                                           Mean daily temperature range (Tmax - Tmin)
#> 18                                            Warm spell days (simplified, see ck_wsdi for ETCCDI WSDI)
#> 19                        Warm spell duration index (ETCCDI WSDI, 1961-1990 calendar-day Tmax 90p base)
#> 20                        Cold spell duration index (ETCCDI CSDI, 1961-1990 calendar-day Tmin 10p base)
#> 21                                                    Maximum consecutive dry days (precip < threshold)
#> 22                                                   Maximum consecutive wet days (precip >= threshold)
#> 23                                                                        Total precipitation by period
#> 24                                                        Count of days with precipitation >= threshold
#> 25                                                        Count of days with precipitation >= threshold
#> 26                                                                          Maximum 1-day precipitation
#> 27                                                                    Maximum 5-day precipitation total
#> 28                                                                Mean precipitation on wet days (SDII)
#> 29 Annual total precipitation on days above 95th percentile of 1961-1990 wet-day baseline (ETCCDI R95p)
#> 30 Annual total precipitation on days above 99th percentile of 1961-1990 wet-day baseline (ETCCDI R99p)
#> 31                                                                     Standardized Precipitation Index
#> 32                                                  Standardized Precipitation-Evapotranspiration Index
#> 33                                                     Potential evapotranspiration (Hargreaves method)
#> 34                                                            Huglin heliothermal index for viticulture
#> 35                                                 Winkler index (growing degree days for wine regions)
#> 36                                                                            Branas hydrothermal index
#> 37                                                              Date of first autumn frost (Tmin < 0°C)
#> 38                                                               Date of last spring frost (Tmin < 0°C)
#> 39                                                                               Wind chill temperature
#> 40                                                                    Heat index (apparent temperature)
#> 41                                                                                     Canadian humidex
#> 42                                                               Simplified fire danger proxy (not FWI)
```
