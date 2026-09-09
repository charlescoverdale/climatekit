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
#> 5                  txx   temperature            °C
#> 6                  tnx   temperature            °C
#> 7                  txn   temperature            °C
#> 8                  tnn   temperature            °C
#> 9                tx10p   temperature             %
#> 10               tn10p   temperature             %
#> 11               tx90p   temperature             %
#> 12               tn90p   temperature             %
#> 13                wsdi   temperature          days
#> 14                csdi   temperature          days
#> 15       diurnal_range   temperature            °C
#> 16      growing_season   temperature          days
#> 17          warm_spell   temperature          days
#> 18                 hwn   temperature        events
#> 19                 hwf   temperature          days
#> 20                 hwd   temperature          days
#> 21                 hwm   temperature            °C
#> 22                 hwa   temperature            °C
#> 23                 cwn   temperature        events
#> 24                 cwf   temperature          days
#> 25                 cwd   temperature          days
#> 26                 cwm   temperature            °C
#> 27                 cwa   temperature            °C
#> 28                 ehf   temperature          °C^2
#> 29 heating_degree_days   temperature   degree-days
#> 30 cooling_degree_days   temperature   degree-days
#> 31 growing_degree_days   temperature   degree-days
#> 32        total_precip precipitation            mm
#> 33            dry_days precipitation          days
#> 34            wet_days precipitation          days
#> 35        heavy_precip precipitation          days
#> 36   very_heavy_precip precipitation          days
#> 37     max_1day_precip precipitation            mm
#> 38     max_5day_precip precipitation            mm
#> 39    precip_intensity precipitation        mm/day
#> 40                r95p precipitation            mm
#> 41                r99p precipitation            mm
#> 42                 spi       drought dimensionless
#> 43                spei       drought dimensionless
#> 44                 pet       drought            mm
#> 45              pet_pm       drought            mm
#> 46              huglin  agroclimatic   degree-days
#> 47             winkler  agroclimatic   degree-days
#> 48              branas  agroclimatic         mm·°C
#> 49         first_frost  agroclimatic   day of year
#> 50          last_frost  agroclimatic   day of year
#> 51          heat_index       comfort            °C
#> 52             humidex       comfort      unitless
#> 53          wind_chill       comfort            °C
#> 54         fire_danger       comfort      unitless
#>                                                                                                                    description
#> 1                                                                                               Count of days where Tmin < 0°C
#> 2                                                                                               Count of days where Tmax < 0°C
#> 3                                                                                              Count of days where Tmax > 25°C
#> 4                                                                                              Count of days where Tmin > 20°C
#> 5                                                                         Annual or monthly maximum of daily Tmax (ETCCDI TXx)
#> 6                                                          Annual or monthly maximum of daily Tmin (ETCCDI TNx, warmest night)
#> 7                                                            Annual or monthly minimum of daily Tmax (ETCCDI TXn, coldest day)
#> 8                                                          Annual or monthly minimum of daily Tmin (ETCCDI TNn, coldest night)
#> 9                                              Percentage of cool days (ETCCDI TX10p, Tmax below calendar-day 10th percentile)
#> 10                                           Percentage of cool nights (ETCCDI TN10p, Tmin below calendar-day 10th percentile)
#> 11                                             Percentage of warm days (ETCCDI TX90p, Tmax above calendar-day 90th percentile)
#> 12                                           Percentage of warm nights (ETCCDI TN90p, Tmin above calendar-day 90th percentile)
#> 13                                               Warm spell duration index (ETCCDI WSDI, 1961-1990 calendar-day Tmax 90p base)
#> 14                                               Cold spell duration index (ETCCDI CSDI, 1961-1990 calendar-day Tmin 10p base)
#> 15                                                                                  Mean daily temperature range (Tmax - Tmin)
#> 16                                                                 Growing season length (ETCCDI: 6-day spells of Tmean > 5°C)
#> 17                                                                   Warm spell days (simplified, see ck_wsdi for ETCCDI WSDI)
#> 18               Annual count of heatwave events (ET-SCI HWN; Tmax above the calendar-day 90th percentile for at least 3 days)
#> 19                                                                 Annual count of days contributing to heatwaves (ET-SCI HWF)
#> 20                                                                   Length of the longest heatwave in the period (ET-SCI HWD)
#> 21                                                                          Mean temperature across heatwave days (ET-SCI HWM)
#> 22                                                                       Peak temperature of the hottest heatwave (ET-SCI HWA)
#> 23              Annual count of cold-wave events (ET-SCI CWN; Tmin below the calendar-day 10th percentile for at least 3 days)
#> 24                                                                Annual count of days contributing to cold waves (ET-SCI CWF)
#> 25 Length of the longest cold wave in the period (ET-SCI CWD; not the ETCCDI consecutive wet days index, which is ck_wet_days)
#> 26                                                                         Mean temperature across cold-wave days (ET-SCI CWM)
#> 27                                                                    Lowest temperature of the coldest cold wave (ET-SCI CWA)
#> 28                                                   Excess Heat Factor, the Bureau of Meteorology operational heatwave metric
#> 29                                                                        Sum of (base - Tavg) for days below base temperature
#> 30                                                                        Sum of (Tavg - base) for days above base temperature
#> 31                                                                        Sum of (Tavg - base) for days above base temperature
#> 32                                                                                               Total precipitation by period
#> 33                                                                           Maximum consecutive dry days (precip < threshold)
#> 34                                                                          Maximum consecutive wet days (precip >= threshold)
#> 35                                                                               Count of days with precipitation >= threshold
#> 36                                                                               Count of days with precipitation >= threshold
#> 37                                                                                                 Maximum 1-day precipitation
#> 38                                                                                           Maximum 5-day precipitation total
#> 39                                                                                       Mean precipitation on wet days (SDII)
#> 40                        Annual total precipitation on days above 95th percentile of 1961-1990 wet-day baseline (ETCCDI R95p)
#> 41                        Annual total precipitation on days above 99th percentile of 1961-1990 wet-day baseline (ETCCDI R99p)
#> 42                                                                                            Standardised Precipitation Index
#> 43                                                                         Standardised Precipitation-Evapotranspiration Index
#> 44                                                                            Potential evapotranspiration (Hargreaves method)
#> 45                                                                Reference evapotranspiration (FAO-56 Penman-Monteith method)
#> 46                                                                                   Huglin heliothermal index for viticulture
#> 47                                                                        Winkler index (growing degree days for wine regions)
#> 48                                                                                                   Branas hydrothermal index
#> 49                                                                                     Date of first autumn frost (Tmin < 0°C)
#> 50                                                                                      Date of last spring frost (Tmin < 0°C)
#> 51                                                                                           Heat index (apparent temperature)
#> 52                                                                                                            Canadian humidex
#> 53                                                                                                      Wind chill temperature
#> 54                                                                                      Simplified fire danger proxy (not FWI)
```
