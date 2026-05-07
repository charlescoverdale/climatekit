# Browse the climatekit Index Catalogue

Filter the comprehensive climatekit index catalogue (see
[`ck_catalogue()`](https://charlescoverdale.github.io/climatekit/reference/ck_catalogue.md))
by sector, applicable standard (ETCCDI / ET-SCI / agroclimatic / comfort
/ drought / energy), or a free-text search across the function name,
full name, and ETCCDI code.

## Usage

``` r
ck_browse(sector = NULL, standard = NULL, search = NULL)
```

## Arguments

- sector:

  Character (length 1) or `NULL`. Filter to indices tagged with this
  sector. Common values: `"agriculture"`, `"health"`, `"water"`,
  `"energy"`. `NULL` (default) returns all rows.

- standard:

  Character (length 1) or `NULL`. Filter to indices under this standard.
  Common values: `"ETCCDI"`, `"ET-SCI"`, `"ETCCDI-approx"`,
  `"agroclimatic"`, `"comfort"`, `"drought"`, `"energy"`. `NULL`
  (default) returns all rows.

- search:

  Character (length 1) or `NULL`. Free-text search; rows are kept where
  the term appears (case-insensitive) in the function name, the full
  name, or the ETCCDI code.

## Value

The catalogue, filtered to matching rows. Same column structure as
[`ck_catalogue()`](https://charlescoverdale.github.io/climatekit/reference/ck_catalogue.md).

## Examples

``` r
ck_browse(standard = "ETCCDI")
#>             ck_function        code                          name      category
#> 1         ck_frost_days          FD                    Frost days   temperature
#> 2           ck_ice_days          ID                      Ice days   temperature
#> 3        ck_summer_days          SU                   Summer days   temperature
#> 4    ck_tropical_nights          TR               Tropical nights   temperature
#> 5                ck_txx         TXx        Max Tmax (warmest day)   temperature
#> 6                ck_tnx         TNx      Max Tmin (warmest night)   temperature
#> 7                ck_txn         TXn        Min Tmax (coldest day)   temperature
#> 8                ck_tnn         TNn      Min Tmin (coldest night)   temperature
#> 9              ck_tx10p       TX10p                     Cool days   temperature
#> 10             ck_tn10p       TN10p                   Cool nights   temperature
#> 11             ck_tx90p       TX90p                     Warm days   temperature
#> 12             ck_tn90p       TN90p                   Warm nights   temperature
#> 13              ck_wsdi        WSDI     Warm spell duration index   temperature
#> 14              ck_csdi        CSDI     Cold spell duration index   temperature
#> 15     ck_diurnal_range         DTR     Diurnal temperature range   temperature
#> 16    ck_growing_season         GSL         Growing season length   temperature
#> 17      ck_total_precip     PRCPTOT   Total wet-day precipitation precipitation
#> 18          ck_dry_days         CDD          Consecutive dry days precipitation
#> 19          ck_wet_days         CWD          Consecutive wet days precipitation
#> 20      ck_heavy_precip R10mm/Rnnmm      Heavy-precipitation days precipitation
#> 21 ck_very_heavy_precip       R20mm Very heavy precipitation days precipitation
#> 22   ck_max_1day_precip      RX1day       Max 1-day precipitation precipitation
#> 23   ck_max_5day_precip      RX5day       Max 5-day precipitation precipitation
#> 24  ck_precip_intensity        SDII        Simple daily intensity precipitation
#> 25              ck_r95p        R95p           Very wet days total precipitation
#> 26              ck_r99p        R99p      Extremely wet days total precipitation
#>         sector   unit standard        citation_key
#> 1  agriculture   days   ETCCDI alexander2006global
#> 2         <NA>   days   ETCCDI alexander2006global
#> 3       health   days   ETCCDI alexander2006global
#> 4       health   days   ETCCDI alexander2006global
#> 5         <NA>     °C   ETCCDI alexander2006global
#> 6         <NA>     °C   ETCCDI alexander2006global
#> 7         <NA>     °C   ETCCDI alexander2006global
#> 8         <NA>     °C   ETCCDI alexander2006global
#> 9       health      %   ETCCDI    zhang2011indices
#> 10      health      %   ETCCDI    zhang2011indices
#> 11      health      %   ETCCDI    zhang2011indices
#> 12      health      %   ETCCDI    zhang2011indices
#> 13      health   days   ETCCDI    zhang2011indices
#> 14      health   days   ETCCDI    zhang2011indices
#> 15        <NA>     °C   ETCCDI alexander2006global
#> 16 agriculture   days   ETCCDI alexander2006global
#> 17       water     mm   ETCCDI alexander2006global
#> 18       water   days   ETCCDI alexander2006global
#> 19       water   days   ETCCDI alexander2006global
#> 20       water   days   ETCCDI alexander2006global
#> 21       water   days   ETCCDI alexander2006global
#> 22       water     mm   ETCCDI alexander2006global
#> 23       water     mm   ETCCDI alexander2006global
#> 24       water mm/day   ETCCDI alexander2006global
#> 25       water     mm   ETCCDI    zhang2011indices
#> 26       water     mm   ETCCDI    zhang2011indices
ck_browse(sector = "agriculture")
#>              ck_function code                      name     category
#> 1          ck_frost_days   FD                Frost days  temperature
#> 2      ck_growing_season  GSL     Growing season length  temperature
#> 3 ck_growing_degree_days <NA>       Growing degree days  temperature
#> 4              ck_huglin <NA> Huglin heliothermal index agroclimatic
#> 5             ck_winkler <NA>             Winkler index agroclimatic
#> 6              ck_branas <NA> Branas hydrothermal index agroclimatic
#> 7         ck_first_frost <NA>   First autumn frost date agroclimatic
#> 8          ck_last_frost <NA>    Last spring frost date agroclimatic
#>        sector        unit     standard        citation_key
#> 1 agriculture        days       ETCCDI alexander2006global
#> 2 agriculture        days       ETCCDI alexander2006global
#> 3 agriculture degree-days agroclimatic                <NA>
#> 4 agriculture degree-days agroclimatic   huglin1978nouveau
#> 5 agriculture degree-days agroclimatic  winkler1974general
#> 6 agriculture       mm·°C agroclimatic                <NA>
#> 7 agriculture day of year agroclimatic                <NA>
#> 8 agriculture day of year agroclimatic                <NA>
ck_browse(search = "heat")
#>              ck_function code                                  name    category
#> 1                 ck_hwn  HWN                       Heatwave number temperature
#> 2                 ck_hwf  HWF                    Heatwave frequency temperature
#> 3                 ck_hwd  HWD                     Heatwave duration temperature
#> 4                 ck_hwm  HWM                    Heatwave magnitude temperature
#> 5                 ck_hwa  HWA                    Heatwave amplitude temperature
#> 6                 ck_ehf  EHF                    Excess Heat Factor temperature
#> 7 ck_heating_degree_days <NA>                   Heating degree days temperature
#> 8          ck_heat_index <NA> Heat index (NWS apparent temperature)     comfort
#>   sector        unit standard           citation_key
#> 1 health      events   ET-SCI perkins2013measurement
#> 2 health        days   ET-SCI perkins2013measurement
#> 3 health        days   ET-SCI perkins2013measurement
#> 4 health          °C   ET-SCI perkins2013measurement
#> 5 health          °C   ET-SCI perkins2013measurement
#> 6 health        °C^2   ET-SCI      nairn2013defining
#> 7 energy degree-days   energy                   <NA>
#> 8 health          °C  comfort       rothfusz1990heat
```
