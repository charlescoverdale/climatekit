# Canonical ETCCDI 27 Indices

Returns the 27 canonical Expert Team on Climate Change Detection and
Indices (ETCCDI) indices as documented by Alexander et al. (2006) and
Zhang et al. (2011), with each row showing the canonical short code,
full name, input variable, unit, definition, and the corresponding
`climatekit` function (or `NA` where the index is not yet implemented).

## Usage

``` r
ck_etccdi_27()
```

## Value

A data frame with one row per canonical ETCCDI index and columns `code`,
`name`, `variable`, `unit`, `definition`, `ck_function`, and `status`.

## Details

Use this table to audit coverage, locate the `ck_*` function for a given
ETCCDI code, or filter to indices that climatekit currently implements:
`subset(ck_etccdi_27(), !is.na(ck_function))`.

## References

Alexander, L. V. et al. (2006). Global observed changes in daily climate
extremes of temperature and precipitation. *Journal of Geophysical
Research: Atmospheres*, 111(D5).
[doi:10.1029/2005JD006290](https://doi.org/10.1029/2005JD006290) .

Zhang, X. et al. (2011). Indices for monitoring changes in extremes
based on daily temperature and precipitation data. *Wiley
Interdisciplinary Reviews: Climate Change*, 2(6), 851-870.
[doi:10.1002/wcc.147](https://doi.org/10.1002/wcc.147) .

## Examples

``` r
tab <- ck_etccdi_27()
head(tab)
#>   code       name variable unit
#> 1  TXx   Max Tmax     Tmax   °C
#> 2  TNx   Max Tmin     Tmin   °C
#> 3  TXn   Min Tmax     Tmax   °C
#> 4  TNn   Min Tmin     Tmin   °C
#> 5   FD Frost days     Tmin days
#> 6   ID   Ice days     Tmax days
#>                                                                  definition
#> 1                 Maximum value of daily maximum temperature in the period.
#> 2 Maximum value of daily minimum temperature in the period (warmest night).
#> 3   Minimum value of daily maximum temperature in the period (coldest day).
#> 4 Minimum value of daily minimum temperature in the period (coldest night).
#> 5                                     Annual count of days when Tmin < 0°C.
#> 6                                     Annual count of days when Tmax < 0°C.
#>     ck_function      status
#> 1        ck_txx implemented
#> 2        ck_tnx implemented
#> 3        ck_txn implemented
#> 4        ck_tnn implemented
#> 5 ck_frost_days implemented
#> 6   ck_ice_days implemented
# Indices currently implemented in climatekit:
subset(tab, !is.na(ck_function))[, c("code", "ck_function")]
#>       code          ck_function
#> 1      TXx               ck_txx
#> 2      TNx               ck_tnx
#> 3      TXn               ck_txn
#> 4      TNn               ck_tnn
#> 5       FD        ck_frost_days
#> 6       ID          ck_ice_days
#> 7       SU       ck_summer_days
#> 8       TR   ck_tropical_nights
#> 9    TX10p             ck_tx10p
#> 10   TN10p             ck_tn10p
#> 11   TX90p             ck_tx90p
#> 12   TN90p             ck_tn90p
#> 13     DTR     ck_diurnal_range
#> 14    WSDI              ck_wsdi
#> 15    CSDI              ck_csdi
#> 16     GSL    ck_growing_season
#> 17  RX1day   ck_max_1day_precip
#> 18  RX5day   ck_max_5day_precip
#> 19    SDII  ck_precip_intensity
#> 20   R10mm      ck_heavy_precip
#> 21   R20mm ck_very_heavy_precip
#> 22   Rnnmm      ck_heavy_precip
#> 23     CDD          ck_dry_days
#> 24     CWD          ck_wet_days
#> 25    R95p              ck_r95p
#> 26    R99p              ck_r99p
#> 27 PRCPTOT      ck_total_precip
# Coverage:
table(tab$status)
#> 
#> implemented 
#>          27 
```
