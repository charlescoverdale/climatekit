# climatekit Index Catalogue

Returns the complete catalogue of climate indices implemented by
`climatekit`, with one row per `ck_*` function and columns covering the
canonical short code (where applicable), the full name, the index
family, the relevant sector, the unit, the source standard, and the
principal citation key.

## Usage

``` r
ck_catalogue()
```

## Value

A data frame with columns `ck_function`, `code`, `name`, `category`,
`sector`, `unit`, `standard`, and `citation_key`.

## Details

Use
[`ck_browse()`](https://charlescoverdale.github.io/climatekit/reference/ck_browse.md)
to filter by sector or standard.

## Examples

``` r
tab <- ck_catalogue()
head(tab)
#>          ck_function code                     name    category      sector unit
#> 1      ck_frost_days   FD               Frost days temperature agriculture days
#> 2        ck_ice_days   ID                 Ice days temperature        <NA> days
#> 3     ck_summer_days   SU              Summer days temperature      health days
#> 4 ck_tropical_nights   TR          Tropical nights temperature      health days
#> 5             ck_txx  TXx   Max Tmax (warmest day) temperature        <NA>   °C
#> 6             ck_tnx  TNx Max Tmin (warmest night) temperature        <NA>   °C
#>   standard        citation_key
#> 1   ETCCDI alexander2006global
#> 2   ETCCDI alexander2006global
#> 3   ETCCDI alexander2006global
#> 4   ETCCDI alexander2006global
#> 5   ETCCDI alexander2006global
#> 6   ETCCDI alexander2006global
# Tally indices by standard:
table(tab$standard)
#> 
#>        ET-SCI        ETCCDI ETCCDI-approx  agroclimatic       comfort 
#>            10            26             1             6             4 
#>       drought        energy 
#>             3             2 
```
