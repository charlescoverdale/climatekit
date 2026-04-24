# Get Metadata for a Climate Index

Returns metadata (unit, category, description, reference) for a named
climate index.

## Usage

``` r
ck_metadata(index)
```

## Arguments

- index:

  Character string. The index name (e.g. `"frost_days"`). Use
  [`ck_available()`](https://charlescoverdale.github.io/climatekit/reference/ck_available.md)
  to see valid names.

## Value

A list with elements `index`, `category`, `unit`, `description`, and
`reference`.

## Examples

``` r
ck_metadata("frost_days")
#> $index
#> [1] "frost_days"
#> 
#> $category
#> [1] "temperature"
#> 
#> $unit
#> [1] "days"
#> 
#> $description
#> [1] "Count of days where Tmin < 0°C"
#> 
#> $reference
#> [1] "ETCCDI"
#> 
```
