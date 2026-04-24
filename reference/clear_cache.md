# Clear Cache

Removes any cached reference data stored by `climatekit`.

## Usage

``` r
clear_cache()
```

## Value

Invisibly returns `TRUE` if cache was cleared, `FALSE` if no cache
existed.

## Examples

``` r
# \donttest{
op <- options(climatekit.cache_dir = tempdir())
clear_cache()
#> ✔ Cache cleared.
options(op)
# }
```
