# Clear Cache (Deprecated)

Deprecated in v0.2.1 and scheduled for removal in v0.4.0. `climatekit`
performs no I/O and never writes a cache, so this function has never had
anything to clear. It remains only so that existing scripts keep
running.

## Usage

``` r
clear_cache()
```

## Value

Invisibly returns `FALSE`. Prior to v0.2.1 this returned `TRUE` when a
directory set through `options(climatekit.cache_dir = ...)` contained
files, which only ever happened if something outside `climatekit` had
written there.

## Details

If you are caching downloaded weather data, clear it with the tools of
whichever data package fetched it (for example `readnoaa`), not here.

## Examples

``` r
# \donttest{
# Deprecated: this warns and does nothing.
suppressWarnings(clear_cache())
# }
```
