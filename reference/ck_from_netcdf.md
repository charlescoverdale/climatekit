# Read a netCDF File as a SpatRaster

Convenience wrapper that delegates to
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html).
Reads the file at `path` and returns a SpatRaster, optionally restricted
to a single variable. terra and ncdf4 must be installed (both are listed
in `Suggests:`).

## Usage

``` r
ck_from_netcdf(path, var = NULL)
```

## Arguments

- path:

  Character. Path to a netCDF file.

- var:

  Character or `NULL`. Variable to extract. If `NULL`, the default
  behaviour of
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  applies.

## Value

A SpatRaster (one layer per time step in the netCDF file).

## Examples

``` r
if (FALSE) { # \dontrun{
  r <- ck_from_netcdf("tas_day.nc", var = "tas")
  terra::nlyr(r)  # number of daily layers
} # }
```
