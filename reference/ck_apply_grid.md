# Apply a climatekit Index Function Over a SpatRaster

Compute a daily climatekit index function for every cell in a SpatRaster
`x` whose layers represent successive days, and return the per-period
results as a SpatRaster. The supplied function `fun` must accept a
numeric vector and a Date vector and return a data frame with `period`
and `value` columns (the standard climatekit shape).

## Usage

``` r
ck_apply_grid(x, fun, dates, ...)
```

## Arguments

- x:

  A SpatRaster. Layers correspond one-to-one with `dates`.

- fun:

  A `ck_*` function (or any function with the same signature: numeric
  vector + Date vector + optional named arguments, returning a data
  frame with `period` and `value`).

- dates:

  Date vector of length `terra::nlyr(x)`.

- ...:

  Additional named arguments forwarded to `fun` (for example
  `period = "annual"`, `ref_start`, `ref_end`).

## Value

A SpatRaster with one layer per output period (layer names are the
period labels).

## Details

All cells must share the same `dates`. Cells that are entirely `NA` are
returned as `NA`. Run-time scales linearly with the number of cells; for
very large grids consider sub-setting first.

## Examples

``` r
if (FALSE) { # \dontrun{
  r <- ck_from_netcdf("tasmax_day.nc", var = "tasmax")
  dates <- seq(as.Date("1961-01-01"), by = "day", length.out = terra::nlyr(r))
  txx_r <- ck_apply_grid(r, ck_txx, dates = dates, period = "annual")
  terra::plot(txx_r[[1]])
} # }
```
