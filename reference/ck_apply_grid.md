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
# \donttest{
  if (requireNamespace("terra", quietly = TRUE)) {
    dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
    n <- length(dates)
    # Tiny 2x2 SpatRaster of synthetic daily Tmax
    r <- terra::rast(nrows = 2, ncols = 2, nlyrs = n,
                     xmin = 0, xmax = 2, ymin = 0, ymax = 2)
    set.seed(1)
    for (i in seq_len(n)) {
      terra::values(r[[i]]) <- rnorm(4, 15, 5)
    }
    txx_r <- ck_apply_grid(r, ck_txx, dates = dates, period = "annual")
    terra::nlyr(txx_r)
  }
#> [1] 1
# }
```
