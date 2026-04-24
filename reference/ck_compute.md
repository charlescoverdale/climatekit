# Compute a Climate Index by Name

A generic dispatcher that calls the appropriate `ck_*` function based on
a string index name. Useful for programmatic workflows where the index
is selected at runtime.

## Usage

``` r
ck_compute(data, index, ...)
```

## Arguments

- data:

  A named list or data frame containing the required input vectors.
  Column names should match function argument names (e.g. `tmin`,
  `tmax`, `precip`, `dates`).

- index:

  Character. Name of the index to compute (e.g. `"frost_days"`). Use
  [`ck_available()`](https://charlescoverdale.github.io/climatekit/reference/ck_available.md)
  to see valid names.

- ...:

  Additional arguments passed to the underlying function (e.g. `period`,
  `threshold`, `base`).

## Value

A data frame as returned by the underlying `ck_*` function.

## Examples

``` r
d <- data.frame(
  dates = as.Date("2024-01-01") + 0:9,
  tmin = c(-2, 3, -1, 5, -3, 0, 2, -4, 1, -1)
)
ck_compute(d, "frost_days")
#>       period value      index unit
#> 1 2024-01-01     5 frost_days days
```
