# Gridded / netCDF support.
#
# climatekit's core functions take vectors and return tidy data frames.
# These helpers let users apply any ck_* function over a SpatRaster
# (one time series per grid cell) or read a netCDF file as a
# SpatRaster. The hard work is delegated to terra and ncdf4, both in
# Suggests, so loading climatekit alone does not pull them in.

#' Read a netCDF File as a SpatRaster
#'
#' Convenience wrapper that delegates to [terra::rast()]. Reads the
#' file at `path` and returns a SpatRaster, optionally restricted to a
#' single variable. terra and ncdf4 must be installed (both are listed
#' in `Suggests:`).
#'
#' @param path Character. Path to a netCDF file.
#' @param var Character or `NULL`. Variable to extract. If `NULL`, the
#'   default behaviour of [terra::rast()] applies.
#'
#' @return A SpatRaster (one layer per time step in the netCDF file).
#'
#' @export
#' @examples
#' \dontrun{
#'   r <- ck_from_netcdf("tas_day.nc", var = "tas")
#'   terra::nlyr(r)  # number of daily layers
#' }
ck_from_netcdf <- function(path, var = NULL) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg terra} is required for {.fn ck_from_netcdf}.",
      "i" = "Install it with {.code install.packages(\"terra\")}."
    ))
  }
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    cli::cli_warn(c(
      "Package {.pkg ncdf4} is recommended for reading 'netCDF' files.",
      "i" = "{.pkg terra}'s GDAL bindings can read .nc on many systems, but",
      "i" = "if {.fn terra::rast} fails on your file, install it with",
      "i" = "{.code install.packages(\"ncdf4\")}."
    ))
  }
  if (!is.character(path) || length(path) != 1L) {
    cli::cli_abort("{.arg path} must be a single character string.")
  }
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}.")
  }
  if (is.null(var)) {
    terra::rast(path)
  } else {
    terra::rast(path, subds = var)
  }
}

#' Apply a climatekit Index Function Over a SpatRaster
#'
#' Compute a daily climatekit index function for every cell in a
#' SpatRaster `x` whose layers represent successive days, and return
#' the per-period results as a SpatRaster. The supplied function `fun`
#' must accept a numeric vector and a Date vector and return a data
#' frame with `period` and `value` columns (the standard climatekit
#' shape).
#'
#' All cells must share the same `dates`. Cells that are entirely `NA`
#' are returned as `NA`. Run-time scales linearly with the number of
#' cells; for very large grids consider sub-setting first.
#'
#' @param x A SpatRaster. Layers correspond one-to-one with `dates`.
#' @param fun A `ck_*` function (or any function with the same
#'   signature: numeric vector + Date vector + optional named
#'   arguments, returning a data frame with `period` and `value`).
#' @param dates Date vector of length `terra::nlyr(x)`.
#' @param ... Additional named arguments forwarded to `fun` (for
#'   example `period = "annual"`, `ref_start`, `ref_end`).
#'
#' @return A SpatRaster with one layer per output period (layer names
#'   are the period labels).
#'
#' @export
#' @examples
#' \donttest{
#'   if (requireNamespace("terra", quietly = TRUE)) {
#'     dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
#'     n <- length(dates)
#'     # Tiny 2x2 SpatRaster of synthetic daily Tmax
#'     r <- terra::rast(nrows = 2, ncols = 2, nlyrs = n,
#'                      xmin = 0, xmax = 2, ymin = 0, ymax = 2)
#'     set.seed(1)
#'     for (i in seq_len(n)) {
#'       terra::values(r[[i]]) <- rnorm(4, 15, 5)
#'     }
#'     txx_r <- ck_apply_grid(r, ck_txx, dates = dates, period = "annual")
#'     terra::nlyr(txx_r)
#'   }
#' }
ck_apply_grid <- function(x, fun, dates, ...) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg terra} is required for {.fn ck_apply_grid}.",
      "i" = "Install it with {.code install.packages(\"terra\")}."
    ))
  }
  if (!inherits(x, "SpatRaster")) {
    cli::cli_abort("{.arg x} must be a {.cls SpatRaster}.")
  }
  fun <- match.fun(fun)
  validate_dates(dates, terra::nlyr(x))

  # Probe one non-NA cell to learn the output period labels and length.
  ref_idx <- NA_integer_
  for (i in seq_len(terra::ncell(x))) {
    vals <- as.numeric(x[i])
    if (any(!is.na(vals))) {
      ref_idx <- i
      break
    }
  }
  if (is.na(ref_idx)) {
    cli::cli_abort("All cells in {.arg x} are NA.")
  }
  ref_res <- fun(as.numeric(x[ref_idx]), dates, ...)
  if (!is.data.frame(ref_res) ||
      !all(c("period", "value") %in% names(ref_res))) {
    cli::cli_abort(
      "{.arg fun} must return a data frame with {.field period} and {.field value} columns."
    )
  }
  n_periods <- nrow(ref_res)
  period_labels <- as.character(ref_res$period)

  cell_fun <- function(vals, ...) {
    if (all(is.na(vals))) {
      return(rep(NA_real_, n_periods))
    }
    res <- fun(vals, dates, ...)
    if (nrow(res) != n_periods) {
      return(rep(NA_real_, n_periods))
    }
    res$value
  }

  out <- terra::app(x, cell_fun, ...)
  names(out) <- period_labels
  out
}
