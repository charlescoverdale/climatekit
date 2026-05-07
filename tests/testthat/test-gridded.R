# Tests for the gridded helpers (ck_apply_grid, ck_from_netcdf).
# Skip cleanly on systems without terra / ncdf4 in Suggests.

test_that("ck_apply_grid errors on non-SpatRaster input", {
  skip_if_not_installed("terra")
  expect_error(
    ck_apply_grid(matrix(1:9, 3, 3), ck_txx, as.Date("2024-01-01") + 0:8),
    "SpatRaster"
  )
})

test_that("ck_apply_grid runs ck_txx over a tiny synthetic grid", {
  skip_if_not_installed("terra")
  set.seed(1)
  dates <- seq(as.Date("1961-01-01"), as.Date("1961-12-31"), by = "day")
  n_layers <- length(dates)
  # 4-cell grid with realistic seasonal Tmax
  cells <- 4L
  vals <- matrix(
    rep(15 + 10 * sin(2 * pi * as.integer(format(dates, "%j")) / 365),
        each = cells) +
      rnorm(cells * n_layers),
    nrow = cells
  )
  r <- terra::rast(nrows = 2, ncols = 2, nlyrs = n_layers,
                   xmin = 0, xmax = 2, ymin = 0, ymax = 2)
  for (i in seq_len(n_layers)) {
    terra::values(r[[i]]) <- vals[, i]
  }

  out <- ck_apply_grid(r, ck_txx, dates = dates, period = "annual")
  expect_s4_class(out, "SpatRaster")
  expect_equal(terra::nlyr(out), 1L)
  expect_equal(terra::ncell(out), 4L)
  # Each cell's TXx should be near the seasonal max plus noise (~25 °C)
  vals_out <- as.numeric(terra::values(out))
  expect_true(all(vals_out > 20 & vals_out < 35))
})

test_that("ck_apply_grid layer names match output periods", {
  skip_if_not_installed("terra")
  dates <- seq(as.Date("1961-01-01"), as.Date("1962-12-31"), by = "day")
  n_layers <- length(dates)
  set.seed(2)
  vals <- matrix(rnorm(2 * n_layers, 15, 5), nrow = 2)
  r <- terra::rast(nrows = 1, ncols = 2, nlyrs = n_layers,
                   xmin = 0, xmax = 2, ymin = 0, ymax = 1)
  for (i in seq_len(n_layers)) {
    terra::values(r[[i]]) <- vals[, i]
  }
  out <- ck_apply_grid(r, ck_txx, dates = dates, period = "annual")
  expect_equal(terra::nlyr(out), 2L)
  expect_equal(names(out), c("1961-01-01", "1962-01-01"))
})

test_that("ck_apply_grid handles all-NA cells gracefully", {
  skip_if_not_installed("terra")
  dates <- seq(as.Date("1961-01-01"), as.Date("1961-12-31"), by = "day")
  n_layers <- length(dates)
  set.seed(3)
  vals <- matrix(rnorm(2 * n_layers, 15, 5), nrow = 2)
  vals[2, ] <- NA_real_  # second cell entirely NA
  r <- terra::rast(nrows = 1, ncols = 2, nlyrs = n_layers,
                   xmin = 0, xmax = 2, ymin = 0, ymax = 1)
  for (i in seq_len(n_layers)) {
    terra::values(r[[i]]) <- vals[, i]
  }
  out <- ck_apply_grid(r, ck_txx, dates = dates, period = "annual")
  vals_out <- as.numeric(terra::values(out))
  expect_false(is.na(vals_out[1]))
  expect_true(is.na(vals_out[2]))
})

test_that("ck_from_netcdf errors on missing file", {
  skip_if_not_installed("terra")
  expect_error(ck_from_netcdf("does_not_exist.nc"), "not found")
})

test_that("ck_from_netcdf rejects non-string path", {
  skip_if_not_installed("terra")
  expect_error(ck_from_netcdf(c("a", "b")), "single character")
})
