test_that("ck_frost_days handles single observation", {
  result <- ck_frost_days(c(-1), as.Date("2024-01-01"))
  expect_equal(result$value, 1)
})

test_that("ck_frost_days with single non-frost day", {
  result <- ck_frost_days(c(5), as.Date("2024-01-01"))
  expect_equal(result$value, 0)
})

test_that("ck_frost_days handles all-NA input", {
  dates <- as.Date("2024-01-01") + 0:4
  result <- ck_frost_days(c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), dates)
  expect_s3_class(result, "data.frame")
})

test_that("ck_frost_days handles cross-year boundary", {
  dates <- as.Date("2024-12-30") + 0:3
  tmin <- c(-5, -3, -2, -1)
  result <- ck_frost_days(tmin, dates, period = "annual")
  # 2024 has 2 days, 2025 has 2 days
  expect_equal(nrow(result), 2)
  expect_equal(result$value, c(2, 2))
})

test_that("ck_frost_days exactly at 0C boundary", {
  dates <- as.Date("2024-01-01") + 0:2
  tmin <- c(-0.001, 0, 0.001)
  result <- ck_frost_days(tmin, dates)
  # Only -0.001 is < 0
  expect_equal(result$value, 1)
})

test_that("temperature convert round-trip", {
  expect_equal(ck_convert_temp(ck_convert_temp(42, "C", "F"), "F", "C"),
               42, tolerance = 1e-10)
})

test_that("ck_dry_days with empty-like input (all wet)", {
  dates <- as.Date("2024-01-01") + 0:4
  precip <- c(5, 10, 15, 20, 25)
  result <- ck_dry_days(precip, dates)
  expect_equal(result$value, 0)
})

test_that("ck_total_precip handles all-zero", {
  dates <- as.Date("2024-01-01") + 0:4
  precip <- rep(0, 5)
  result <- ck_total_precip(precip, dates)
  expect_equal(result$value, 0)
})

test_that("ck_growing_season handles short data", {
  dates <- as.Date("2024-01-01") + 0:4
  tavg <- rep(20, 5)
  result <- ck_growing_season(tavg, dates)
  expect_true(is.na(result$value))  # < 6 days
})

test_that("ck_heating_degree_days single day", {
  result <- ck_heating_degree_days(c(10), as.Date("2024-01-01"), base = 18)
  expect_equal(result$value, 8)
})

test_that("ck_wind_chill handles NA in input", {
  result <- ck_wind_chill(tavg = c(-5, NA), wind_speed = c(20, 20))
  expect_true(is.na(result$value[2]))
})

test_that("ck_heat_index handles NA in input", {
  result <- ck_heat_index(tavg = c(35, NA), humidity = c(60, 60))
  expect_true(is.na(result$value[2]))
})
