test_that("ck_compute dispatches frost_days", {
  d <- data.frame(
    dates = as.Date("2024-01-01") + 0:9,
    tmin = c(-2, 3, -1, 5, -3, 0, 2, -4, 1, -1)
  )
  result <- ck_compute(d, "frost_days")
  expect_equal(result$value, 5)
  expect_equal(result$index, "frost_days")
})

test_that("ck_compute dispatches with extra args", {
  d <- data.frame(
    dates = as.Date("2024-01-01") + 0:9,
    tavg = c(10, 15, 20, 5, 18, 22, 8, 25, 12, 30)
  )
  result <- ck_compute(d, "heating_degree_days", base = 20)
  expect_s3_class(result, "data.frame")
})

test_that("ck_compute rejects unknown index", {
  d <- data.frame(dates = as.Date("2024-01-01"), tmin = 1)
  expect_error(ck_compute(d, "nonexistent"), "Unknown index")
})

test_that("ck_compute rejects missing columns", {
  d <- data.frame(dates = as.Date("2024-01-01"), tmax = 1)
  expect_error(ck_compute(d, "frost_days"), "Missing required")
})

test_that("ck_compute works with list input", {
  d <- list(
    dates = as.Date("2024-01-01") + 0:4,
    tmin = c(-2, 3, -1, 5, -3)
  )
  result <- ck_compute(d, "frost_days")
  expect_equal(result$value, 3)
})

test_that("ck_compute dispatches comfort indices", {
  d <- list(
    tavg = c(30, 35),
    humidity = c(60, 70)
  )
  result <- ck_compute(d, "heat_index")
  expect_equal(nrow(result), 2)
})

test_that("ck_compute dispatches precipitation indices", {
  d <- data.frame(
    dates = as.Date("2024-01-01") + 0:4,
    precip = c(0, 5, 12, 0, 15)
  )
  result <- ck_compute(d, "total_precip")
  expect_equal(result$value, 32)
})

test_that("ck_available returns correct structure", {
  result <- ck_available()
  expect_s3_class(result, "data.frame")
  expect_true(all(c("index", "category", "unit", "description") %in% names(result)))
  expect_true(nrow(result) >= 28)
})

test_that("ck_metadata returns correct info", {
  result <- ck_metadata("frost_days")
  expect_true(is.list(result))
  expect_equal(result$index, "frost_days")
  expect_equal(result$category, "temperature")
})

test_that("ck_metadata rejects unknown index", {
  expect_error(ck_metadata("fake_index"), "Unknown index")
})

test_that("ck_metadata rejects non-string", {
  expect_error(ck_metadata(123), "character string")
})

test_that("ck_convert_temp C to F", {
  expect_equal(ck_convert_temp(0, "C", "F"), 32)
  expect_equal(ck_convert_temp(100, "C", "F"), 212)
})

test_that("ck_convert_temp F to C", {
  expect_equal(ck_convert_temp(32, "F", "C"), 0)
  expect_equal(ck_convert_temp(212, "F", "C"), 100)
})

test_that("ck_convert_temp C to K", {
  expect_equal(ck_convert_temp(0, "C", "K"), 273.15)
})

test_that("ck_convert_temp same unit returns unchanged", {
  expect_equal(ck_convert_temp(42, "C", "C"), 42)
})

test_that("ck_convert_temp rejects non-numeric", {
  expect_error(ck_convert_temp("a", "C", "F"), "numeric")
})
