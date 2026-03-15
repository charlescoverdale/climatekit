test_that("ck_dry_days finds max consecutive dry days", {
  dates <- as.Date("2024-01-01") + 0:9
  precip <- c(0, 0, 5, 0, 0, 0, 2, 0, 0, 0)
  result <- ck_dry_days(precip, dates)
  expect_equal(result$value, 3)
  expect_equal(result$index, "dry_days")
})

test_that("ck_wet_days finds max consecutive wet days", {
  dates <- as.Date("2024-01-01") + 0:9
  precip <- c(5, 3, 0, 2, 8, 1, 0, 0, 4, 6)
  result <- ck_wet_days(precip, dates)
  expect_equal(result$value, 3)
})

test_that("ck_dry_days with custom threshold", {
  dates <- as.Date("2024-01-01") + 0:4
  precip <- c(0.5, 0.3, 0.8, 2, 0.1)
  result <- ck_dry_days(precip, dates, threshold = 1)
  expect_equal(result$value, 3)
})

test_that("ck_total_precip sums correctly", {
  dates <- as.Date("2024-01-01") + 0:4
  precip <- c(0, 5, 3, 0, 8)
  result <- ck_total_precip(precip, dates)
  expect_equal(result$value, 16)
  expect_equal(result$unit, "mm")
})

test_that("ck_heavy_precip counts correctly", {
  dates <- as.Date("2024-01-01") + 0:4
  precip <- c(0, 5, 12, 0, 15)
  result <- ck_heavy_precip(precip, dates, threshold = 10)
  expect_equal(result$value, 2)
})

test_that("ck_very_heavy_precip counts correctly", {
  dates <- as.Date("2024-01-01") + 0:4
  precip <- c(0, 5, 22, 0, 25)
  result <- ck_very_heavy_precip(precip, dates, threshold = 20)
  expect_equal(result$value, 2)
})

test_that("ck_max_1day_precip returns max", {
  dates <- as.Date("2024-01-01") + 0:4
  precip <- c(0, 5, 22, 0, 15)
  result <- ck_max_1day_precip(precip, dates)
  expect_equal(result$value, 22)
})

test_that("ck_max_5day_precip returns max rolling sum", {
  dates <- as.Date("2024-01-01") + 0:9
  precip <- c(0, 1, 2, 3, 4, 10, 0, 0, 0, 0)
  result <- ck_max_5day_precip(precip, dates)
  # 1+2+3+4+10 = 20 or 2+3+4+10+0 = 19, max is 20
  expect_equal(result$value, 20)
})

test_that("ck_max_5day_precip with fewer than 5 days", {
  dates <- as.Date("2024-01-01") + 0:2
  precip <- c(5, 3, 8)
  result <- ck_max_5day_precip(precip, dates)
  expect_equal(result$value, 16)
})

test_that("ck_precip_intensity computes mean of wet days", {
  dates <- as.Date("2024-01-01") + 0:4
  precip <- c(0, 5, 0, 15, 0)
  result <- ck_precip_intensity(precip, dates)
  expect_equal(result$value, 10)
  expect_equal(result$unit, "mm/day")
})

test_that("ck_precip_intensity returns 0 when no wet days", {
  dates <- as.Date("2024-01-01") + 0:4
  precip <- c(0, 0.5, 0, 0.3, 0)
  result <- ck_precip_intensity(precip, dates)
  expect_equal(result$value, 0)
})

test_that("monthly aggregation works for precipitation", {
  dates <- c(as.Date("2024-01-01") + 0:4, as.Date("2024-02-01") + 0:4)
  precip <- c(0, 5, 3, 0, 8, 10, 0, 0, 2, 1)
  result <- ck_total_precip(precip, dates, period = "monthly")
  expect_equal(nrow(result), 2)
  expect_equal(result$value, c(16, 13))
})

test_that("max_consecutive handles all-FALSE", {
  expect_equal(climatekit:::max_consecutive(c(FALSE, FALSE)), 0)
})

test_that("max_consecutive handles all-TRUE", {
  expect_equal(climatekit:::max_consecutive(c(TRUE, TRUE, TRUE)), 3)
})

test_that("max_consecutive handles empty", {
  expect_equal(climatekit:::max_consecutive(logical(0)), 0)
})

test_that("max_consecutive handles NA", {
  expect_equal(climatekit:::max_consecutive(c(TRUE, NA, TRUE)), 1)
})
