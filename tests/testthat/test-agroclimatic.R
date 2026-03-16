test_that("ck_huglin returns annual data frame", {
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-09-30"), by = "day")
  set.seed(42)
  tmin <- rnorm(length(dates), mean = 12, sd = 3)
  tmax <- tmin + runif(length(dates), 8, 15)
  result <- ck_huglin(tmin, tmax, dates, lat = 45)
  expect_s3_class(result, "data.frame")
  expect_equal(result$index, "huglin")
  expect_true(result$value > 0)
})

test_that("ck_huglin rejects mismatched lengths", {
  dates <- as.Date("2024-04-01") + 0:4
  expect_error(ck_huglin(1:5, 1:3, dates, lat = 45), "same length")
})

test_that("ck_huglin rejects non-numeric lat", {
  dates <- as.Date("2024-04-01") + 0:4
  expect_error(ck_huglin(1:5, 6:10, dates, lat = "a"), "numeric")
})

test_that("ck_winkler returns annual GDD", {
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-10-31"), by = "day")
  set.seed(42)
  tavg <- rnorm(length(dates), mean = 18, sd = 4)
  result <- ck_winkler(tavg, dates)
  expect_s3_class(result, "data.frame")
  expect_equal(result$index, "winkler")
  expect_true(result$value > 0)
})

test_that("ck_winkler ignores temps below 10", {
  dates <- as.Date("2024-04-01") + 0:4
  tavg <- c(5, 8, 15, 20, 9)
  result <- ck_winkler(tavg, dates)
  # Only (15-10) + (20-10) = 15
  expect_equal(result$value, 15)
})

test_that("ck_branas computes hydrothermal index", {
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-08-31"), by = "day")
  set.seed(42)
  tavg <- rnorm(length(dates), mean = 12, sd = 3)
  precip <- rgamma(length(dates), shape = 0.5, rate = 0.2)
  result <- ck_branas(precip, tavg, dates)
  expect_s3_class(result, "data.frame")
  expect_equal(result$index, "branas")
})

test_that("ck_branas rejects mismatched lengths", {
  dates <- as.Date("2024-04-01") + 0:4
  expect_error(ck_branas(1:5, 1:3, dates), "same length")
})

test_that("ck_first_frost finds first frost after July 1", {
  dates <- seq(as.Date("2024-07-01"), as.Date("2024-12-31"), by = "day")
  tmin <- rep(10, length(dates))
  # Set frost on Oct 15 (day index for Oct 15 from Jul 1 = 106)
  frost_date <- as.Date("2024-10-15")
  tmin[dates == frost_date] <- -2
  result <- ck_first_frost(tmin, dates)
  expect_equal(result$date, frost_date)
  expect_equal(result$value, as.integer(format(frost_date, "%j")))
})

test_that("ck_first_frost returns NA when no frost", {
  dates <- seq(as.Date("2024-07-01"), as.Date("2024-12-31"), by = "day")
  tmin <- rep(10, length(dates))
  result <- ck_first_frost(tmin, dates)
  expect_true(is.na(result$value))
})

test_that("ck_last_frost finds last frost before July 1", {
  dates <- seq(as.Date("2024-01-01"), as.Date("2024-06-30"), by = "day")
  tmin <- rep(10, length(dates))
  frost_date <- as.Date("2024-04-10")
  tmin[dates == frost_date] <- -1
  result <- ck_last_frost(tmin, dates)
  expect_equal(result$date, frost_date)
})

test_that("ck_last_frost returns NA when no frost", {
  dates <- seq(as.Date("2024-01-01"), as.Date("2024-06-30"), by = "day")
  tmin <- rep(10, length(dates))
  result <- ck_last_frost(tmin, dates)
  expect_true(is.na(result$value))
})

# --- Reference value tests ---

test_that("Huglin K coefficient at specific latitudes", {
  # K(40) = 1.02, K(45) = 1.04, K(50) = 1.06 (Huglin 1978)
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-09-30"), by = "day")
  tmin <- rep(15, length(dates))
  tmax <- rep(25, length(dates))

  # At lat=40 vs lat=50, Huglin should differ by the K ratio
  r40 <- ck_huglin(tmin, tmax, dates, lat = 40)
  r45 <- ck_huglin(tmin, tmax, dates, lat = 45)
  r50 <- ck_huglin(tmin, tmax, dates, lat = 50)

  # Ratio should reflect K values: 1.04/1.02 and 1.06/1.02
  expect_equal(r45$value / r40$value, 1.04 / 1.02, tolerance = 0.001)
  expect_equal(r50$value / r40$value, 1.06 / 1.02, tolerance = 0.001)
})

test_that("Winkler with all temps exactly 10C returns 0", {
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-10-31"), by = "day")
  tavg <- rep(10, length(dates))
  result <- ck_winkler(tavg, dates)
  expect_equal(result$value, 0)
})

test_that("Branas with known monthly means and precip", {
  # April only: mean temp = 15, total precip = 100
  # HI = 15 * 100 = 1500
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-04-30"), by = "day")
  tavg <- rep(15, 30)
  precip <- rep(100 / 30, 30)  # totals to 100mm
  result <- ck_branas(precip, tavg, dates)
  expect_equal(result$value, 15 * 100, tolerance = 1)
})
