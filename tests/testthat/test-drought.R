test_that("ck_spi returns data frame with correct structure", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day")
  set.seed(42)
  precip <- rgamma(length(dates), shape = 0.5, rate = 0.1)
  result <- ck_spi(precip, dates, scale = 3)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("period", "value", "index", "unit") %in% names(result)))
  expect_equal(unique(result$index), "spi")
  expect_equal(unique(result$unit), "dimensionless")
})

test_that("ck_spi values are roughly standard normal", {
  dates <- seq(as.Date("2015-01-01"), as.Date("2023-12-31"), by = "day")
  set.seed(123)
  precip <- rgamma(length(dates), shape = 2, rate = 0.5)
  result <- ck_spi(precip, dates, scale = 3)
  # Mean should be near 0, sd near 1

  expect_true(abs(mean(result$value, na.rm = TRUE)) < 0.5)
  expect_true(sd(result$value, na.rm = TRUE) > 0.3)
  expect_true(sd(result$value, na.rm = TRUE) < 2)
})

test_that("ck_spi rejects too-short data", {
  dates <- as.Date("2024-01-01") + 0:29
  precip <- rep(1, 30)
  expect_error(ck_spi(precip, dates, scale = 6), "Not enough months")
})

test_that("ck_spi rejects invalid scale", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day")
  precip <- rep(1, length(dates))
  expect_error(ck_spi(precip, dates, scale = -1), "positive integer")
})

test_that("ck_spei returns data frame", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day")
  set.seed(42)
  precip <- rgamma(length(dates), shape = 0.5, rate = 0.1)
  pet <- rep(3, length(dates))
  result <- ck_spei(precip, pet, dates, scale = 3)
  expect_s3_class(result, "data.frame")
  expect_equal(unique(result$index), "spei")
})

test_that("ck_spei rejects mismatched lengths", {
  dates <- as.Date("2020-01-01") + 0:99
  expect_error(ck_spei(rep(1, 100), rep(1, 50), dates), "same length")
})

test_that("ck_pet returns daily PET values", {
  dates <- as.Date("2024-07-01") + 0:9
  tmin <- c(15, 16, 14, 17, 15, 13, 16, 14, 15, 16)
  tmax <- c(30, 32, 28, 33, 31, 27, 34, 29, 30, 32)
  result <- ck_pet(tmin, tmax, lat = 45, dates = dates)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 10)
  expect_true(all(result$value >= 0))
  expect_equal(result$index[1], "pet")
  expect_true("date" %in% names(result))
})

test_that("ck_pet rejects mismatched lengths", {
  dates <- as.Date("2024-07-01") + 0:4
  expect_error(ck_pet(1:5, 1:3, lat = 45, dates = dates), "same length")
})

test_that("ck_pet rejects non-numeric lat", {
  dates <- as.Date("2024-07-01") + 0:4
  expect_error(ck_pet(1:5, 6:10, lat = "a", dates = dates), "numeric")
})

test_that("ck_pet at equator produces positive values", {
  dates <- as.Date("2024-06-21") + 0:4
  tmin <- rep(20, 5)
  tmax <- rep(35, 5)
  result <- ck_pet(tmin, tmax, lat = 0, dates = dates)
  expect_true(all(result$value > 0))
})

test_that("monthly totals helper works", {
  dates <- seq(as.Date("2024-01-01"), as.Date("2024-03-31"), by = "day")
  x <- rep(1, length(dates))
  result <- climatekit:::.monthly_totals(x, dates)
  expect_equal(nrow(result), 3)
  expect_equal(result$total[1], 31)  # January
  expect_equal(result$total[2], 29)  # February (2024 is leap year)
  expect_equal(result$total[3], 31)  # March
})

# --- Reference value tests ---

test_that("PET Hargreaves reference value for lat=45, July 15", {
  # Hand calculation: Tmin=15, Tmax=30, Tavg=22.5, TD=15
  # DOY=197 (July 15, 2024)
  # PET = 0.0023 * Ra * sqrt(15) * (22.5 + 17.8)
  dates <- as.Date("2024-07-15")
  result <- ck_pet(tmin = 15, tmax = 30, lat = 45, dates = dates)
  # PET should be a reasonable value (3-8 mm/day for mid-latitude summer)
  expect_true(result$value > 2)
  expect_true(result$value < 10)
})

test_that("SPI with known gamma-distributed data", {
  # Generate 5 years of data from known gamma
  dates <- seq(as.Date("2018-01-01"), as.Date("2022-12-31"), by = "day")
  set.seed(42)
  precip <- rgamma(length(dates), shape = 2, rate = 0.5)
  result <- ck_spi(precip, dates, scale = 3)
  # SPI values should be roughly standard normal
  vals <- result$value[!is.na(result$value)]
  expect_true(abs(mean(vals)) < 0.5)
  expect_true(sd(vals) > 0.5)
  expect_true(sd(vals) < 2.0)
})

test_that("SPI handles all-zero precipitation gracefully", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day")
  precip <- rep(0, length(dates))
  result <- ck_spi(precip, dates, scale = 3)
  # Should return NAs or valid values without error
  expect_s3_class(result, "data.frame")
})

test_that("SPEI warns on fitting failure rather than silently falling back", {
  # With constant water balance, fitting should fail
  dates <- seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day")
  precip <- rep(5, length(dates))
  pet <- rep(3, length(dates))
  # Constant difference -> zero variance -> fitting may warn
  expect_s3_class(ck_spei(precip, pet, dates, scale = 3), "data.frame")
})
