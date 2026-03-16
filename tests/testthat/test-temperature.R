test_that("ck_frost_days counts days below 0", {
  dates <- as.Date("2024-01-01") + 0:9
  tmin <- c(-2, 3, -1, 5, -3, 0, 2, -4, 1, -1)
  result <- ck_frost_days(tmin, dates)
  expect_s3_class(result, "data.frame")
  expect_equal(result$value, 5)
  expect_equal(result$index, "frost_days")
  expect_equal(result$unit, "days")
})

test_that("ck_frost_days monthly aggregation works", {
  dates <- c(as.Date("2024-01-01") + 0:4, as.Date("2024-02-01") + 0:4)
  tmin <- c(-2, 3, -1, 5, -3, 0, 2, -4, 1, -1)
  result <- ck_frost_days(tmin, dates, period = "monthly")
  expect_equal(nrow(result), 2)
  expect_equal(result$value, c(3, 2))
})

test_that("ck_frost_days rejects non-numeric", {
  dates <- as.Date("2024-01-01") + 0:2
  expect_error(ck_frost_days("a", dates), "numeric")
})

test_that("ck_frost_days rejects mismatched lengths", {
  expect_error(ck_frost_days(c(1, 2), as.Date("2024-01-01")), "same length")
})

test_that("ck_ice_days counts days where tmax < 0", {
  dates <- as.Date("2024-01-01") + 0:4
  tmax <- c(-2, 3, -1, 5, -3)
  result <- ck_ice_days(tmax, dates)
  expect_equal(result$value, 3)
})

test_that("ck_summer_days counts days where tmax > 25", {
  dates <- as.Date("2024-07-01") + 0:4
  tmax <- c(22, 26, 28, 24, 30)
  result <- ck_summer_days(tmax, dates)
  expect_equal(result$value, 3)
})

test_that("ck_tropical_nights counts days where tmin > 20", {
  dates <- as.Date("2024-07-01") + 0:4
  tmin <- c(18, 21, 22, 19, 25)
  result <- ck_tropical_nights(tmin, dates)
  expect_equal(result$value, 3)
})

test_that("ck_growing_season returns days using 6-day spells and tavg", {
  # Create a year of data with clear warm season
  dates <- as.Date("2024-01-01") + 0:364
  tavg <- sin(seq(0, 2 * pi, length.out = 365)) * 15 + 5
  result <- ck_growing_season(tavg, dates)
  expect_s3_class(result, "data.frame")
  expect_true(result$value > 0)
  expect_equal(result$index, "growing_season")
})

test_that("ck_growing_season returns 0 when no 6-day spell", {
  dates <- as.Date("2024-01-01") + 0:29
  tavg <- rep(-10, 30)
  result <- ck_growing_season(tavg, dates)
  expect_equal(result$value, 0)
})

test_that("ck_growing_season uses 6-day spell not 5-day", {
  # Exactly 5 warm days should NOT trigger GSL
  dates <- as.Date("2024-01-01") + 0:364
  tavg <- rep(0, 365)
  tavg[100:104] <- 10  # only 5 warm days
  result <- ck_growing_season(tavg, dates)
  expect_equal(result$value, 0)

  # 6 warm days should trigger
  tavg[100:105] <- 10
  result <- ck_growing_season(tavg, dates)
  expect_true(result$value > 0)
})

test_that("ck_growing_season ends at first cold spell after July 1", {
  dates <- as.Date("2024-01-01") + 0:364
  # Warm from day 60 to day 250, then cold
  tavg <- rep(0, 365)
  tavg[60:250] <- 10
  result <- ck_growing_season(tavg, dates, lat = 50)
  # Season starts at day 60, ends when cold spell starts after July 1
  # Cold spell of 6+ days starts at day 251
  # End of season = day 250
  expect_equal(result$value, 250 - 60 + 1)
})

test_that("ck_heating_degree_days sums correctly", {
  dates <- as.Date("2024-01-01") + 0:4
  tavg <- c(10, 15, 20, 5, 18)
  result <- ck_heating_degree_days(tavg, dates, base = 18)
  # (18-10) + (18-15) + 0 + (18-5) + 0 = 8 + 3 + 13 = 24
  expect_equal(result$value, 24)
})

test_that("ck_cooling_degree_days sums correctly", {
  dates <- as.Date("2024-07-01") + 0:4
  tavg <- c(20, 25, 15, 30, 18)
  result <- ck_cooling_degree_days(tavg, dates, base = 18)
  # (20-18) + (25-18) + 0 + (30-18) + 0 = 2 + 7 + 12 = 21
  expect_equal(result$value, 21)
})

test_that("ck_growing_degree_days sums correctly", {
  dates <- as.Date("2024-07-01") + 0:4
  tavg <- c(15, 20, 8, 12, 25)
  result <- ck_growing_degree_days(tavg, dates, base = 10)
  # (15-10) + (20-10) + 0 + (12-10) + (25-10) = 5 + 10 + 2 + 15 = 32
  expect_equal(result$value, 32)
})

test_that("ck_diurnal_range computes mean range", {
  dates <- as.Date("2024-01-01") + 0:4
  tmin <- c(0, 5, 2, 3, 1)
  tmax <- c(10, 15, 12, 13, 11)
  result <- ck_diurnal_range(tmin, tmax, dates)
  expect_equal(result$value, 10)
  expect_equal(result$unit, "\u00b0C")
})

test_that("ck_diurnal_range rejects mismatched lengths", {
  dates <- as.Date("2024-01-01") + 0:4
  expect_error(ck_diurnal_range(1:5, 1:3, dates), "same length")
})

test_that("ck_warm_spell detects spells >= 6 days", {
  dates <- as.Date("2024-01-01") + 0:19
  # 90th percentile will be high; create a clear 7-day spell above it
  tmax <- c(rep(10, 13), rep(50, 7))
  result <- ck_warm_spell(tmax, dates, threshold = 0.5)
  expect_true(result$value >= 7)
})

test_that("ck_warm_spell returns 0 with no long spells", {
  dates <- as.Date("2024-01-01") + 0:9
  tmax <- c(30, 10, 30, 10, 30, 10, 30, 10, 30, 10)
  result <- ck_warm_spell(tmax, dates)
  expect_equal(result$value, 0)
})

test_that("all temperature functions handle NA values", {
  dates <- as.Date("2024-01-01") + 0:4
  tmin <- c(-2, NA, -1, 5, -3)
  result <- ck_frost_days(tmin, dates)
  expect_s3_class(result, "data.frame")
})

test_that("temperature functions reject non-Date dates", {
  expect_error(ck_frost_days(c(1, 2), c("a", "b")), "Date")
})

# --- Reference value tests ---

test_that("frost days at exactly 0C returns 0 (strict < 0)", {
  dates <- as.Date("2024-01-01")
  result <- ck_frost_days(c(0), dates)
  expect_equal(result$value, 0)
})

test_that("HDD with base=0 returns 0 when all temps >= 0", {
  dates <- as.Date("2024-01-01") + 0:4
  tavg <- c(0, 5, 10, 15, 20)
  result <- ck_heating_degree_days(tavg, dates, base = 0)
  expect_equal(result$value, 0)
})

test_that("CDD with base=0 returns sum of all temps", {
  dates <- as.Date("2024-01-01") + 0:4
  tavg <- c(5, 10, 15, 20, 25)
  result <- ck_cooling_degree_days(tavg, dates, base = 0)
  expect_equal(result$value, 75)
})
