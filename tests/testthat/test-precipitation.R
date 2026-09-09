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
  # A five-day maximum is undefined on three days. Up to v0.2.1 this
  # returned the three-day sum (16) as though it were an Rx5day value.
  dates <- as.Date("2024-01-01") + 0:2
  precip <- c(5, 3, 8)
  result <- ck_max_5day_precip(precip, dates)
  expect_true(is.na(result$value))
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

# --- Reference value tests ---

test_that("dry days with all-zero precip returns full length", {
  dates <- as.Date("2024-01-01") + 0:9
  precip <- rep(0, 10)
  result <- ck_dry_days(precip, dates)
  expect_equal(result$value, 10)
})

test_that("wet days with all precip >= 1 returns full length", {
  dates <- as.Date("2024-01-01") + 0:9
  precip <- rep(5, 10)
  result <- ck_wet_days(precip, dates)
  expect_equal(result$value, 10)
})

test_that("max 5-day precip with exactly 5 days", {
  dates <- as.Date("2024-01-01") + 0:4
  precip <- c(1, 2, 3, 4, 5)
  result <- ck_max_5day_precip(precip, dates)
  expect_equal(result$value, 15)
})

test_that("precip = exactly 1.0 mm counts as wet, not dry", {
  dates <- as.Date("2024-01-01") + 0:4
  precip <- c(1.0, 1.0, 1.0, 1.0, 1.0)
  result_wet <- ck_wet_days(precip, dates)
  result_dry <- ck_dry_days(precip, dates)
  expect_equal(result_wet$value, 5)
  expect_equal(result_dry$value, 0)
})

# ETCCDI percentile-based precip indices (R95p / R99p) -----------------------

test_that("ck_r95p sums precip on extreme wet days, returns mm", {
  set.seed(11)
  dates <- seq(as.Date("1961-01-01"), as.Date("1962-12-31"), by = "day")
  precip <- pmax(rgamma(length(dates), shape = 0.4, scale = 8) - 1, 0)
  result <- ck_r95p(precip, dates, ref_start = 1961L, ref_end = 1962L)
  expect_s3_class(result, "data.frame")
  expect_equal(result$index[1], "r95p")
  expect_equal(result$unit[1], "mm")
  expect_true(all(result$value >= 0))
  # R95p must be <= total annual precip on wet days
  total <- ck_total_precip(precip, dates)
  expect_true(all(result$value <= total$value + 1e-6))
})

test_that("ck_r99p sums even smaller fraction than ck_r95p", {
  set.seed(12)
  dates <- seq(as.Date("1961-01-01"), as.Date("1962-12-31"), by = "day")
  precip <- pmax(rgamma(length(dates), shape = 0.4, scale = 8) - 1, 0)
  r95 <- ck_r95p(precip, dates, ref_start = 1961L, ref_end = 1962L)
  r99 <- ck_r99p(precip, dates, ref_start = 1961L, ref_end = 1962L)
  expect_equal(r99$index[1], "r99p")
  # R99p <= R95p (smaller fraction of days exceed the higher threshold)
  expect_true(all(r99$value <= r95$value + 1e-6))
})

test_that("ck_r95p errors on no reference data", {
  dates <- seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day")
  precip <- rep(0.1, length(dates))
  expect_error(
    ck_r95p(precip, dates, ref_start = 1961L, ref_end = 1990L),
    "reference period"
  )
})

test_that("ck_r95p errors on dry reference period", {
  dates <- seq(as.Date("1961-01-01"), as.Date("1961-12-31"), by = "day")
  precip <- rep(0, length(dates))
  expect_error(
    ck_r95p(precip, dates, ref_start = 1961L, ref_end = 1961L),
    "wet days"
  )
})

# Bug fixes ------------------------------------------------------------------

test_that("ck_total_precip applies ETCCDI wet-day filter (PRCPTOT)", {
  dates <- as.Date("2024-01-01") + 0:5
  # Three trace days (<1mm) and three wet days; ETCCDI excludes the trace days.
  precip <- c(0.5, 0.3, 0.7, 5, 10, 15)
  expect_equal(ck_total_precip(precip, dates)$value, 30)
})

test_that("ck_total_precip wet_day_threshold = 0 recovers raw sum", {
  dates <- as.Date("2024-01-01") + 0:5
  precip <- c(0.5, 0.3, 0.7, 5, 10, 15)
  expect_equal(
    ck_total_precip(precip, dates, wet_day_threshold = 0)$value,
    sum(precip)
  )
})

test_that("ck_total_precip rejects negative threshold", {
  dates <- as.Date("2024-01-01") + 0:2
  expect_error(
    ck_total_precip(c(1, 2, 3), dates, wet_day_threshold = -1),
    "non-negative"
  )
})

test_that("ck_precip_intensity returns NA for all-NA period", {
  dates <- as.Date("2024-01-01") + 0:4
  expect_true(is.na(ck_precip_intensity(rep(NA_real_, 5), dates)$value))
})

test_that("ck_precip_intensity ignores NAs in mixed period", {
  dates <- as.Date("2024-01-01") + 0:4
  precip <- c(NA, 5, NA, 15, NA)
  result <- ck_precip_intensity(precip, dates)
  expect_equal(result$value, 10)
})
