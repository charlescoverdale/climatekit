# Tests for ck_ehf (Excess Heat Factor, Nairn & Fawcett 2013).

make_climate_two_years <- function(seed = 71) {
  set.seed(seed)
  dates <- seq(as.Date("1961-01-01"), as.Date("1962-12-31"), by = "day")
  s <- 2 * pi * as.integer(format(dates, "%j")) / 365
  tmax <- 20 + 10 * sin(s) + rnorm(length(dates))
  tmin <- 10 +  8 * sin(s) + rnorm(length(dates))
  list(dates = dates, tmax = tmax, tmin = tmin)
}

test_that("ck_ehf returns annual data frame with expected shape", {
  d <- make_climate_two_years()
  result <- ck_ehf(d$tmax, d$tmin, d$dates,
                   ref_start = 1961L, ref_end = 1961L)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2L)
  expect_equal(result$index[1], "ehf_max")
  expect_true(all(c("period", "value", "index", "unit") %in% names(result)))
})

test_that("ck_ehf 'n_positive' counts heatwave-condition days", {
  d <- make_climate_two_years(seed = 72)
  hot <- which(d$dates >= as.Date("1962-07-15") & d$dates <= as.Date("1962-07-25"))
  d$tmax[hot] <- d$tmax[hot] + 15
  d$tmin[hot] <- d$tmin[hot] + 10
  result <- ck_ehf(d$tmax, d$tmin, d$dates,
                   ref_start = 1961L, ref_end = 1961L,
                   stat = "n_positive")
  yr_1962 <- result$value[format(result$period, "%Y") == "1962"]
  expect_gte(yr_1962, 5)
  expect_equal(result$index[1], "ehf_n_positive")
  expect_equal(result$unit[1], "days")
})

test_that("ck_ehf 'sum_positive' >= max EHF on year of injected heatwave", {
  d <- make_climate_two_years(seed = 73)
  hot <- which(d$dates >= as.Date("1962-07-15") & d$dates <= as.Date("1962-07-25"))
  d$tmax[hot] <- d$tmax[hot] + 15
  d$tmin[hot] <- d$tmin[hot] + 10
  rmax <- ck_ehf(d$tmax, d$tmin, d$dates,
                 ref_start = 1961L, ref_end = 1961L, stat = "max")
  rsum <- ck_ehf(d$tmax, d$tmin, d$dates,
                 ref_start = 1961L, ref_end = 1961L, stat = "sum_positive")
  yr <- format(rmax$period, "%Y") == "1962"
  expect_gte(rsum$value[yr], rmax$value[yr])
})

test_that("ck_ehf rejects bad stat", {
  d <- make_climate_two_years(seed = 74)
  expect_error(
    ck_ehf(d$tmax, d$tmin, d$dates, ref_start = 1961L, ref_end = 1961L,
           stat = "bogus"),
    "should be one of"
  )
})

test_that("ck_ehf rejects mismatched lengths and missing reference data", {
  expect_error(ck_ehf(c(1, 2), c(1, 2, 3), as.Date("1961-01-01") + 0:1),
               "same length")
  dates <- seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day")
  tmax <- rnorm(length(dates), 20, 5)
  tmin <- rnorm(length(dates), 10, 4)
  expect_error(ck_ehf(tmax, tmin, dates, ref_start = 1961L, ref_end = 1990L),
               "reference period")
})

test_that("ck_compute dispatches ehf", {
  d <- make_climate_two_years(seed = 75)
  df <- data.frame(dates = d$dates, tmax = d$tmax, tmin = d$tmin)
  result <- ck_compute(df, "ehf", ref_start = 1961L, ref_end = 1961L)
  expect_s3_class(result, "data.frame")
})
