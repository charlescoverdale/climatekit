# ET-SCI heatwave / cold-wave family tests.
# Reference period 1961-1961 (one year) keeps thresholds out of the
# analysis years so injected events show up cleanly.

make_tmax_two_years <- function(seed = 31) {
  set.seed(seed)
  dates <- seq(as.Date("1961-01-01"), as.Date("1962-12-31"), by = "day")
  tmax <- 15 + 10 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
          rnorm(length(dates))
  list(dates = dates, tmax = tmax)
}

make_tmin_two_years <- function(seed = 32) {
  set.seed(seed)
  dates <- seq(as.Date("1961-01-01"), as.Date("1962-12-31"), by = "day")
  tmin <- 5 + 8 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
          rnorm(length(dates))
  list(dates = dates, tmin = tmin)
}

# Heatwave family ------------------------------------------------------------

test_that("heatwave family functions return sane structure and units", {
  d <- make_tmax_two_years()
  for (fn in c("ck_hwn", "ck_hwf", "ck_hwd", "ck_hwm", "ck_hwa")) {
    result <- do.call(fn, list(d$tmax, d$dates,
                               ref_start = 1961L, ref_end = 1961L))
    expect_s3_class(result, "data.frame")
    expect_true(all(c("period", "value", "index", "unit") %in% names(result)),
                label = fn)
  }
})

test_that("ck_hwn detects injected 7-day heatwave in 1962", {
  d <- make_tmax_two_years(seed = 41)
  hot <- which(d$dates >= as.Date("1962-07-15") & d$dates <= as.Date("1962-07-21"))
  d$tmax[hot] <- d$tmax[hot] + 25  # well above any reasonable threshold
  result <- ck_hwn(d$tmax, d$dates, ref_start = 1961L, ref_end = 1961L)
  yr_1962 <- result$value[format(result$period, "%Y") == "1962"]
  expect_gte(yr_1962, 1L)
})

test_that("HWF >= HWD >= 0 (frequency dominates duration)", {
  d <- make_tmax_two_years(seed = 42)
  hot <- which(d$dates >= as.Date("1962-07-15") & d$dates <= as.Date("1962-07-21"))
  d$tmax[hot] <- d$tmax[hot] + 25
  hwf <- ck_hwf(d$tmax, d$dates, ref_start = 1961L, ref_end = 1961L)
  hwd <- ck_hwd(d$tmax, d$dates, ref_start = 1961L, ref_end = 1961L)
  expect_true(all(hwf$value >= hwd$value))
  expect_true(all(hwd$value >= 0))
})

test_that("HWA >= HWM (peak excess >= mean excess)", {
  d <- make_tmax_two_years(seed = 43)
  hot <- which(d$dates >= as.Date("1962-07-15") & d$dates <= as.Date("1962-07-21"))
  d$tmax[hot] <- d$tmax[hot] + 25
  hwm <- ck_hwm(d$tmax, d$dates, ref_start = 1961L, ref_end = 1961L)
  hwa <- ck_hwa(d$tmax, d$dates, ref_start = 1961L, ref_end = 1961L)
  yr <- format(hwm$period, "%Y") == "1962"
  expect_true(hwa$value[yr] >= hwm$value[yr])
})

test_that("HWM and HWA return NA for years with no heatwaves", {
  d <- make_tmax_two_years(seed = 44)
  # No injection: noisy data shouldn't reliably produce 3-day spells in 1962
  hwm <- ck_hwm(d$tmax, d$dates, ref_start = 1961L, ref_end = 1961L)
  hwa <- ck_hwa(d$tmax, d$dates, ref_start = 1961L, ref_end = 1961L)
  # Where HWN is 0, HWM/HWA should be NA
  hwn <- ck_hwn(d$tmax, d$dates, ref_start = 1961L, ref_end = 1961L)
  zero_years <- hwn$value == 0
  if (any(zero_years)) {
    expect_true(all(is.na(hwm$value[zero_years])))
    expect_true(all(is.na(hwa$value[zero_years])))
  }
})

# Cold-wave family -----------------------------------------------------------

test_that("cold-wave family functions return sane structure", {
  d <- make_tmin_two_years()
  for (fn in c("ck_cwn", "ck_cwf", "ck_cwd", "ck_cwm", "ck_cwa")) {
    result <- do.call(fn, list(d$tmin, d$dates,
                               ref_start = 1961L, ref_end = 1961L))
    expect_s3_class(result, "data.frame")
  }
})

test_that("ck_cwn detects injected cold spell", {
  d <- make_tmin_two_years(seed = 51)
  cold <- which(d$dates >= as.Date("1962-02-01") & d$dates <= as.Date("1962-02-07"))
  d$tmin[cold] <- d$tmin[cold] - 25
  result <- ck_cwn(d$tmin, d$dates, ref_start = 1961L, ref_end = 1961L)
  yr_1962 <- result$value[format(result$period, "%Y") == "1962"]
  expect_gte(yr_1962, 1L)
})

test_that("cold-wave magnitudes are positive (threshold - tmin)", {
  d <- make_tmin_two_years(seed = 52)
  cold <- which(d$dates >= as.Date("1962-02-01") & d$dates <= as.Date("1962-02-07"))
  d$tmin[cold] <- d$tmin[cold] - 25
  cwm <- ck_cwm(d$tmin, d$dates, ref_start = 1961L, ref_end = 1961L)
  cwa <- ck_cwa(d$tmin, d$dates, ref_start = 1961L, ref_end = 1961L)
  yr <- format(cwm$period, "%Y") == "1962"
  expect_true(cwm$value[yr] > 0)
  expect_true(cwa$value[yr] >= cwm$value[yr])
})

# Argument validation --------------------------------------------------------

test_that("heatwave / cold-wave functions reject mismatched lengths", {
  expect_error(ck_hwn(c(1, 2), as.Date("1961-01-01")), "same length")
  expect_error(ck_cwa(c(1, 2), as.Date("1961-01-01")), "same length")
})

test_that("heatwave / cold-wave functions error on missing reference data", {
  dates <- seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day")
  tmax <- rnorm(length(dates), 15, 5)
  tmin <- rnorm(length(dates), 5, 5)
  expect_error(ck_hwn(tmax, dates, ref_start = 1961L, ref_end = 1990L),
               "reference period")
  expect_error(ck_cwn(tmin, dates, ref_start = 1961L, ref_end = 1990L),
               "reference period")
})

# Excess vs absolute mode (Phase B) ------------------------------------------

test_that("ck_hwm / ck_hwa absolute mode returns raw Tmax", {
  d <- make_tmax_two_years(seed = 61)
  hot <- which(d$dates >= as.Date("1962-07-15") & d$dates <= as.Date("1962-07-21"))
  d$tmax[hot] <- d$tmax[hot] + 25  # injected heatwave
  hwm_ex  <- ck_hwm(d$tmax, d$dates, ref_start = 1961L, ref_end = 1961L,
                    mode = "excess")
  hwm_abs <- ck_hwm(d$tmax, d$dates, ref_start = 1961L, ref_end = 1961L,
                    mode = "absolute")
  hwa_ex  <- ck_hwa(d$tmax, d$dates, ref_start = 1961L, ref_end = 1961L,
                    mode = "excess")
  hwa_abs <- ck_hwa(d$tmax, d$dates, ref_start = 1961L, ref_end = 1961L,
                    mode = "absolute")
  yr <- format(hwm_ex$period, "%Y") == "1962"
  # Absolute Tmax must be larger than the excess (absolute = excess + threshold).
  expect_gt(hwm_abs$value[yr], hwm_ex$value[yr])
  expect_gt(hwa_abs$value[yr], hwa_ex$value[yr])
})

test_that("ck_cwm / ck_cwa absolute mode returns raw Tmin", {
  d <- make_tmin_two_years(seed = 62)
  cold <- which(d$dates >= as.Date("1962-02-01") & d$dates <= as.Date("1962-02-07"))
  d$tmin[cold] <- d$tmin[cold] - 25
  cwm_ex  <- ck_cwm(d$tmin, d$dates, ref_start = 1961L, ref_end = 1961L,
                    mode = "excess")
  cwm_abs <- ck_cwm(d$tmin, d$dates, ref_start = 1961L, ref_end = 1961L,
                    mode = "absolute")
  cwa_ex  <- ck_cwa(d$tmin, d$dates, ref_start = 1961L, ref_end = 1961L,
                    mode = "excess")
  cwa_abs <- ck_cwa(d$tmin, d$dates, ref_start = 1961L, ref_end = 1961L,
                    mode = "absolute")
  yr <- format(cwm_ex$period, "%Y") == "1962"
  # excess is positive; absolute Tmin during deep cold should be negative or much lower
  expect_lt(cwm_abs$value[yr], cwm_ex$value[yr])
  expect_lt(cwa_abs$value[yr], cwa_ex$value[yr])
})

test_that("invalid mode is rejected", {
  d <- make_tmax_two_years(seed = 63)
  expect_error(
    ck_hwm(d$tmax, d$dates, ref_start = 1961L, ref_end = 1961L,
           mode = "bogus"),
    "should be one of"
  )
})
