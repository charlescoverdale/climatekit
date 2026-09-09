# Regression guards for the three defects fixed in v0.2.2. Each test states
# the property the index must satisfy by definition, so it fails if the
# formula is ever transcribed wrongly again.

test_that("SPEI is a standardised index under every distribution", {
  # SPI and SPEI are standard normal deviates by construction. Before
  # v0.2.2 the default log-logistic fit returned mean 1.61, sd 3.10.
  set.seed(1)
  dates <- seq(as.Date("1940-01-01"), by = "day", length.out = 80 * 365)
  precip <- rgamma(length(dates), shape = 0.6, rate = 0.15)
  pet <- 3 + 2 * sin(2 * pi * as.integer(format(dates, "%j")) / 365)

  for (dist in c("log-logistic", "gev")) {
    v <- suppressWarnings(
      ck_spei(precip, pet, dates, scale = 3, distribution = dist)$value)
    v <- v[is.finite(v)]
    expect_gt(length(v), 500)
    expect_lt(abs(mean(v)), 0.15, label = paste("SPEI", dist, "mean"))
    expect_lt(abs(sd(v) - 1), 0.15, label = paste("SPEI", dist, "sd"))
  }
  for (dist in c("gamma", "pearsonIII")) {
    v <- ck_spi(precip, dates, scale = 3, distribution = dist)$value
    v <- v[is.finite(v)]
    expect_lt(abs(mean(v)), 0.15, label = paste("SPI", dist, "mean"))
    expect_lt(abs(sd(v) - 1), 0.15, label = paste("SPI", dist, "sd"))
  }
})

test_that("the log-logistic L-moment estimator recovers known parameters", {
  # Simulate from a 3-parameter log-logistic and check the fit is centred
  # on the truth. The pre-v0.2.2 estimator returned beta ~ 1 regardless.
  set.seed(7)
  xi <- 10; alpha <- 4; beta <- 3
  u <- runif(20000)
  x <- xi + alpha * (u / (1 - u))^(1 / beta)
  z <- climatekit:::.loglogistic_to_normal(x)
  # A correct fit maps the sample onto standard normal deviates
  expect_lt(abs(mean(z, na.rm = TRUE)), 0.05)
  expect_lt(abs(sd(z, na.rm = TRUE) - 1), 0.05)
})

test_that("EHF follows Nairn & Fawcett (2013), not the swapped operands", {
  dates <- seq(as.Date("1961-01-01"), as.Date("1990-12-31"), by = "day")
  doy <- as.integer(format(dates, "%j"))
  tmax <- 20 + 12 * sin(2 * pi * (doy - 100) / 365)
  tmin <- tmax - 8
  r <- climatekit:::.ck_ehf_daily(tmax, tmin, dates)

  # EHF = EHIsig * max(EHIaccl, 1)
  expect_equal(r$ehf, r$ehi_sig * pmax(r$ehi_accl, 1))

  # The defining property: EHF can only be positive when the recent
  # three-day mean exceeds the reference 95th percentile. The swapped
  # form flagged 4915 days that were below it.
  expect_false(any(r$ehf > 0 & r$ehi_sig <= 0, na.rm = TRUE))

  # And a heatwave metric should flag a small minority of days
  n_pos <- ck_ehf(tmax, tmin, dates, stat = "n_positive")$value
  expect_lt(mean(n_pos), 60)
})

test_that("extraterrestrial radiation is correct at the poles and in FAO-56", {
  ra <- function(lat, d) {
    climatekit:::.extraterrestrial_radiation(
      lat, as.integer(format(as.Date(d), "%j"))) * 2.45
  }
  # FAO-56 Example 8: 3 September at 20 S -> 32.2 MJ/m2/day
  expect_equal(ra(-20, "2001-09-03"), 32.2, tolerance = 0.01)

  # Ra is a radiation flux and can never be negative. Before v0.2.2 the
  # polar branch was chosen by hemisphere, giving -47.75 at 80 N in December.
  for (lat in c(-89, -80, -70, 0, 70, 80, 89)) {
    for (d in c("2001-03-21", "2001-06-21", "2001-09-21", "2001-12-21")) {
      expect_gte(ra(lat, d), 0)
    }
  }
  # Polar night at both poles gives exactly zero; polar day gives a maximum
  expect_equal(ra(80, "2001-12-21"), 0)
  expect_equal(ra(-80, "2001-06-21"), 0)
  expect_gt(ra(-80, "2001-12-21"), 40)
  expect_gt(ra(80, "2001-06-21"), 40)

  # and ETo stays physical rather than reaching millions of mm/day
  eto <- ck_pet_pm(tmin = -30, tmax = -20, lat = 80,
                   dates = as.Date("2001-12-21"))$value
  expect_lt(eto, 5)
  expect_gte(eto, 0)
})

test_that("physically impossible input is rejected rather than computed on", {
  d <- as.Date("2024-01-01") + 0:9
  expect_error(ck_total_precip(c(-5, rep(1, 9)), d), "cannot be negative")
  expect_error(ck_total_precip(c(-999, rep(1, 9)), d), "cannot be negative")
  expect_error(ck_spi(c(-999, rep(1, 9)), d), "cannot be negative")
  expect_error(ck_diurnal_range(rep(30, 10), rep(10, 10), d), "exceeds")
  expect_error(ck_pet(rep(30, 10), rep(10, 10), lat = 45, dates = d), "exceeds")
  expect_warning(ck_frost_days(c(-999, rep(1, 9)), d), "outside -100 to 70")

  # valid input is untouched
  expect_silent(ck_total_precip(c(0, 5, 3, 0, 8, 2, 0, 1, 4, 0), d))
  expect_silent(ck_frost_days(c(-2, 3, -1, 5, -3, 0, 2, -4, 1, -1), d))
})

test_that("Rx5day is NA when the period is shorter than five days", {
  expect_true(is.na(
    ck_max_5day_precip(c(1, 2, 3), as.Date("2024-01-01") + 0:2)$value))
  # and unchanged where five days exist
  expect_equal(
    ck_max_5day_precip(rep(2, 10), as.Date("2024-01-01") + 0:9)$value, 10)
})
