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

# ck_etccdi_27 audit table ----------------------------------------------------

test_that("ck_etccdi_27 returns canonical 27 rows with required columns", {
  tab <- ck_etccdi_27()
  expect_s3_class(tab, "data.frame")
  expect_equal(nrow(tab), 27)
  expect_true(all(c("code", "name", "variable", "unit",
                    "definition", "ck_function", "status") %in% names(tab)))
})

test_that("ck_etccdi_27 codes are unique and canonical", {
  tab <- ck_etccdi_27()
  expect_equal(length(unique(tab$code)), 27)
  canonical <- c("TXx", "TNx", "TXn", "TNn", "FD", "ID", "SU", "TR",
                 "TX10p", "TN10p", "TX90p", "TN90p", "DTR", "WSDI", "CSDI",
                 "GSL", "RX1day", "RX5day", "SDII", "R10mm", "R20mm",
                 "Rnnmm", "CDD", "CWD", "R95p", "R99p", "PRCPTOT")
  expect_setequal(tab$code, canonical)
})

test_that("ck_etccdi_27 implemented entries point to real functions", {
  tab <- ck_etccdi_27()
  impl <- subset(tab, status == "implemented")
  for (fn in unique(impl$ck_function)) {
    expect_true(exists(fn, mode = "function"),
                info = paste("Function not exported:", fn))
  }
})

test_that("ck_etccdi_27 status values are within allowed set", {
  tab <- ck_etccdi_27()
  expect_true(all(tab$status %in% c("implemented", "approximation", "missing")))
})

test_that("ck_etccdi_27 missing entries have NA ck_function", {
  tab <- ck_etccdi_27()
  miss <- subset(tab, status == "missing")
  expect_true(all(is.na(miss$ck_function)))
})

# Dispatch for new extreme-value indices --------------------------------------

test_that("ck_compute dispatches txx/tnx/txn/tnn", {
  d <- data.frame(
    dates = as.Date("2024-01-01") + 0:9,
    tmax = c(5, 10, 18, 12, 4, 8, 22, 3, 7, 6),
    tmin = c(-2, 3, -1, 5, -8, 0, 2, -12, 1, -1)
  )
  expect_equal(ck_compute(d, "txx")$value, 22)
  expect_equal(ck_compute(d, "tnx")$value, 5)
  expect_equal(ck_compute(d, "txn")$value, 3)
  expect_equal(ck_compute(d, "tnn")$value, -12)
})

test_that("ck_compute dispatches percentile indices", {
  set.seed(99)
  dates <- seq(as.Date("1961-01-01"), as.Date("1962-12-31"), by = "day")
  d <- data.frame(
    dates = dates,
    tmax = 15 + 10 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
           rnorm(length(dates)),
    tmin = 5 + 8 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
           rnorm(length(dates)),
    precip = pmax(rgamma(length(dates), shape = 0.4, scale = 8) - 1, 0)
  )
  expect_s3_class(
    ck_compute(d, "tx10p", ref_start = 1961L, ref_end = 1962L),
    "data.frame"
  )
  expect_s3_class(
    ck_compute(d, "tn90p", ref_start = 1961L, ref_end = 1962L),
    "data.frame"
  )
  expect_s3_class(
    ck_compute(d, "r95p", ref_start = 1961L, ref_end = 1962L),
    "data.frame"
  )
})

test_that("ck_etccdi_27 reports 27/27 implemented after Phase 2C", {
  tab <- ck_etccdi_27()
  expect_equal(sum(tab$status == "implemented"), 27L)
  expect_equal(sum(tab$status == "missing"), 0L)
})
