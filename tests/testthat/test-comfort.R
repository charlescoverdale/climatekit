test_that("ck_wind_chill computes correctly", {
  result <- ck_wind_chill(tavg = c(-5, -10), wind_speed = c(20, 30))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(result$value < c(-5, -10)))  # wind chill is colder
  expect_equal(result$index[1], "wind_chill")
})

test_that("ck_wind_chill returns tavg for warm temps", {
  result <- ck_wind_chill(tavg = c(15), wind_speed = c(20))
  expect_equal(result$value, 15)
})

test_that("ck_wind_chill returns tavg for calm winds", {
  result <- ck_wind_chill(tavg = c(-5), wind_speed = c(3))
  expect_equal(result$value, -5)
})

test_that("ck_wind_chill rejects mismatched lengths", {
  expect_error(ck_wind_chill(c(1, 2), c(10)), "same length")
})

test_that("ck_heat_index computes correctly", {
  result <- ck_heat_index(tavg = c(30, 35), humidity = c(60, 70))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(result$value > c(30, 35)))  # heat index is hotter
  expect_equal(result$index[1], "heat_index")
})

test_that("ck_heat_index for cool temps uses simple formula", {
  result <- ck_heat_index(tavg = c(20), humidity = c(50))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

test_that("ck_heat_index rejects mismatched lengths", {
  expect_error(ck_heat_index(c(1, 2), c(50)), "same length")
})

test_that("ck_humidex computes correctly", {
  result <- ck_humidex(tavg = c(30, 35), dewpoint = c(20, 25))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(result$value > c(30, 35)))  # humidex is higher
  expect_equal(result$index[1], "humidex")
})

test_that("ck_humidex rejects mismatched lengths", {
  expect_error(ck_humidex(c(1, 2), c(10)), "same length")
})

test_that("ck_fire_danger computes positive values", {
  result <- ck_fire_danger(
    tavg = c(30, 25),
    humidity = c(20, 40),
    wind_speed = c(25, 10),
    precip = c(0, 0)
  )
  expect_s3_class(result, "data.frame")
  expect_true(all(result$value > 0))
  expect_equal(result$index[1], "fire_danger")
})

test_that("ck_fire_danger dampened by rain", {
  dry <- ck_fire_danger(tavg = 30, humidity = 20, wind_speed = 25, precip = 0)
  wet <- ck_fire_danger(tavg = 30, humidity = 20, wind_speed = 25, precip = 10)
  expect_true(dry$value > wet$value)
})

test_that("ck_fire_danger rejects mismatched lengths", {
  expect_error(
    ck_fire_danger(c(1, 2), c(50), c(10, 20), c(0, 0)),
    "same length"
  )
})

# --- Reference value tests ---

test_that("wind chill reference: T=-10C, V=20 km/h", {
  # WC = 13.12 + 0.6215*(-10) - 11.37*(20^0.16) + 0.3965*(-10)*(20^0.16)
  expected <- 13.12 + 0.6215 * (-10) - 11.37 * (20^0.16) + 0.3965 * (-10) * (20^0.16)
  result <- ck_wind_chill(tavg = -10, wind_speed = 20)
  expect_equal(result$value, expected, tolerance = 0.01)
})

test_that("heat index reference: T=90F (32.2C), RH=65%", {
  # NWS reference: ~104.7F => ~40.4C (approximately)
  result <- ck_heat_index(tavg = 32.222, humidity = 65)
  # Should be substantially above input temperature
  expect_true(result$value > 35)
  expect_true(result$value < 50)
})

test_that("heat index low-humidity adjustment applies", {
  # T=95F (35C), RH=10% — should trigger low-humidity adjustment
  result <- ck_heat_index(tavg = 35, humidity = 10)
  expect_s3_class(result, "data.frame")
  expect_true(result$value > 30)
})

test_that("heat index high-humidity adjustment applies", {
  # T=82F (~27.8C), RH=90% — should trigger high-humidity adjustment
  result <- ck_heat_index(tavg = 27.8, humidity = 90)
  expect_s3_class(result, "data.frame")
})

test_that("humidex reference: T=30C, Td=20C", {
  # e = 6.11 * exp(5417.7530 * (1/273.16 - 1/293.15))
  # humidex = 30 + 5/9 * (e - 10)
  e <- 6.11 * exp(5417.7530 * (1 / 273.16 - 1 / 293.15))
  expected <- 30 + 5 / 9 * (e - 10)
  result <- ck_humidex(tavg = 30, dewpoint = 20)
  expect_equal(result$value, expected, tolerance = 0.1)
})

test_that("ck_compute dispatches fire_danger", {
  d <- list(
    tavg = c(30, 25),
    humidity = c(20, 40),
    wind_speed = c(25, 10),
    precip = c(0, 0)
  )
  result <- ck_compute(d, "fire_danger")
  expect_equal(result$index[1], "fire_danger")
})
