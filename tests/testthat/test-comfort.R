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

test_that("ck_fire_weather computes positive values", {
  result <- ck_fire_weather(
    tavg = c(30, 25),
    humidity = c(20, 40),
    wind_speed = c(25, 10),
    precip = c(0, 0)
  )
  expect_s3_class(result, "data.frame")
  expect_true(all(result$value > 0))
  expect_equal(result$index[1], "fire_weather")
})

test_that("ck_fire_weather dampened by rain", {
  dry <- ck_fire_weather(tavg = 30, humidity = 20, wind_speed = 25, precip = 0)
  wet <- ck_fire_weather(tavg = 30, humidity = 20, wind_speed = 25, precip = 10)
  expect_true(dry$value > wet$value)
})

test_that("ck_fire_weather rejects mismatched lengths", {
  expect_error(
    ck_fire_weather(c(1, 2), c(50), c(10, 20), c(0, 0)),
    "same length"
  )
})
