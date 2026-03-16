#' Wind Chill Temperature
#'
#' Compute wind chill using the North American Wind Chill Index formula
#' (Environment Canada / US NWS). Valid for temperatures at or below 10 degrees C
#' and wind speeds above 4.8 km/h.
#'
#' @param tavg Numeric vector of temperatures (degrees C).
#' @param wind_speed Numeric vector of wind speeds (km/h).
#'
#' @return A data frame with columns `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' ck_wind_chill(tavg = c(-5, -10, 0), wind_speed = c(20, 30, 15))
ck_wind_chill <- function(tavg, wind_speed) {
  validate_numeric(tavg, "tavg")
  validate_numeric(wind_speed, "wind_speed")
  if (length(tavg) != length(wind_speed)) {
    cli::cli_abort("{.arg tavg} and {.arg wind_speed} must have the same length.")
  }

  # North American formula
  wc <- 13.12 + 0.6215 * tavg -
    11.37 * wind_speed^0.16 +
    0.3965 * tavg * wind_speed^0.16

  # Only valid for T <= 10C and wind > 4.8 km/h; return tavg otherwise
  invalid <- tavg > 10 | wind_speed <= 4.8
  wc[invalid] <- tavg[invalid]

  data.frame(
    value = wc,
    index = "wind_chill",
    unit = "\u00b0C",
    stringsAsFactors = FALSE
  )
}

#' Heat Index
#'
#' Compute the heat index (apparent temperature) using the Rothfusz
#' regression equation used by the US National Weather Service.
#'
#' @param tavg Numeric vector of temperatures (degrees C).
#' @param humidity Numeric vector of relative humidity (percent, 0-100).
#'
#' @return A data frame with columns `value`, `index`, and `unit`.
#'
#' @references Rothfusz, L. P. (1990). The heat index equation.
#'   NWS Technical Attachment SR 90-23.
#'
#' @export
#' @examples
#' ck_heat_index(tavg = c(30, 35, 40), humidity = c(60, 70, 50))
ck_heat_index <- function(tavg, humidity) {
  validate_numeric(tavg, "tavg")
  validate_numeric(humidity, "humidity")
  if (length(tavg) != length(humidity)) {
    cli::cli_abort("{.arg tavg} and {.arg humidity} must have the same length.")
  }

  # Convert to Fahrenheit for the NWS equation
  tf <- tavg * 9 / 5 + 32

  # Steadman's simple formula first
  hi_simple <- 0.5 * (tf + 61.0 + (tf - 68.0) * 1.2 + humidity * 0.094)

  # Use Rothfusz regression when simple formula > 80F
  hi <- ifelse(hi_simple > 80,
    -42.379 + 2.04901523 * tf + 10.14333127 * humidity -
      0.22475541 * tf * humidity - 6.83783e-03 * tf^2 -
      5.481717e-02 * humidity^2 + 1.22874e-03 * tf^2 * humidity +
      8.5282e-04 * tf * humidity^2 - 1.99e-06 * tf^2 * humidity^2,
    hi_simple
  )

  # NWS low-humidity adjustment
  low_rh <- !is.na(humidity) & !is.na(tf) & humidity < 13 & tf > 80 & tf < 112
  if (any(low_rh)) {
    adj1 <- ((13 - humidity[low_rh]) / 4) *
      sqrt(pmax((17 - abs(tf[low_rh] - 95)) / 17, 0))
    hi[low_rh] <- hi[low_rh] - adj1
  }

  # NWS high-humidity adjustment
  high_rh <- !is.na(humidity) & !is.na(tf) & humidity > 85 & tf >= 80 & tf <= 87
  if (any(high_rh)) {
    adj2 <- ((humidity[high_rh] - 85) / 10) * ((87 - tf[high_rh]) / 5)
    hi[high_rh] <- hi[high_rh] + adj2
  }

  # Convert back to Celsius
  hi_c <- (hi - 32) * 5 / 9

  data.frame(
    value = hi_c,
    index = "heat_index",
    unit = "\u00b0C",
    stringsAsFactors = FALSE
  )
}

#' Humidex
#'
#' Compute the Canadian humidex from temperature and dewpoint.
#'
#' @param tavg Numeric vector of temperatures (degrees C).
#' @param dewpoint Numeric vector of dewpoint temperatures (degrees C).
#'
#' @return A data frame with columns `value`, `index`, and `unit`.
#'
#' @references Masterson, J., & Richardson, F. A. (1979). Humidex: A method
#'   of quantifying human discomfort due to excessive heat and humidity.
#'   Environment Canada.
#'
#' @export
#' @examples
#' ck_humidex(tavg = c(30, 35), dewpoint = c(20, 25))
ck_humidex <- function(tavg, dewpoint) {
  validate_numeric(tavg, "tavg")
  validate_numeric(dewpoint, "dewpoint")
  if (length(tavg) != length(dewpoint)) {
    cli::cli_abort("{.arg tavg} and {.arg dewpoint} must have the same length.")
  }

  # Vapour pressure from dewpoint
  e <- 6.11 * exp(5417.7530 * (1 / 273.16 - 1 / (273.15 + dewpoint)))

  hx <- tavg + 5 / 9 * (e - 10)

  data.frame(
    value = hx,
    index = "humidex",
    unit = "unitless",
    stringsAsFactors = FALSE
  )
}

#' Fire Danger Index (Simplified)
#'
#' A simplified fire danger proxy based on temperature, humidity, wind speed,
#' and recent precipitation. This is NOT the Canadian Forest Fire Weather
#' Index (Van Wagner 1987); for the full FWI system, use the cffdrs package.
#'
#' @param tavg Numeric vector of temperatures (degrees C).
#' @param humidity Numeric vector of relative humidity (percent, 0-100).
#' @param wind_speed Numeric vector of wind speeds (km/h).
#' @param precip Numeric vector of daily precipitation (mm).
#'
#' @return A data frame with columns `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' ck_fire_danger(
#'   tavg = c(30, 25, 35),
#'   humidity = c(20, 40, 15),
#'   wind_speed = c(25, 10, 30),
#'   precip = c(0, 5, 0)
#' )
ck_fire_danger <- function(tavg, humidity, wind_speed, precip) {
  validate_numeric(tavg, "tavg")
  validate_numeric(humidity, "humidity")
  validate_numeric(wind_speed, "wind_speed")
  validate_numeric(precip, "precip")

  lens <- c(length(tavg), length(humidity), length(wind_speed), length(precip))
  if (length(unique(lens)) != 1) {
    cli::cli_abort("All input vectors must have the same length.")
  }

  # Simplified FWI: higher temp + wind = higher risk; higher humidity + precip = lower
  # This is a linearised approximation, not the full iterative CFFWIS
  moisture_factor <- pmax(1 - humidity / 100, 0)
  rain_dampening <- pmax(1 - precip / 10, 0)
  wind_factor <- wind_speed / 30

  fwi <- (tavg / 30) * moisture_factor * rain_dampening * (1 + wind_factor) * 10
  fwi[fwi < 0] <- 0

  data.frame(
    value = fwi,
    index = "fire_danger",
    unit = "unitless",
    stringsAsFactors = FALSE
  )
}
