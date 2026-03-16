#' Standardized Precipitation Index (SPI)
#'
#' Compute the SPI by fitting a gamma distribution to monthly precipitation
#' totals accumulated over a rolling window, then transforming to standard
#' normal deviates.
#'
#' @param precip Numeric vector of daily precipitation (mm).
#' @param dates Date vector of the same length as `precip`.
#' @param scale Integer. Accumulation period in months (default 3).
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @references McKee, T. B., Doesken, N. J., & Kleist, J. (1993).
#'   The relationship of drought frequency and duration to time scales.
#'
#' @export
#' @examples
#' dates <- seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day")
#' set.seed(42)
#' precip <- rgamma(length(dates), shape = 0.5, rate = 0.1)
#' ck_spi(precip, dates, scale = 3)
ck_spi <- function(precip, dates, scale = 3) {
  validate_numeric(precip, "precip")
  validate_dates(dates, length(precip))
  if (!is.numeric(scale) || length(scale) != 1 || scale < 1) {
    cli::cli_abort("{.arg scale} must be a positive integer.")
  }
  scale <- as.integer(scale)

  # Aggregate to monthly totals
  monthly <- .monthly_totals(precip, dates)

  # Rolling accumulation
  n <- nrow(monthly)
  if (n < scale) {
    cli::cli_abort("Not enough months ({n}) for scale {scale}.")
  }

  accum <- rep(NA_real_, n)
  for (i in scale:n) {
    accum[i] <- sum(monthly$total[(i - scale + 1):i], na.rm = TRUE)
  }

  # Fit gamma per calendar month (WMO-1090 standard)
  cal_month <- as.integer(format(monthly$month, "%m"))
  spi_values <- rep(NA_real_, n)
  for (m in 1:12) {
    idx <- which(cal_month == m & !is.na(accum))
    if (length(idx) >= 3) {
      spi_values[idx] <- .gamma_to_normal(accum[idx])
    }
  }

  result <- data.frame(
    period = monthly$month,
    value = spi_values,
    index = "spi",
    unit = "dimensionless",
    stringsAsFactors = FALSE
  )
  result[!is.na(result$value), ]
}

#' Standardized Precipitation-Evapotranspiration Index (SPEI)
#'
#' Compute the SPEI by fitting a log-logistic distribution to the monthly
#' climatic water balance (precipitation minus potential evapotranspiration)
#' accumulated over a rolling window.
#'
#' @param precip Numeric vector of daily precipitation (mm).
#' @param pet Numeric vector of daily potential evapotranspiration (mm).
#' @param dates Date vector of the same length as `precip` and `pet`.
#' @param scale Integer. Accumulation period in months (default 3).
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @references Vicente-Serrano, S. M., Begueria, S., & Lopez-Moreno, J. I.
#'   (2010). A multiscalar drought index sensitive to global warming: the
#'   Standardized Precipitation Evapotranspiration Index. *Journal of Climate*,
#'   23(7), 1696-1718.
#'
#' @export
#' @examples
#' dates <- seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day")
#' set.seed(42)
#' precip <- rgamma(length(dates), shape = 0.5, rate = 0.1)
#' pet <- rep(3, length(dates))
#' ck_spei(precip, pet, dates, scale = 3)
ck_spei <- function(precip, pet, dates, scale = 3) {
  validate_numeric(precip, "precip")
  validate_numeric(pet, "pet")
  validate_dates(dates, length(precip))
  if (length(precip) != length(pet)) {
    cli::cli_abort("{.arg precip} and {.arg pet} must have the same length.")
  }
  if (!is.numeric(scale) || length(scale) != 1 || scale < 1) {
    cli::cli_abort("{.arg scale} must be a positive integer.")
  }
  scale <- as.integer(scale)

  # Climatic water balance
  wb <- precip - pet

  # Aggregate to monthly totals
  monthly <- .monthly_totals(wb, dates)

  n <- nrow(monthly)
  if (n < scale) {
    cli::cli_abort("Not enough months ({n}) for scale {scale}.")
  }

  accum <- rep(NA_real_, n)
  for (i in scale:n) {
    accum[i] <- sum(monthly$total[(i - scale + 1):i], na.rm = TRUE)
  }

  # Fit log-logistic per calendar month (Vicente-Serrano et al. 2010)
  cal_month <- as.integer(format(monthly$month, "%m"))
  spei_values <- rep(NA_real_, n)
  for (m in 1:12) {
    idx <- which(cal_month == m & !is.na(accum))
    if (length(idx) >= 3) {
      spei_values[idx] <- .loglogistic_to_normal(accum[idx])
    }
  }

  result <- data.frame(
    period = monthly$month,
    value = spei_values,
    index = "spei",
    unit = "dimensionless",
    stringsAsFactors = FALSE
  )
  result[!is.na(result$value), ]
}

#' Potential Evapotranspiration (Hargreaves Method)
#'
#' Estimate daily PET using the Hargreaves-Samani equation, which requires
#' only daily temperature extremes and latitude.
#'
#' @param tmin Numeric vector of daily minimum temperatures (degrees C).
#' @param tmax Numeric vector of daily maximum temperatures (degrees C).
#' @param lat Numeric. Latitude in decimal degrees.
#' @param dates Date vector of the same length as `tmin`.
#'
#' @return A data frame with columns `date`, `value`, `index`, and `unit`.
#'
#' @references Hargreaves, G. H., & Samani, Z. A. (1985). Reference crop
#'   evapotranspiration from temperature. *Applied Engineering in Agriculture*,
#'   1(2), 96-99.
#'
#' @export
#' @examples
#' dates <- as.Date("2024-07-01") + 0:9
#' tmin <- c(15, 16, 14, 17, 15, 13, 16, 14, 15, 16)
#' tmax <- c(30, 32, 28, 33, 31, 27, 34, 29, 30, 32)
#' ck_pet(tmin, tmax, lat = 45, dates = dates)
ck_pet <- function(tmin, tmax, lat, dates) {
  validate_numeric(tmin, "tmin")
  validate_numeric(tmax, "tmax")
  validate_dates(dates, length(tmin))
  if (length(tmin) != length(tmax)) {
    cli::cli_abort("{.arg tmin} and {.arg tmax} must have the same length.")
  }
  if (!is.numeric(lat) || length(lat) != 1) {
    cli::cli_abort("{.arg lat} must be a single numeric value.")
  }

  tavg <- (tmin + tmax) / 2
  td <- tmax - tmin
  td[td < 0] <- 0

  # Day of year
  doy <- as.integer(format(dates, "%j"))

  # Extraterrestrial radiation (Ra) in mm/day equivalent
  ra <- .extraterrestrial_radiation(lat, doy)

  # Hargreaves equation
  pet <- 0.0023 * ra * sqrt(td) * (tavg + 17.8)
  pet[pet < 0] <- 0

  data.frame(
    date = dates,
    value = pet,
    index = "pet",
    unit = "mm",
    stringsAsFactors = FALSE
  )
}

#' Compute extraterrestrial radiation (Ra) in mm/day equivalent
#' @noRd
.extraterrestrial_radiation <- function(lat, doy) {
  # Solar constant in MJ/m2/min
  gsc <- 0.0820

  # Latitude in radians
  phi <- lat * pi / 180

  # Solar declination
  delta <- 0.409 * sin(2 * pi / 365 * doy - 1.39)

  # Sunset hour angle
  ws <- acos(-tan(phi) * tan(delta))
  # Clamp to valid range

  ws[is.nan(ws)] <- if (lat >= 0) pi else 0

  # Inverse relative distance Earth-Sun

  dr <- 1 + 0.033 * cos(2 * pi / 365 * doy)

  # Ra in MJ/m2/day
  ra_mj <- (24 * 60 / pi) * gsc * dr *
    (ws * sin(phi) * sin(delta) + cos(phi) * cos(delta) * sin(ws))

  # Convert MJ/m2/day to mm/day (divide by latent heat of vaporization 2.45)
  ra_mj / 2.45
}

#' Aggregate daily values to monthly totals
#' @noRd
.monthly_totals <- function(x, dates) {
  month_labels <- format(dates, "%Y-%m")
  unique_months <- unique(month_labels)
  totals <- vapply(unique_months, function(m) {
    sum(x[month_labels == m], na.rm = TRUE)
  }, numeric(1))
  data.frame(
    month = as.Date(paste0(unique_months, "-01")),
    total = totals,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' Fit gamma distribution (MLE) and transform to standard normal
#'
#' Uses Maximum Likelihood Estimation via Thom's (1958) approximation,
#' as specified by the WMO User Guide (WMO-No. 1090) and consistent with
#' the SPEI R package.
#' @noRd
.gamma_to_normal <- function(x) {
  result <- rep(NA_real_, length(x))
  valid <- !is.na(x)
  xv <- x[valid]

  if (length(xv) < 3) return(result)

  # Proportion of zeros
  q <- sum(xv == 0) / length(xv)
  xv_pos <- xv[xv > 0]

  if (length(xv_pos) < 2) return(result)

  # MLE via Thom's (1958) approximation
  xbar <- mean(xv_pos)
  A <- log(xbar) - mean(log(xv_pos))
  if (A <= 0) return(result)

  alpha <- (1 + sqrt(1 + 4 * A / 3)) / (4 * A)
  beta <- xbar / alpha

  # Transform to standard normal
  cdf <- rep(NA_real_, length(xv))
  for (i in seq_along(xv)) {
    if (xv[i] == 0) {
      cdf[i] <- q
    } else {
      cdf[i] <- q + (1 - q) * stats::pgamma(xv[i], shape = alpha, scale = beta)
    }
  }
  # Clamp to avoid Inf
  cdf[cdf <= 0] <- 0.001
  cdf[cdf >= 1] <- 0.999

  result[valid] <- stats::qnorm(cdf)
  result
}

#' Fit log-logistic distribution and transform to standard normal
#'
#' Uses unbiased probability weighted moments (Hosking 1990) to estimate
#' log-logistic parameters, consistent with Vicente-Serrano et al. (2010).
#' @noRd
.loglogistic_to_normal <- function(x) {
  result <- rep(NA_real_, length(x))
  valid <- !is.na(x)
  xv <- x[valid]
  n <- length(xv)

  if (n < 3) return(result)

  # Shift to positive for PWM estimation
  xv_shifted <- xv - min(xv) + 1
  xv_sorted <- sort(xv_shifted)

  # Unbiased PWMs (Hosking 1990)
  ranks <- seq_len(n)
  b0 <- mean(xv_sorted)
  b1 <- sum(((ranks - 1) / (n - 1)) * xv_sorted) / n
  b2 <- sum(((ranks - 1) * (ranks - 2) / ((n - 1) * (n - 2))) *
              xv_sorted) / n

  # L-moments
  l1 <- b0
  l2 <- 2 * b1 - b0
  l3 <- 6 * b2 - 6 * b1 + b0

  # Log-logistic parameters from L-moments
  if (l2 <= 0) {
    cli::cli_warn("SPEI fitting failed: L-moment l2 <= 0. Returning NAs.")
    return(result)
  }
  t3 <- l3 / l2  # L-skewness

  if (abs(t3) >= 1) {
    cli::cli_warn("SPEI fitting failed: L-skewness out of range. Returning NAs.")
    return(result)
  }

  beta_ll <- t3 * pi / (3 * sin(t3 * pi / 3))

  if (is.na(beta_ll) || !is.finite(beta_ll) || beta_ll <= 0) {
    cli::cli_warn("SPEI fitting failed: invalid shape parameter. Returning NAs.")
    return(result)
  }

  g1 <- gamma(1 + 1 / beta_ll)
  g2 <- gamma(1 - 1 / beta_ll)
  if (!is.finite(g1) || !is.finite(g2) || g1 * g2 == 0) {
    cli::cli_warn("SPEI fitting failed: gamma function overflow. Returning NAs.")
    return(result)
  }

  alpha_ll <- l2 / (g1 * g2)
  xi <- l1 - alpha_ll * g1 * g2

  if (is.na(alpha_ll) || alpha_ll <= 0) {
    cli::cli_warn("SPEI fitting failed: invalid scale parameter. Returning NAs.")
    return(result)
  }

  # Compute CDF for each observation (in original order)
  cdf <- vapply(seq_along(xv), function(i) {
    a <- (xv_shifted[i] - xi) / alpha_ll
    if (a <= 0) 0.001 else 1 / (1 + a^(-beta_ll))
  }, numeric(1))

  cdf[cdf <= 0] <- 0.001
  cdf[cdf >= 1] <- 0.999
  result[valid] <- stats::qnorm(cdf)
  result
}
