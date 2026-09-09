#' Standardised Precipitation Index (SPI)
#'
#' Compute the SPI by fitting a parametric distribution to rolling monthly
#' precipitation accumulations and transforming to standard normal
#' deviates. Two distributions are supported: the two-parameter gamma
#' (default; WMO-1090 standard) and the three-parameter Pearson III. The
#' Pearson III tail is heavier and is preferred in arid regions where the
#' wet-day distribution is highly skewed (Stagge et al. 2015).
#'
#' @param precip Numeric vector of daily precipitation (mm).
#' @param dates Date vector of the same length as `precip`.
#' @param scale Integer. Accumulation period in months (default 3).
#' @param distribution Character. Either `"gamma"` (default) or
#'   `"pearsonIII"`.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @references
#' McKee, T. B., Doesken, N. J., & Kleist, J. (1993). The relationship of
#' drought frequency and duration to time scales.
#'
#' Stagge, J. H., Tallaksen, L. M., Gudmundsson, L., Van Loon, A. F., &
#' Stahl, K. (2015). Candidate distributions for climatological drought
#' indices (SPI and SPEI). *International Journal of Climatology*, 35(13),
#' 4027-4040. \doi{10.1002/joc.4267}.
#'
#' @export
#' @examples
#' dates <- seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day")
#' set.seed(42)
#' precip <- rgamma(length(dates), shape = 0.5, rate = 0.1)
#' ck_spi(precip, dates, scale = 3)
ck_spi <- function(precip, dates, scale = 3,
                   distribution = c("gamma", "pearsonIII")) {
  validate_precip(precip, "precip")
  validate_dates(dates, length(precip))
  if (!is.numeric(scale) || length(scale) != 1 || scale < 1) {
    cli::cli_abort("{.arg scale} must be a positive integer.")
  }
  scale <- as.integer(scale)
  distribution <- match.arg(distribution)

  monthly <- .monthly_totals(precip, dates)

  n <- nrow(monthly)
  if (n < scale) {
    cli::cli_abort("Not enough months ({n}) for scale {scale}.")
  }

  accum <- rep(NA_real_, n)
  for (i in scale:n) {
    accum[i] <- sum(monthly$total[(i - scale + 1):i], na.rm = TRUE)
  }

  cal_month <- as.integer(format(monthly$month, "%m"))
  spi_values <- rep(NA_real_, n)
  fit_fun <- switch(distribution,
    "gamma"      = .gamma_to_normal,
    "pearsonIII" = .pearson3_to_normal
  )
  for (m in 1:12) {
    idx <- which(cal_month == m & !is.na(accum))
    if (length(idx) >= 3) {
      spi_values[idx] <- fit_fun(accum[idx])
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

#' Standardised Precipitation-Evapotranspiration Index (SPEI)
#'
#' Compute the SPEI by fitting a log-logistic distribution to the monthly
#' climatic water balance (precipitation minus potential evapotranspiration)
#' accumulated over a rolling window.
#'
#' @param precip Numeric vector of daily precipitation (mm).
#' @param pet Numeric vector of daily potential evapotranspiration (mm).
#' @param dates Date vector of the same length as `precip` and `pet`.
#' @param scale Integer. Accumulation period in months (default 3).
#' @param distribution Character. Either `"log-logistic"` (default,
#'   Vicente-Serrano et al. 2010) or `"gev"` (Generalised Extreme
#'   Value, fitted by L-moments; preferred for water-balance series
#'   with heavy upper or lower tails).
#'
#'   The three-parameter log-logistic is fitted by L-moments, which
#'   requires an L-skewness in `(0, 1)`. A calendar month whose water
#'   balance is symmetric or left-skewed falls outside that range; the
#'   fit warns and returns `NA` for that month rather than reporting a
#'   value from a distribution that does not describe the data. Use
#'   `distribution = "gev"`, which accommodates both tails, when this
#'   happens often in your series.
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
ck_spei <- function(precip, pet, dates, scale = 3,
                    distribution = c("log-logistic", "gev")) {
  validate_precip(precip, "precip")
  validate_numeric(pet, "pet")
  validate_dates(dates, length(precip))
  if (length(precip) != length(pet)) {
    cli::cli_abort("{.arg precip} and {.arg pet} must have the same length.")
  }
  if (!is.numeric(scale) || length(scale) != 1 || scale < 1) {
    cli::cli_abort("{.arg scale} must be a positive integer.")
  }
  scale <- as.integer(scale)
  distribution <- match.arg(distribution)

  wb <- precip - pet
  monthly <- .monthly_totals(wb, dates)

  n <- nrow(monthly)
  if (n < scale) {
    cli::cli_abort("Not enough months ({n}) for scale {scale}.")
  }

  accum <- rep(NA_real_, n)
  for (i in scale:n) {
    accum[i] <- sum(monthly$total[(i - scale + 1):i], na.rm = TRUE)
  }

  cal_month <- as.integer(format(monthly$month, "%m"))
  spei_values <- rep(NA_real_, n)
  fit_fun <- switch(distribution,
    "log-logistic" = .loglogistic_to_normal,
    "gev"          = .gev_to_normal
  )
  for (m in 1:12) {
    idx <- which(cal_month == m & !is.na(accum))
    if (length(idx) >= 3) {
      spei_values[idx] <- fit_fun(accum[idx])
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
  validate_temperature(tmin, "tmin")
  validate_temperature(tmax, "tmax")
  validate_dates(dates, length(tmin))
  if (length(tmin) != length(tmax)) {
    cli::cli_abort("{.arg tmin} and {.arg tmax} must have the same length.")
  }
  validate_tmin_tmax(tmin, tmax)
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

#' Reference Evapotranspiration (FAO-56 Penman-Monteith)
#'
#' Compute reference evapotranspiration ETo using the FAO-56
#' Penman-Monteith equation (Allen et al. 1998), the international
#' standard for ETo estimation. Required inputs are daily Tmin and
#' Tmax; optional inputs (humidity, wind speed, incoming solar
#' radiation, elevation) increase accuracy. Where humidity, wind, or
#' solar radiation are missing, FAO-56 fallback estimators are used.
#'
#' Inputs and units:
#' \itemize{
#'   \item `tmin`, `tmax`: daily minimum and maximum temperature (degrees C).
#'   \item `lat`: latitude in decimal degrees, used for the
#'         extraterrestrial-radiation calculation.
#'   \item `elev`: elevation above sea level in metres (default 0).
#'   \item `wind`: 2-metre wind speed (m/s). Default 2 m/s (the
#'         FAO-56 fallback value when wind data are unavailable).
#'   \item `rh_min`, `rh_max`: minimum and maximum daily relative
#'         humidity (\%). If both are NULL (default), actual vapour
#'         pressure is estimated as `e0(tmin)` (FAO-56 Eq. 48).
#'   \item `rs`: daily incoming solar radiation (MJ m^-2 day^-1).
#'         If NULL, estimated by Hargreaves-Samani: `Rs = krs * Ra
#'         * sqrt(Tmax - Tmin)` with `krs = 0.16` for inland sites.
#'   \item `albedo`: surface albedo (default 0.23, the FAO grass
#'         reference).
#' }
#'
#' @param tmin Numeric vector of daily minimum temperatures (degrees C).
#' @param tmax Numeric vector of daily maximum temperatures (degrees C),
#'   same length as `tmin`.
#' @param lat Numeric. Latitude in decimal degrees.
#' @param dates Date vector of the same length as `tmin`.
#' @param elev Numeric. Elevation above sea level in metres
#'   (default 0).
#' @param wind Numeric vector or single value. 2-m wind speed (m/s).
#'   Default `2` (FAO-56 fallback for unmeasured wind).
#' @param rh_min,rh_max Optional numeric vectors of daily minimum and
#'   maximum relative humidity in percent. Both must be supplied
#'   together; otherwise vapour pressure falls back to `e0(tmin)`.
#' @param rs Optional numeric vector of daily incoming solar
#'   radiation (MJ m^-2 day^-1). If `NULL`, Hargreaves-Samani
#'   estimate is used.
#' @param albedo Numeric. Surface albedo (default 0.23).
#' @param krs Numeric. Hargreaves-Samani coefficient for the Rs
#'   fallback (default 0.16 for inland sites; 0.19 for coastal).
#'
#' @return A data frame with columns `date`, `value`, `index`
#'   (`"pet_pm"`), and `unit`.
#'
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M.
#'   (1998). Crop evapotranspiration: guidelines for computing crop
#'   water requirements. *FAO Irrigation and Drainage Paper 56*.
#'
#' @export
#' @examples
#' dates <- as.Date("2024-07-01") + 0:9
#' tmin <- c(15, 16, 14, 17, 15, 13, 16, 14, 15, 16)
#' tmax <- c(30, 32, 28, 33, 31, 27, 34, 29, 30, 32)
#' ck_pet_pm(tmin, tmax, lat = 45, dates = dates)
ck_pet_pm <- function(tmin, tmax, lat, dates,
                      elev = 0, wind = 2,
                      rh_min = NULL, rh_max = NULL, rs = NULL,
                      albedo = 0.23, krs = 0.16) {
  validate_temperature(tmin, "tmin")
  validate_temperature(tmax, "tmax")
  validate_dates(dates, length(tmin))
  if (length(tmin) != length(tmax)) {
    cli::cli_abort("{.arg tmin} and {.arg tmax} must have the same length.")
  }
  validate_tmin_tmax(tmin, tmax)
  if (!is.numeric(lat) || length(lat) != 1L) {
    cli::cli_abort("{.arg lat} must be a single numeric value.")
  }
  if (!is.numeric(elev) || length(elev) != 1L) {
    cli::cli_abort("{.arg elev} must be a single numeric value.")
  }
  if (!is.numeric(albedo) || length(albedo) != 1L ||
      albedo < 0 || albedo > 1) {
    cli::cli_abort("{.arg albedo} must be a single number in [0, 1].")
  }
  if (!is.numeric(krs) || length(krs) != 1L || krs <= 0) {
    cli::cli_abort("{.arg krs} must be a single positive numeric value.")
  }

  n <- length(tmin)

  # Wind: scalar or vector of length n
  if (length(wind) == 1L) wind <- rep(as.numeric(wind), n)
  validate_numeric(wind, "wind")
  if (length(wind) != n) {
    cli::cli_abort("{.arg wind} must be length 1 or length {n}.")
  }

  tmean <- (tmin + tmax) / 2

  # Saturation vapour pressure: e0(T) = 0.6108 * exp(17.27 * T / (T + 237.3)) (kPa)
  e0 <- function(T) 0.6108 * exp(17.27 * T / (T + 237.3))
  e0_tmax <- e0(tmax)
  e0_tmin <- e0(tmin)
  es <- (e0_tmax + e0_tmin) / 2  # FAO-56 Eq. 12

  # Slope of vapour pressure curve at Tmean (kPa / degC, FAO-56 Eq. 13)
  delta <- 4098 * e0(tmean) / (tmean + 237.3)^2

  # Atmospheric pressure (kPa, FAO-56 Eq. 7)
  P <- 101.3 * ((293 - 0.0065 * elev) / 293)^5.26
  # Psychrometric constant (kPa / degC, FAO-56 Eq. 8)
  gamma <- 0.000665 * P

  # Actual vapour pressure
  if (!is.null(rh_min) && !is.null(rh_max)) {
    validate_numeric(rh_min, "rh_min")
    validate_numeric(rh_max, "rh_max")
    if (length(rh_min) != n || length(rh_max) != n) {
      cli::cli_abort("{.arg rh_min} and {.arg rh_max} must be length {n}.")
    }
    ea <- (e0_tmin * rh_max + e0_tmax * rh_min) / 200  # FAO-56 Eq. 17
  } else {
    # FAO-56 Eq. 48 fallback: assume Tdew = Tmin
    ea <- e0_tmin
  }

  # Extraterrestrial radiation (MJ/m^2/day)
  doy <- as.integer(format(dates, "%j"))
  ra_mm <- .extraterrestrial_radiation(lat, doy)  # mm/day
  ra_mj <- ra_mm * 2.45                          # convert to MJ/m^2/day

  # Solar radiation: supplied or Hargreaves-Samani estimate
  if (is.null(rs)) {
    td <- pmax(tmax - tmin, 0)
    rs_calc <- krs * ra_mj * sqrt(td)
  } else {
    validate_numeric(rs, "rs")
    if (length(rs) != n) {
      cli::cli_abort("{.arg rs} must be length {n}.")
    }
    rs_calc <- rs
  }

  # Clear-sky solar radiation Rso (FAO-56 Eq. 37)
  rso <- (0.75 + 2e-5 * elev) * ra_mj
  # Cap rs/rso at 1 to keep the longwave correction term sensible
  ratio <- pmin(rs_calc / pmax(rso, 1e-6), 1)

  # Net shortwave radiation (MJ/m^2/day, FAO-56 Eq. 38)
  rns <- (1 - albedo) * rs_calc

  # Net longwave radiation (MJ/m^2/day, FAO-56 Eq. 39)
  sigma_sb <- 4.903e-9
  rnl <- sigma_sb *
    ((tmax + 273.16)^4 + (tmin + 273.16)^4) / 2 *
    (0.34 - 0.14 * sqrt(pmax(ea, 0))) *
    (1.35 * ratio - 0.35)

  rn <- rns - rnl

  # FAO-56 Penman-Monteith (Eq. 6); G = 0 for daily timestep
  numerator <- 0.408 * delta * rn +
               gamma * (900 / (tmean + 273)) * wind * (es - ea)
  denominator <- delta + gamma * (1 + 0.34 * wind)
  pet <- numerator / denominator
  pet[!is.finite(pet) | pet < 0] <- 0

  data.frame(
    date  = dates,
    value = pet,
    index = "pet_pm",
    unit  = "mm",
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

  # Sunset hour angle. Clamp the argument to [-1, 1] so polar latitudes
  # resolve correctly: an argument of +1 or more means the sun never rises
  # (polar night, ws = 0) and -1 or less means it never sets (polar day,
  # ws = pi). Selecting on the argument rather than the hemisphere is what
  # makes both poles come out right in both solstices, and it also avoids
  # acos() emitting NaN warnings.
  ws <- acos(pmin(pmax(-tan(phi) * tan(delta), -1), 1))

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

#' Fit Pearson III distribution (method of moments) and transform to standard normal
#'
#' Method-of-moments estimator for the three-parameter Pearson III. Used as
#' an alternative SPI fit for arid-region or heavy-tailed precipitation
#' regimes where the gamma fit is poor (Stagge et al. 2015).
#' @noRd
.pearson3_to_normal <- function(x) {
  result <- rep(NA_real_, length(x))
  valid <- !is.na(x)
  xv <- x[valid]
  n <- length(xv)

  if (n < 3L) return(result)

  mu <- mean(xv)
  sigma <- stats::sd(xv)
  if (!is.finite(sigma) || sigma <= 0) return(result)

  m3 <- mean((xv - mu)^3)
  gamma_skew <- m3 / sigma^3

  if (!is.finite(gamma_skew) || abs(gamma_skew) < 1e-6) {
    # Approximately normal: fall back to standardisation.
    cdf <- stats::pnorm((xv - mu) / sigma)
  } else {
    alpha <- 4 / gamma_skew^2
    beta  <- sigma * abs(gamma_skew) / 2
    xi    <- mu - alpha * sign(gamma_skew) * beta

    if (!is.finite(alpha) || alpha <= 0 || !is.finite(beta) || beta <= 0) {
      return(result)
    }

    if (gamma_skew > 0) {
      cdf <- stats::pgamma((xv - xi) / beta, shape = alpha)
    } else {
      cdf <- 1 - stats::pgamma((xi - xv) / beta, shape = alpha)
    }
  }

  cdf[is.na(cdf)] <- 0.5
  cdf[cdf <= 0] <- 0.001
  cdf[cdf >= 1] <- 0.999

  result[valid] <- stats::qnorm(cdf)
  result
}

#' Fit Generalised Extreme Value distribution (L-moments) and transform to standard normal
#'
#' Hosking (1985, 1990) L-moment estimator. Used as an alternative SPEI
#' fit when the climatic water balance has long-tailed extremes that the
#' log-logistic does not capture.
#' @noRd
.gev_to_normal <- function(x) {
  result <- rep(NA_real_, length(x))
  valid <- !is.na(x)
  xv <- x[valid]
  n <- length(xv)

  if (n < 4L) return(result)

  xs <- sort(xv)
  ranks <- seq_len(n)
  b0 <- mean(xs)
  b1 <- sum(((ranks - 1) / (n - 1)) * xs) / n
  b2 <- sum(((ranks - 1) * (ranks - 2) / ((n - 1) * (n - 2))) * xs) / n

  l1 <- b0
  l2 <- 2 * b1 - b0
  l3 <- 6 * b2 - 6 * b1 + b0

  if (!is.finite(l2) || l2 <= 0) {
    cli::cli_warn("SPEI GEV fit failed: L-moment l2 <= 0. Returning NAs.")
    return(result)
  }
  t3 <- l3 / l2
  if (!is.finite(t3) || abs(t3) >= 1) {
    cli::cli_warn("SPEI GEV fit failed: L-skewness out of range. Returning NAs.")
    return(result)
  }

  # Hosking 1985 approximation; k follows Hosking sign convention
  # (opposite of the textbook xi).
  c_const <- 2 / (3 + t3) - log(2) / log(3)
  k <- 7.8590 * c_const + 2.9554 * c_const^2

  if (abs(k) < 1e-6) {
    # Gumbel limit: F(x) = exp(-exp(-(x - u) / alpha))
    alpha <- l2 / log(2)
    if (!is.finite(alpha) || alpha <= 0) return(result)
    u <- l1 - alpha * 0.5772156649  # Euler-Mascheroni
    cdf <- exp(-exp(-(xv - u) / alpha))
  } else {
    g1 <- gamma(1 + k)
    if (!is.finite(g1)) return(result)
    alpha <- l2 * k / ((1 - 2^(-k)) * g1)
    if (!is.finite(alpha) || alpha <= 0) return(result)
    u <- l1 - alpha * (1 - g1) / k
    z <- 1 - k * (xv - u) / alpha
    z[z <= 0] <- NA_real_
    cdf <- exp(-z^(1 / k))
  }

  cdf[is.na(cdf)] <- 0.5
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

  # The three-parameter log-logistic has tau3 = 1 / beta (Vicente-Serrano
  # et al. 2010, appendix). beta must exceed 1 for Gamma(1 - 1/beta) to be
  # finite and positive, which requires 0 < tau3 < 1.
  if (!is.finite(t3) || t3 <= 0 || t3 >= 1) {
    cli::cli_warn("SPEI fitting failed: L-skewness out of range. Returning NAs.")
    return(result)
  }

  beta_ll <- 1 / t3

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

  alpha_ll <- l2 * beta_ll / (g1 * g2)
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
