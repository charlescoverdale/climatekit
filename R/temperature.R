#' Frost Days
#'
#' Count the number of days where minimum temperature is below 0 degrees C.
#'
#' @param tmin Numeric vector of daily minimum temperatures (degrees C).
#' @param dates Date vector of the same length as `tmin`.
#' @param period Character. Aggregation period: `"annual"` (default) or
#'   `"monthly"`.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' dates <- as.Date("2024-01-01") + 0:9
#' tmin <- c(-2, 3, -1, 5, -3, 0, 2, -4, 1, -1)
#' ck_frost_days(tmin, dates)
ck_frost_days <- function(tmin, dates, period = "annual") {
  validate_numeric(tmin, "tmin")
  validate_dates(dates, length(tmin))
  period <- validate_period(period)

  result <- count_by_period(tmin < 0, dates, period)
  build_result(result$periods, result$values, "frost_days", "days", period)
}

#' Ice Days
#'
#' Count the number of days where maximum temperature is below 0 degrees C.
#'
#' @param tmax Numeric vector of daily maximum temperatures (degrees C).
#' @param dates Date vector of the same length as `tmax`.
#' @param period Character. Aggregation period: `"annual"` (default) or
#'   `"monthly"`.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' dates <- as.Date("2024-01-01") + 0:9
#' tmax <- c(-2, 3, -1, 5, -3, 0, 2, -4, 1, -1)
#' ck_ice_days(tmax, dates)
ck_ice_days <- function(tmax, dates, period = "annual") {
  validate_numeric(tmax, "tmax")
  validate_dates(dates, length(tmax))
  period <- validate_period(period)

  result <- count_by_period(tmax < 0, dates, period)
  build_result(result$periods, result$values, "ice_days", "days", period)
}

#' Summer Days
#'
#' Count the number of days where maximum temperature exceeds 25 degrees C.
#'
#' @param tmax Numeric vector of daily maximum temperatures (degrees C).
#' @param dates Date vector of the same length as `tmax`.
#' @param period Character. Aggregation period: `"annual"` (default) or
#'   `"monthly"`.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' dates <- as.Date("2024-07-01") + 0:9
#' tmax <- c(22, 26, 28, 24, 30, 25, 27, 23, 31, 29)
#' ck_summer_days(tmax, dates)
ck_summer_days <- function(tmax, dates, period = "annual") {
  validate_numeric(tmax, "tmax")
  validate_dates(dates, length(tmax))
  period <- validate_period(period)

  result <- count_by_period(tmax > 25, dates, period)
  build_result(result$periods, result$values, "summer_days", "days", period)
}

#' Tropical Nights
#'
#' Count the number of days where minimum temperature exceeds 20 degrees C.
#'
#' @param tmin Numeric vector of daily minimum temperatures (degrees C).
#' @param dates Date vector of the same length as `tmin`.
#' @param period Character. Aggregation period: `"annual"` (default) or
#'   `"monthly"`.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' dates <- as.Date("2024-07-01") + 0:9
#' tmin <- c(18, 21, 22, 19, 25, 20, 23, 17, 24, 21)
#' ck_tropical_nights(tmin, dates)
ck_tropical_nights <- function(tmin, dates, period = "annual") {
  validate_numeric(tmin, "tmin")
  validate_dates(dates, length(tmin))
  period <- validate_period(period)

  result <- count_by_period(tmin > 20, dates, period)
  build_result(result$periods, result$values, "tropical_nights", "days", period)
}

#' Growing Season Length
#'
#' Compute the growing season length following the ETCCDI definition: the
#' number of days between the first occurrence of at least 6 consecutive days
#' with daily mean temperature above 5 degrees C and the first span of 6
#' consecutive days with Tmean below 5 degrees C after July 1 (Northern
#' Hemisphere) or January 1 (Southern Hemisphere). Calculated per year.
#'
#' @param tavg Numeric vector of daily mean temperatures (degrees C).
#' @param dates Date vector of the same length as `tavg`.
#' @param lat Numeric. Latitude in decimal degrees (used to determine
#'   hemisphere for end-of-season rule). Default 50 (Northern Hemisphere).
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' dates <- as.Date("2024-01-01") + 0:364
#' set.seed(42)
#' tavg <- sin(seq(0, 2 * pi, length.out = 365)) * 15 + 5
#' ck_growing_season(tavg, dates)
ck_growing_season <- function(tavg, dates, lat = 50) {
  validate_numeric(tavg, "tavg")
  validate_dates(dates, length(tavg))
  if (!is.numeric(lat) || length(lat) != 1) {
    cli::cli_abort("{.arg lat} must be a single numeric value.")
  }

  years <- as.integer(format(dates, "%Y"))
  unique_years <- unique(years)

  # Day-of-year after which to look for end of season
  # NH: after July 1 (doy 182); SH: after Jan 1 (doy 1)
  mid_doy <- if (lat >= 0) 182L else 1L

  values <- vapply(unique_years, function(yr) {
    idx <- which(years == yr)
    temps <- tavg[idx]
    doy <- as.integer(format(dates[idx], "%j"))
    n <- length(temps)
    if (n < 6) return(NA_real_)

    above5 <- temps > 5

    # Find first 6-day spell above 5C
    first_start <- NA_integer_
    for (i in seq_len(n - 5)) {
      if (all(above5[i:(i + 5)])) {
        first_start <- i
        break
      }
    }
    if (is.na(first_start)) return(0)

    # Find end of season: first 6-day spell below 5C after mid-year
    season_end <- n  # default: end of data
    for (i in seq_len(n - 5)) {
      if (doy[i] >= mid_doy && all(!above5[i:(i + 5)])) {
        season_end <- i - 1L
        break
      }
    }

    as.double(max(season_end - first_start + 1, 0))
  }, numeric(1))

  build_result(as.character(unique_years), values, "growing_season", "days", "annual")
}

#' Heating Degree Days
#'
#' Sum of `(base - Tavg)` for all days where daily average temperature is
#' below the base temperature (default 18 degrees C).
#'
#' @param tavg Numeric vector of daily average temperatures (degrees C).
#' @param dates Date vector of the same length as `tavg`.
#' @param base Numeric. Base temperature in degrees C (default 18).
#' @param period Character. Aggregation period: `"annual"` (default) or
#'   `"monthly"`.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' dates <- as.Date("2024-01-01") + 0:9
#' tavg <- c(5, 10, 15, 20, 8, 12, 18, 3, 25, 7)
#' ck_heating_degree_days(tavg, dates)
ck_heating_degree_days <- function(tavg, dates, base = 18, period = "annual") {
  validate_numeric(tavg, "tavg")
  validate_dates(dates, length(tavg))
  period <- validate_period(period)

  hdd <- pmax(base - tavg, 0)
  result <- aggregate_by_period(hdd, dates, period, sum)
  build_result(result$periods, result$values, "heating_degree_days", "degree-days", period)
}

#' Cooling Degree Days
#'
#' Sum of `(Tavg - base)` for all days where daily average temperature is
#' above the base temperature (default 18 degrees C).
#'
#' @param tavg Numeric vector of daily average temperatures (degrees C).
#' @param dates Date vector of the same length as `tavg`.
#' @param base Numeric. Base temperature in degrees C (default 18).
#' @param period Character. Aggregation period: `"annual"` (default) or
#'   `"monthly"`.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' dates <- as.Date("2024-07-01") + 0:9
#' tavg <- c(25, 30, 22, 20, 28, 19, 32, 17, 35, 27)
#' ck_cooling_degree_days(tavg, dates)
ck_cooling_degree_days <- function(tavg, dates, base = 18, period = "annual") {
  validate_numeric(tavg, "tavg")
  validate_dates(dates, length(tavg))
  period <- validate_period(period)

  cdd <- pmax(tavg - base, 0)
  result <- aggregate_by_period(cdd, dates, period, sum)
  build_result(result$periods, result$values, "cooling_degree_days", "degree-days", period)
}

#' Growing Degree Days
#'
#' Sum of `(Tavg - base)` for all days where daily average temperature is
#' above the base temperature (default 10 degrees C).
#'
#' @param tavg Numeric vector of daily average temperatures (degrees C).
#' @param dates Date vector of the same length as `tavg`.
#' @param base Numeric. Base temperature in degrees C (default 10).
#' @param period Character. Aggregation period: `"annual"` (default) or
#'   `"monthly"`.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' dates <- as.Date("2024-07-01") + 0:9
#' tavg <- c(15, 20, 8, 12, 25, 9, 30, 11, 22, 18)
#' ck_growing_degree_days(tavg, dates)
ck_growing_degree_days <- function(tavg, dates, base = 10, period = "annual") {
  validate_numeric(tavg, "tavg")
  validate_dates(dates, length(tavg))
  period <- validate_period(period)

  gdd <- pmax(tavg - base, 0)
  result <- aggregate_by_period(gdd, dates, period, sum)
  build_result(result$periods, result$values, "growing_degree_days", "degree-days", period)
}

#' Diurnal Temperature Range
#'
#' Mean daily temperature range (Tmax - Tmin) per period.
#'
#' @param tmin Numeric vector of daily minimum temperatures (degrees C).
#' @param tmax Numeric vector of daily maximum temperatures (degrees C).
#' @param dates Date vector of the same length as `tmin` and `tmax`.
#' @param period Character. Aggregation period: `"annual"` (default) or
#'   `"monthly"`.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' dates <- as.Date("2024-01-01") + 0:9
#' tmin <- c(-2, 3, -1, 5, -3, 0, 2, -4, 1, -1)
#' tmax <- c(5, 10, 6, 12, 4, 8, 9, 3, 7, 6)
#' ck_diurnal_range(tmin, tmax, dates)
ck_diurnal_range <- function(tmin, tmax, dates, period = "annual") {
  validate_numeric(tmin, "tmin")
  validate_numeric(tmax, "tmax")
  validate_dates(dates, length(tmin))
  if (length(tmin) != length(tmax)) {
    cli::cli_abort("{.arg tmin} and {.arg tmax} must have the same length.")
  }

  dtr <- tmax - tmin
  result <- aggregate_by_period(dtr, dates, period, mean)
  build_result(result$periods, result$values, "diurnal_range", "\u00b0C", period)
}

#' Warm Spell Days
#'
#' Count the number of days in warm spells, where a warm spell is defined as
#' at least 6 consecutive days with Tmax above the `threshold` quantile of
#' the full series. This computes warm spell days using a quantile threshold
#' from the input series. It does not implement the ETCCDI WSDI, which
#' requires calendar-day percentiles from a 1961-1990 reference period.
#'
#' @param tmax Numeric vector of daily maximum temperatures (degrees C).
#' @param dates Date vector of the same length as `tmax`.
#' @param threshold Numeric. Quantile threshold (default 0.9, i.e. 90th
#'   percentile).
#' @param period Character. Aggregation period: `"annual"` (default) or
#'   `"monthly"`.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' dates <- as.Date("2024-01-01") + 0:364
#' set.seed(42)
#' tmax <- rnorm(365, mean = 20, sd = 5)
#' ck_warm_spell(tmax, dates)
ck_warm_spell <- function(tmax, dates, threshold = 0.9, period = "annual") {
  validate_numeric(tmax, "tmax")
  validate_dates(dates, length(tmax))
  period <- validate_period(period)

  thresh_val <- stats::quantile(tmax, threshold, na.rm = TRUE)
  above <- tmax > thresh_val

  # Mark days that are part of spells >= 6 days
  in_spell <- logical(length(tmax))
  n <- length(tmax)
  i <- 1
  while (i <= n) {
    if (!is.na(above[i]) && above[i]) {
      start <- i
      while (i <= n && !is.na(above[i]) && above[i]) {
        i <- i + 1
      }
      spell_len <- i - start
      if (spell_len >= 6) {
        in_spell[start:(i - 1)] <- TRUE
      }
    } else {
      i <- i + 1
    }
  }

  result <- count_by_period(in_spell, dates, period)
  build_result(result$periods, result$values, "warm_spell", "days", period)
}
