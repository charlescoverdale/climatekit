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

#' Annual or Monthly Maximum of Daily Maximum Temperature (TXx)
#'
#' ETCCDI canonical index TXx. The maximum value of daily maximum
#' temperature (Tmax) within each reporting period.
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
#' tmax <- c(5, 10, 18, 12, 4, 8, 22, 3, 7, 6)
#' ck_txx(tmax, dates)
ck_txx <- function(tmax, dates, period = "annual") {
  validate_numeric(tmax, "tmax")
  validate_dates(dates, length(tmax))
  period <- validate_period(period)

  result <- aggregate_by_period(tmax, dates, period, max)
  build_result(result$periods, result$values, "txx", "\u00b0C", period)
}

#' Annual or Monthly Maximum of Daily Minimum Temperature (TNx)
#'
#' ETCCDI canonical index TNx. The maximum value of daily minimum
#' temperature (Tmin) within each reporting period (warmest night).
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
#' tmin <- c(15, 18, 22, 19, 14, 21, 23, 17, 20, 19)
#' ck_tnx(tmin, dates)
ck_tnx <- function(tmin, dates, period = "annual") {
  validate_numeric(tmin, "tmin")
  validate_dates(dates, length(tmin))
  period <- validate_period(period)

  result <- aggregate_by_period(tmin, dates, period, max)
  build_result(result$periods, result$values, "tnx", "\u00b0C", period)
}

#' Annual or Monthly Minimum of Daily Maximum Temperature (TXn)
#'
#' ETCCDI canonical index TXn. The minimum value of daily maximum
#' temperature (Tmax) within each reporting period (coldest day).
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
#' tmax <- c(5, 10, -3, 12, 4, 8, 22, -8, 7, 6)
#' ck_txn(tmax, dates)
ck_txn <- function(tmax, dates, period = "annual") {
  validate_numeric(tmax, "tmax")
  validate_dates(dates, length(tmax))
  period <- validate_period(period)

  result <- aggregate_by_period(tmax, dates, period, min)
  build_result(result$periods, result$values, "txn", "\u00b0C", period)
}

#' Annual or Monthly Minimum of Daily Minimum Temperature (TNn)
#'
#' ETCCDI canonical index TNn. The minimum value of daily minimum
#' temperature (Tmin) within each reporting period (coldest night).
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
#' tmin <- c(-2, 3, -1, 5, -8, 0, 2, -12, 1, -1)
#' ck_tnn(tmin, dates)
ck_tnn <- function(tmin, dates, period = "annual") {
  validate_numeric(tmin, "tmin")
  validate_dates(dates, length(tmin))
  period <- validate_period(period)

  result <- aggregate_by_period(tmin, dates, period, min)
  build_result(result$periods, result$values, "tnn", "\u00b0C", period)
}

#' Percentage of Cool Days (TX10p)
#'
#' ETCCDI canonical index TX10p. Percentage of days where daily Tmax
#' falls below the 10th percentile of the calendar-day distribution
#' from a reference period (default 1961 to 1990). The threshold is
#' computed using a 5-day window centred on each calendar day, pooled
#' across the reference period.
#'
#' Set `bootstrap = TRUE` to apply the Zhang et al. (2005) in-base
#' leave-one-out bootstrap, which removes the self-inclusion bias for
#' years inside the reference period. The bootstrap is computationally
#' expensive (roughly N^2 percentile fits for an N-year reference)
#' but is the canonical climdex.pcic / climpact behaviour and is
#' required for climate-change attribution work that spans the base.
#'
#' @param tmax Numeric vector of daily maximum temperatures (degrees C).
#' @param dates Date vector of the same length as `tmax`. Must contain
#'   data covering the reference period.
#' @param ref_start,ref_end Integer. Reference period boundary years
#'   (inclusive). Defaults to 1961 and 1990.
#' @param period Character. Aggregation period: `"annual"` (default) or
#'   `"monthly"`.
#' @param bootstrap Logical. If `TRUE`, apply the Zhang (2005) in-base
#'   bootstrap correction. Default `FALSE` for backward compatibility
#'   and speed.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @references Zhang, X., Hegerl, G. C., Zwiers, F. W., & Kenyon, J.
#'   (2005). Avoiding inhomogeneity in percentile-based indices of
#'   temperature extremes. *Journal of Climate*, 18(11), 1641-1651.
#'   \doi{10.1175/JCLI3366.1}.
#'
#' @export
#' @examples
#' set.seed(1)
#' dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
#' tmax <- 15 + 10 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
#'         rnorm(length(dates))
#' tail(ck_tx10p(tmax, dates))
ck_tx10p <- function(tmax, dates, ref_start = 1961L, ref_end = 1990L,
                     period = "annual", bootstrap = FALSE) {
  validate_numeric(tmax, "tmax")
  validate_dates(dates, length(tmax))
  period <- validate_period(period)
  if (!is.logical(bootstrap) || length(bootstrap) != 1L) {
    cli::cli_abort("{.arg bootstrap} must be a single logical value.")
  }

  soft_ex <- .pct_exceedance_per_day(tmax, dates, percentile = 0.10,
                                     op = "<", ref_start, ref_end,
                                     bootstrap = bootstrap)
  agg <- .aggregate_pct_exceedance(soft_ex, dates, period)
  build_result(agg$periods, agg$values, "tx10p", "%", period)
}

#' Percentage of Cool Nights (TN10p)
#'
#' ETCCDI canonical index TN10p. Percentage of days where daily Tmin
#' falls below the 10th percentile of the calendar-day distribution
#' from a reference period (default 1961 to 1990). Computation follows
#' the same convention as [ck_tx10p()] and supports the same
#' `bootstrap` argument.
#'
#' @inheritParams ck_tx10p
#' @param tmin Numeric vector of daily minimum temperatures (degrees C).
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' set.seed(1)
#' dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
#' tmin <- 5 + 8 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
#'         rnorm(length(dates))
#' tail(ck_tn10p(tmin, dates))
ck_tn10p <- function(tmin, dates, ref_start = 1961L, ref_end = 1990L,
                     period = "annual", bootstrap = FALSE) {
  validate_numeric(tmin, "tmin")
  validate_dates(dates, length(tmin))
  period <- validate_period(period)
  if (!is.logical(bootstrap) || length(bootstrap) != 1L) {
    cli::cli_abort("{.arg bootstrap} must be a single logical value.")
  }

  soft_ex <- .pct_exceedance_per_day(tmin, dates, percentile = 0.10,
                                     op = "<", ref_start, ref_end,
                                     bootstrap = bootstrap)
  agg <- .aggregate_pct_exceedance(soft_ex, dates, period)
  build_result(agg$periods, agg$values, "tn10p", "%", period)
}

#' Percentage of Warm Days (TX90p)
#'
#' ETCCDI canonical index TX90p. Percentage of days where daily Tmax
#' exceeds the 90th percentile of the calendar-day distribution from a
#' reference period (default 1961 to 1990). Computation follows the
#' same convention as [ck_tx10p()] and supports the same `bootstrap`
#' argument.
#'
#' @inheritParams ck_tx10p
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' set.seed(1)
#' dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
#' tmax <- 15 + 10 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
#'         rnorm(length(dates))
#' tail(ck_tx90p(tmax, dates))
ck_tx90p <- function(tmax, dates, ref_start = 1961L, ref_end = 1990L,
                     period = "annual", bootstrap = FALSE) {
  validate_numeric(tmax, "tmax")
  validate_dates(dates, length(tmax))
  period <- validate_period(period)
  if (!is.logical(bootstrap) || length(bootstrap) != 1L) {
    cli::cli_abort("{.arg bootstrap} must be a single logical value.")
  }

  soft_ex <- .pct_exceedance_per_day(tmax, dates, percentile = 0.90,
                                     op = ">", ref_start, ref_end,
                                     bootstrap = bootstrap)
  agg <- .aggregate_pct_exceedance(soft_ex, dates, period)
  build_result(agg$periods, agg$values, "tx90p", "%", period)
}

#' Percentage of Warm Nights (TN90p)
#'
#' ETCCDI canonical index TN90p. Percentage of days where daily Tmin
#' exceeds the 90th percentile of the calendar-day distribution from a
#' reference period (default 1961 to 1990). Computation follows the
#' same convention as [ck_tx10p()] and supports the same `bootstrap`
#' argument.
#'
#' @inheritParams ck_tn10p
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' set.seed(1)
#' dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
#' tmin <- 5 + 8 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
#'         rnorm(length(dates))
#' tail(ck_tn90p(tmin, dates))
ck_tn90p <- function(tmin, dates, ref_start = 1961L, ref_end = 1990L,
                     period = "annual", bootstrap = FALSE) {
  validate_numeric(tmin, "tmin")
  validate_dates(dates, length(tmin))
  period <- validate_period(period)
  if (!is.logical(bootstrap) || length(bootstrap) != 1L) {
    cli::cli_abort("{.arg bootstrap} must be a single logical value.")
  }

  soft_ex <- .pct_exceedance_per_day(tmin, dates, percentile = 0.90,
                                     op = ">", ref_start, ref_end,
                                     bootstrap = bootstrap)
  agg <- .aggregate_pct_exceedance(soft_ex, dates, period)
  build_result(agg$periods, agg$values, "tn90p", "%", period)
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

#' Warm Spell Duration Index (WSDI)
#'
#' ETCCDI canonical index WSDI. Annual count of days in spans of at least
#' six consecutive days where daily Tmax exceeds the 90th percentile of the
#' calendar-day distribution from a reference period (default 1961 to 1990).
#' The threshold is computed using a 5-day window centred on each calendar
#' day, pooled across the reference period.
#'
#' Days inside qualifying spells are counted into the year they fall in;
#' a spell that crosses a year boundary contributes to both years
#' proportionally. Annual aggregation only.
#'
#' @inheritParams ck_tx10p
#' @param min_spell Integer. Minimum spell length in days (default 6,
#'   the ETCCDI standard).
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' set.seed(1)
#' dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
#' tmax <- 15 + 10 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
#'         rnorm(length(dates))
#' tail(ck_wsdi(tmax, dates))
ck_wsdi <- function(tmax, dates, ref_start = 1961L, ref_end = 1990L,
                    min_spell = 6L) {
  validate_numeric(tmax, "tmax")
  validate_dates(dates, length(tmax))

  thresholds <- .calendar_day_percentile(tmax, dates, 0.90,
                                         ref_start, ref_end, window = 5L)
  doy <- as.integer(format(dates, "%j"))
  above <- !is.na(tmax) & tmax > thresholds[doy]
  in_spell <- .find_spells(above, as.integer(min_spell))

  result <- count_by_period(in_spell, dates, "annual")
  build_result(result$periods, result$values, "wsdi", "days", "annual")
}

#' Cold Spell Duration Index (CSDI)
#'
#' ETCCDI canonical index CSDI. Annual count of days in spans of at least
#' six consecutive days where daily Tmin falls below the 10th percentile of
#' the calendar-day distribution from a reference period (default 1961 to
#' 1990). Cold-side counterpart to [ck_wsdi()].
#'
#' @inheritParams ck_tn10p
#' @param min_spell Integer. Minimum spell length in days (default 6,
#'   the ETCCDI standard).
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' set.seed(1)
#' dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
#' tmin <- 5 + 8 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
#'         rnorm(length(dates))
#' tail(ck_csdi(tmin, dates))
ck_csdi <- function(tmin, dates, ref_start = 1961L, ref_end = 1990L,
                    min_spell = 6L) {
  validate_numeric(tmin, "tmin")
  validate_dates(dates, length(tmin))

  thresholds <- .calendar_day_percentile(tmin, dates, 0.10,
                                         ref_start, ref_end, window = 5L)
  doy <- as.integer(format(dates, "%j"))
  below <- !is.na(tmin) & tmin < thresholds[doy]
  in_spell <- .find_spells(below, as.integer(min_spell))

  result <- count_by_period(in_spell, dates, "annual")
  build_result(result$periods, result$values, "csdi", "days", "annual")
}

#' Warm Spell Days (Series-Quantile Approximation)
#'
#' Count the number of days in warm spells, where a warm spell is defined
#' as at least six consecutive days with Tmax above the `threshold`
#' quantile of the full input series. This is a quick approximation
#' driven by a single series-wide quantile, and does not require a
#' reference period.
#'
#' For the canonical ETCCDI WSDI definition (1961-1990 calendar-day base,
#' six-day spell rule), use [ck_wsdi()].
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
