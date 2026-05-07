#' Maximum Consecutive Dry Days
#'
#' Maximum number of consecutive days with precipitation below a threshold.
#'
#' @param precip Numeric vector of daily precipitation (mm).
#' @param dates Date vector of the same length as `precip`.
#' @param threshold Numeric. Dry day threshold in mm (default 1).
#' @param period Character. Aggregation period: `"annual"` (default) or
#'   `"monthly"`.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' dates <- as.Date("2024-01-01") + 0:9
#' precip <- c(0, 0, 5, 0, 0, 0, 2, 0, 0, 0)
#' ck_dry_days(precip, dates)
ck_dry_days <- function(precip, dates, threshold = 1, period = "annual") {
  validate_numeric(precip, "precip")
  validate_dates(dates, length(precip))
  period <- validate_period(period)

  periods <- get_periods(dates, period)
  unique_periods <- unique(periods)

  values <- vapply(unique_periods, function(p) {
    idx <- which(periods == p)
    dry <- precip[idx] < threshold
    max_consecutive(dry)
  }, numeric(1))

  build_result(unique_periods, values, "dry_days", "days", period)
}

#' Maximum Consecutive Wet Days
#'
#' Maximum number of consecutive days with precipitation at or above a
#' threshold.
#'
#' @param precip Numeric vector of daily precipitation (mm).
#' @param dates Date vector of the same length as `precip`.
#' @param threshold Numeric. Wet day threshold in mm (default 1).
#' @param period Character. Aggregation period: `"annual"` (default) or
#'   `"monthly"`.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' dates <- as.Date("2024-01-01") + 0:9
#' precip <- c(5, 3, 0, 2, 8, 1, 0, 0, 4, 6)
#' ck_wet_days(precip, dates)
ck_wet_days <- function(precip, dates, threshold = 1, period = "annual") {
  validate_numeric(precip, "precip")
  validate_dates(dates, length(precip))
  period <- validate_period(period)

  periods <- get_periods(dates, period)
  unique_periods <- unique(periods)

  values <- vapply(unique_periods, function(p) {
    idx <- which(periods == p)
    wet <- precip[idx] >= threshold
    max_consecutive(wet)
  }, numeric(1))

  build_result(unique_periods, values, "wet_days", "days", period)
}

#' Total Wet-Day Precipitation (PRCPTOT)
#'
#' Annual or monthly total precipitation in wet days, where a wet day is
#' a day with precipitation at or above `wet_day_threshold` mm (default
#' 1 mm, the ETCCDI standard). This is the canonical ETCCDI 'PRCPTOT'
#' definition (Alexander et al. 2006; Zhang et al. 2011).
#'
#' Sub-threshold trace amounts are excluded. Pass
#' `wet_day_threshold = 0` to recover the previous behaviour of
#' summing all daily values.
#'
#' @param precip Numeric vector of daily precipitation (mm).
#' @param dates Date vector of the same length as `precip`.
#' @param period Character. Aggregation period: `"annual"` (default) or
#'   `"monthly"`.
#' @param wet_day_threshold Numeric (mm). Days with precipitation
#'   strictly below this threshold are excluded from the sum. Default 1.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' dates <- as.Date("2024-01-01") + 0:9
#' precip <- c(0, 5, 3, 0, 8, 2, 0, 1, 4, 0)
#' ck_total_precip(precip, dates)
ck_total_precip <- function(precip, dates, period = "annual",
                            wet_day_threshold = 1) {
  validate_numeric(precip, "precip")
  validate_dates(dates, length(precip))
  period <- validate_period(period)
  if (!is.numeric(wet_day_threshold) || length(wet_day_threshold) != 1L ||
      wet_day_threshold < 0) {
    cli::cli_abort("{.arg wet_day_threshold} must be a single non-negative numeric value.")
  }

  # Apply ETCCDI wet-day filter: sum only days with precip >= threshold.
  is_wet <- !is.na(precip) & precip >= wet_day_threshold
  contribution <- ifelse(is_wet, precip, 0)

  result <- aggregate_by_period(contribution, dates, period, sum)
  build_result(result$periods, result$values, "total_precip", "mm", period)
}

#' Heavy Precipitation Days
#'
#' Count of days with precipitation at or above a threshold (default 10 mm).
#'
#' @param precip Numeric vector of daily precipitation (mm).
#' @param dates Date vector of the same length as `precip`.
#' @param threshold Numeric. Threshold in mm (default 10).
#' @param period Character. Aggregation period: `"annual"` (default) or
#'   `"monthly"`.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' dates <- as.Date("2024-01-01") + 0:9
#' precip <- c(0, 5, 12, 0, 15, 2, 0, 11, 4, 0)
#' ck_heavy_precip(precip, dates)
ck_heavy_precip <- function(precip, dates, threshold = 10, period = "annual") {
  validate_numeric(precip, "precip")
  validate_dates(dates, length(precip))
  period <- validate_period(period)

  result <- count_by_period(precip >= threshold, dates, period)
  build_result(result$periods, result$values, "heavy_precip", "days", period)
}

#' Very Heavy Precipitation Days
#'
#' Count of days with precipitation at or above a threshold (default 20 mm).
#'
#' @param precip Numeric vector of daily precipitation (mm).
#' @param dates Date vector of the same length as `precip`.
#' @param threshold Numeric. Threshold in mm (default 20).
#' @param period Character. Aggregation period: `"annual"` (default) or
#'   `"monthly"`.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' dates <- as.Date("2024-01-01") + 0:9
#' precip <- c(0, 5, 22, 0, 15, 25, 0, 11, 4, 30)
#' ck_very_heavy_precip(precip, dates)
ck_very_heavy_precip <- function(precip, dates, threshold = 20, period = "annual") {
  validate_numeric(precip, "precip")
  validate_dates(dates, length(precip))
  period <- validate_period(period)

  result <- count_by_period(precip >= threshold, dates, period)
  build_result(result$periods, result$values, "very_heavy_precip", "days", period)
}

#' Maximum 1-Day Precipitation
#'
#' Maximum precipitation recorded in a single day per period.
#'
#' @param precip Numeric vector of daily precipitation (mm).
#' @param dates Date vector of the same length as `precip`.
#' @param period Character. Aggregation period: `"annual"` (default) or
#'   `"monthly"`.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' dates <- as.Date("2024-01-01") + 0:9
#' precip <- c(0, 5, 22, 0, 15, 25, 0, 11, 4, 30)
#' ck_max_1day_precip(precip, dates)
ck_max_1day_precip <- function(precip, dates, period = "annual") {
  validate_numeric(precip, "precip")
  validate_dates(dates, length(precip))
  period <- validate_period(period)

  result <- aggregate_by_period(precip, dates, period, max)
  build_result(result$periods, result$values, "max_1day_precip", "mm", period)
}

#' Maximum 5-Day Precipitation
#'
#' Maximum precipitation total over any 5 consecutive days per period.
#'
#' @param precip Numeric vector of daily precipitation (mm).
#' @param dates Date vector of the same length as `precip`.
#' @param period Character. Aggregation period: `"annual"` (default) or
#'   `"monthly"`.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' dates <- as.Date("2024-01-01") + 0:9
#' precip <- c(0, 5, 22, 0, 15, 25, 0, 11, 4, 30)
#' ck_max_5day_precip(precip, dates)
ck_max_5day_precip <- function(precip, dates, period = "annual") {
  validate_numeric(precip, "precip")
  validate_dates(dates, length(precip))
  period <- validate_period(period)

  periods <- get_periods(dates, period)
  unique_periods <- unique(periods)

  values <- vapply(unique_periods, function(p) {
    idx <- which(periods == p)
    x <- precip[idx]
    n <- length(x)
    if (n < 5) return(sum(x, na.rm = TRUE))
    rolling5 <- vapply(seq_len(n - 4), function(i) {
      sum(x[i:(i + 4)], na.rm = TRUE)
    }, numeric(1))
    max(rolling5, na.rm = TRUE)
  }, numeric(1))

  build_result(unique_periods, values, "max_5day_precip", "mm", period)
}

#' Precipitation Intensity (SDII)
#'
#' Mean precipitation on wet days (days with precipitation >= 1 mm).
#' Also known as the Simple Daily Intensity Index.
#'
#' @param precip Numeric vector of daily precipitation (mm).
#' @param dates Date vector of the same length as `precip`.
#' @param period Character. Aggregation period: `"annual"` (default) or
#'   `"monthly"`.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' dates <- as.Date("2024-01-01") + 0:9
#' precip <- c(0, 5, 12, 0, 15, 2, 0, 11, 4, 0)
#' ck_precip_intensity(precip, dates)
ck_precip_intensity <- function(precip, dates, period = "annual") {
  validate_numeric(precip, "precip")
  validate_dates(dates, length(precip))
  period <- validate_period(period)

  periods <- get_periods(dates, period)
  unique_periods <- unique(periods)

  values <- vapply(unique_periods, function(p) {
    pp <- precip[periods == p]
    pp <- pp[!is.na(pp)]
    if (length(pp) == 0L) return(NA_real_)
    wet <- pp[pp >= 1]
    if (length(wet) == 0L) return(0)
    mean(wet)
  }, numeric(1))

  build_result(unique_periods, values, "precip_intensity", "mm/day", period)
}

#' Very Wet Days Total (R95p)
#'
#' ETCCDI canonical index R95p. Annual total precipitation on days where
#' daily precipitation exceeds the 95th percentile of wet-day precipitation
#' in a reference period (default 1961 to 1990). A wet day is one with
#' precipitation at or above 1 mm. The threshold is a single value derived
#' from all wet days in the reference period (not calendar-day specific).
#'
#' @param precip Numeric vector of daily precipitation (mm).
#' @param dates Date vector of the same length as `precip`. Must contain
#'   data covering the reference period.
#' @param ref_start,ref_end Integer. Reference period boundary years
#'   (inclusive). Defaults to 1961 and 1990.
#' @param period Character. Aggregation period: `"annual"` (default) or
#'   `"monthly"`.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' set.seed(1)
#' dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
#' precip <- pmax(rgamma(length(dates), shape = 0.4, scale = 8) - 1, 0)
#' tail(ck_r95p(precip, dates))
ck_r95p <- function(precip, dates, ref_start = 1961L, ref_end = 1990L,
                    period = "annual") {
  .precip_pct_total(precip, dates, 0.95, ref_start, ref_end, period, "r95p")
}

#' Extremely Wet Days Total (R99p)
#'
#' ETCCDI canonical index R99p. Annual total precipitation on days where
#' daily precipitation exceeds the 99th percentile of wet-day precipitation
#' in a reference period (default 1961 to 1990). Same convention as
#' [ck_r95p()].
#'
#' @inheritParams ck_r95p
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' set.seed(1)
#' dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
#' precip <- pmax(rgamma(length(dates), shape = 0.4, scale = 8) - 1, 0)
#' tail(ck_r99p(precip, dates))
ck_r99p <- function(precip, dates, ref_start = 1961L, ref_end = 1990L,
                    period = "annual") {
  .precip_pct_total(precip, dates, 0.99, ref_start, ref_end, period, "r99p")
}

#' Implementation backbone for R95p / R99p
#' @noRd
.precip_pct_total <- function(precip, dates, percentile,
                              ref_start, ref_end, period, index_name) {
  validate_numeric(precip, "precip")
  validate_dates(dates, length(precip))
  period <- validate_period(period)

  years <- as.integer(format(dates, "%Y"))
  in_ref <- years >= ref_start & years <= ref_end
  if (!any(in_ref)) {
    cli::cli_abort(
      "No data in reference period {ref_start}-{ref_end}; supply data covering the period or pass {.arg ref_start} / {.arg ref_end}."
    )
  }

  ref_wet <- precip[in_ref]
  ref_wet <- ref_wet[!is.na(ref_wet) & ref_wet >= 1]
  if (length(ref_wet) == 0L) {
    cli::cli_abort(
      "No wet days (precip >= 1 mm) in reference period {ref_start}-{ref_end}."
    )
  }
  threshold <- stats::quantile(ref_wet, percentile, names = FALSE,
                               na.rm = TRUE, type = 8L)

  is_wet <- !is.na(precip) & precip >= 1
  exceeds <- is_wet & precip > threshold

  contribution <- ifelse(exceeds, precip, 0)
  result <- aggregate_by_period(contribution, dates, period, sum)
  build_result(result$periods, result$values, index_name, "mm", period)
}

#' Max consecutive TRUE values in a logical vector
#' @noRd
max_consecutive <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(0)
  x[is.na(x)] <- FALSE
  if (!any(x)) return(0)
  r <- rle(x)
  max(r$lengths[r$values])
}
