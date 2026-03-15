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

#' Total Precipitation
#'
#' Total precipitation sum by period.
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
#' precip <- c(0, 5, 3, 0, 8, 2, 0, 1, 4, 0)
#' ck_total_precip(precip, dates)
ck_total_precip <- function(precip, dates, period = "annual") {
  validate_numeric(precip, "precip")
  validate_dates(dates, length(precip))
  period <- validate_period(period)

  result <- aggregate_by_period(precip, dates, period, sum)
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
    idx <- which(periods == p)
    wet <- precip[idx][precip[idx] >= 1]
    if (length(wet) == 0) return(0)
    mean(wet, na.rm = TRUE)
  }, numeric(1))

  build_result(unique_periods, values, "precip_intensity", "mm/day", period)
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
