# Input validation and period aggregation helpers

#' Validate numeric vector input
#' @noRd
validate_numeric <- function(x, name) {

if (!is.numeric(x)) {
    cli::cli_abort("{.arg {name}} must be a numeric vector.")
  }
  invisible(x)
}

#' Validate dates vector
#' @noRd
validate_dates <- function(dates, n) {
  if (!inherits(dates, "Date")) {
    cli::cli_abort("{.arg dates} must be a {.cls Date} vector.")
  }
  if (length(dates) != n) {
    cli::cli_abort(
      "{.arg dates} must have the same length as the data vector ({n}), not {length(dates)}."
    )
  }
  invisible(dates)
}

#' Validate and parse period argument
#' @noRd
validate_period <- function(period) {
  period <- match.arg(period, c("annual", "monthly"))
  period
}

#' Extract period labels from dates
#' @noRd
get_periods <- function(dates, period) {
  if (period == "annual") {
    as.integer(format(dates, "%Y"))
  } else {
    format(dates, "%Y-%m")
  }
}

#' Convert period labels to Date objects
#' @noRd
period_to_date <- function(period_labels, period) {
  if (period == "annual") {
    as.Date(paste0(period_labels, "-01-01"))
  } else {
    as.Date(paste0(period_labels, "-01"))
  }
}

#' Build output data.frame
#' @noRd
build_result <- function(period_labels, values, index_name, unit, period) {
  data.frame(
    period = period_to_date(period_labels, period),
    value = values,
    index = index_name,
    unit = unit,
    stringsAsFactors = FALSE
  )
}

#' Aggregate by period applying a function
#' @noRd
aggregate_by_period <- function(x, dates, period, fun, na.rm = TRUE) {
  periods <- get_periods(dates, period)
  unique_periods <- unique(periods)
  values <- vapply(unique_periods, function(p) {
    fun(x[periods == p], na.rm = na.rm)
  }, numeric(1))
  list(periods = unique_periods, values = values)
}

#' Count by period where condition is TRUE
#' @noRd
count_by_period <- function(condition, dates, period) {
  periods <- get_periods(dates, period)
  unique_periods <- unique(periods)
  values <- vapply(unique_periods, function(p) {
    sum(condition[periods == p], na.rm = TRUE)
  }, numeric(1))
  list(periods = unique_periods, values = values)
}

#' Calendar-day percentile thresholds for a daily series
#'
#' Returns a length-366 vector of thresholds, one per day of year, computed
#' from the supplied reference window pooled across the reference period.
#' Used by the ETCCDI percentile indices (TX10p, TN10p, TX90p, TN90p, CSDI,
#' WSDI). Follows the ETCCDI +/-2-day window convention (a 5-day window
#' centred on each calendar day, wrapped at year boundaries). Does not
#' implement the Zhang et al. (2005) in-base bootstrap; thresholds are
#' applied directly, so values inside the reference period have a small
#' self-inclusion bias.
#'
#' @noRd
.calendar_day_percentile <- function(values, dates, percentile,
                                     ref_start = 1961L, ref_end = 1990L,
                                     window = 5L) {
  if (!is.numeric(percentile) || length(percentile) != 1L ||
      percentile <= 0 || percentile >= 1) {
    cli::cli_abort("{.arg percentile} must be a single number in (0, 1).")
  }
  if (window < 1L || window %% 2L != 1L) {
    cli::cli_abort("{.arg window} must be a positive odd integer.")
  }

  years <- as.integer(format(dates, "%Y"))
  doy <- as.integer(format(dates, "%j"))

  in_ref <- years >= ref_start & years <= ref_end
  if (!any(in_ref)) {
    cli::cli_abort(
      "No data in reference period {ref_start}-{ref_end}; supply data covering the period or pass {.arg ref_start} / {.arg ref_end}."
    )
  }

  ref_values <- values[in_ref]
  ref_doy <- doy[in_ref]

  half <- (window - 1L) %/% 2L
  thresholds <- rep(NA_real_, 366L)

  for (d in seq_len(366L)) {
    target_doys <- ((d - half - 1L):(d + half - 1L)) %% 366L + 1L
    pool <- ref_values[ref_doy %in% target_doys]
    if (length(pool) > 0L && !all(is.na(pool))) {
      thresholds[d] <- stats::quantile(pool, percentile, na.rm = TRUE,
                                       names = FALSE, type = 8L)
    }
  }
  thresholds
}

#' Mark days that fall inside spans of at least `min_spell` consecutive TRUE
#' values in a logical vector. Used by spell-duration indices (CSDI, WSDI).
#' @noRd
.find_spells <- function(condition, min_spell = 6L) {
  n <- length(condition)
  in_spell <- logical(n)
  i <- 1L
  while (i <= n) {
    if (!is.na(condition[i]) && condition[i]) {
      start <- i
      while (i <= n && !is.na(condition[i]) && condition[i]) {
        i <- i + 1L
      }
      if (i - start >= min_spell) {
        in_spell[start:(i - 1L)] <- TRUE
      }
    } else {
      i <- i + 1L
    }
  }
  in_spell
}
