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
