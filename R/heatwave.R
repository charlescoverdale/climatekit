# ET-SCI heatwave and cold-wave family indices.
#
# Definitions follow the Expert Team on Sector-specific Climate Indices.
# A heat wave is a period of at least three consecutive days with daily
# maximum temperature exceeding the 90th percentile of the calendar-day
# distribution from a reference period (default 1961-1990). The cold-wave
# dual uses daily minimum temperature below the 10th percentile of the
# calendar-day base. Each heatwave / cold-wave family produces five
# annual statistics:
#
# - HWN / CWN: number of events
# - HWF / CWF: frequency (total days in events)
# - HWD / CWD: duration (longest event in days)
# - HWM / CWM: magnitude (mean excess of TX over the threshold, or
#   threshold minus TN, on event days)
# - HWA / CWA: amplitude (peak excess on event days)
#
# Note: `ck_cwd` here is ET-SCI Cold Wave Duration; the ETCCDI CWD
# (Consecutive Wet Days) is a different index and lives in
# precipitation.R as `ck_wet_days`.

#' Per-year heatwave / cold-wave family statistics
#'
#' Returns one row per analysis year with columns:
#'   year, n (count), f (total days in events), d (longest event),
#'   m_excess, a_excess, m_value, a_extreme.
#'
#' `m_excess` and `a_excess` are mean and peak of (value - threshold)
#' for `op = ">"` heatwave or (threshold - value) for `op = "<"` cold
#' wave (always non-negative). `m_value` is the mean of raw values on
#' event days. `a_extreme` is the max raw value on event days for `op
#' = ">"` and the min raw value for `op = "<"`. NA for years with no
#' qualifying events.
#' @noRd
.spell_family_stats <- function(values, dates, ref_start, ref_end,
                                percentile, op, min_spell) {
  thresholds <- .calendar_day_percentile(values, dates, percentile,
                                         ref_start, ref_end, window = 5L)
  doy <- as.integer(format(dates, "%j"))
  threshold_per_day <- thresholds[doy]

  if (identical(op, ">")) {
    matches <- !is.na(values) & values > threshold_per_day
    excess <- values - threshold_per_day
  } else if (identical(op, "<")) {
    matches <- !is.na(values) & values < threshold_per_day
    excess <- threshold_per_day - values
  } else {
    cli::cli_abort("Internal: unsupported {.arg op}.")
  }

  in_spell <- .find_spells(matches, as.integer(min_spell))

  years <- as.integer(format(dates, "%Y"))
  unique_years <- unique(years)

  rows <- lapply(unique_years, function(yr) {
    sel <- years == yr
    in_y <- in_spell[sel]
    ex_y <- excess[sel]
    val_y <- values[sel]

    if (!any(in_y, na.rm = TRUE)) {
      return(c(yr, 0L, 0L, 0L,
               NA_real_, NA_real_, NA_real_, NA_real_))
    }

    r <- rle(in_y)
    seg_lens <- r$lengths[!is.na(r$values) & r$values]
    n_events <- length(seg_lens)
    f_total  <- sum(seg_lens)
    d_max    <- max(seg_lens)

    keep <- in_y & !is.na(in_y) & !is.na(ex_y) & !is.na(val_y)
    ex_in  <- ex_y[keep]
    val_in <- val_y[keep]

    if (length(ex_in) > 0L) {
      m_excess  <- mean(ex_in)
      a_excess  <- max(ex_in)
      m_value   <- mean(val_in)
      a_extreme <- if (identical(op, ">")) max(val_in) else min(val_in)
    } else {
      m_excess  <- NA_real_
      a_excess  <- NA_real_
      m_value   <- NA_real_
      a_extreme <- NA_real_
    }

    c(yr, n_events, f_total, d_max,
      m_excess, a_excess, m_value, a_extreme)
  })

  m <- do.call(rbind, rows)
  data.frame(
    year      = as.integer(m[, 1]),
    n         = as.integer(m[, 2]),
    f         = as.integer(m[, 3]),
    d         = as.integer(m[, 4]),
    m_excess  = m[, 5],
    a_excess  = m[, 6],
    m_value   = m[, 7],
    a_extreme = m[, 8],
    stringsAsFactors = FALSE
  )
}

#' Validate the spell-family `mode` argument
#' @noRd
.validate_spell_mode <- function(mode) {
  match.arg(mode, c("excess", "absolute"))
}

# Heatwave family (TX > 90th percentile, calendar-day base) -----------------

#' Heatwave Number (HWN)
#'
#' ET-SCI heatwave family index. Annual count of distinct heatwave events,
#' where a heatwave is a span of at least three consecutive days with
#' daily Tmax above the 90th percentile of the calendar-day distribution
#' from a reference period (default 1961 to 1990).
#'
#' Single-threshold definition (TX-only). For the dual-threshold
#' Perkins-Alexander variant (TX and TN both above 90th percentile) see
#' `climpact`.
#'
#' @inheritParams ck_tx10p
#' @param min_spell Integer. Minimum spell length in days (default 3,
#'   the ET-SCI standard).
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' set.seed(1)
#' dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
#' tmax <- 15 + 10 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
#'         rnorm(length(dates))
#' tail(ck_hwn(tmax, dates))
ck_hwn <- function(tmax, dates, ref_start = 1961L, ref_end = 1990L,
                   min_spell = 3L) {
  validate_temperature(tmax, "tmax")
  validate_dates(dates, length(tmax))
  stats <- .spell_family_stats(tmax, dates, ref_start, ref_end,
                               percentile = 0.90, op = ">",
                               min_spell = min_spell)
  build_result(as.character(stats$year), as.numeric(stats$n),
               "hwn", "events", "annual")
}

#' Heatwave Frequency (HWF)
#'
#' ET-SCI heatwave family index. Annual total number of days inside any
#' heatwave event (see [ck_hwn()] for the heatwave definition).
#'
#' @inheritParams ck_hwn
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' set.seed(1)
#' dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
#' tmax <- 15 + 10 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
#'         rnorm(length(dates))
#' tail(ck_hwf(tmax, dates))
ck_hwf <- function(tmax, dates, ref_start = 1961L, ref_end = 1990L,
                   min_spell = 3L) {
  validate_temperature(tmax, "tmax")
  validate_dates(dates, length(tmax))
  stats <- .spell_family_stats(tmax, dates, ref_start, ref_end,
                               percentile = 0.90, op = ">",
                               min_spell = min_spell)
  build_result(as.character(stats$year), as.numeric(stats$f),
               "hwf", "days", "annual")
}

#' Heatwave Duration (HWD)
#'
#' ET-SCI heatwave family index. Length in days of the longest heatwave
#' event in each year (see [ck_hwn()] for the heatwave definition).
#'
#' @inheritParams ck_hwn
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' set.seed(1)
#' dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
#' tmax <- 15 + 10 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
#'         rnorm(length(dates))
#' tail(ck_hwd(tmax, dates))
ck_hwd <- function(tmax, dates, ref_start = 1961L, ref_end = 1990L,
                   min_spell = 3L) {
  validate_temperature(tmax, "tmax")
  validate_dates(dates, length(tmax))
  stats <- .spell_family_stats(tmax, dates, ref_start, ref_end,
                               percentile = 0.90, op = ">",
                               min_spell = min_spell)
  build_result(as.character(stats$year), as.numeric(stats$d),
               "hwd", "days", "annual")
}

#' Heatwave Magnitude (HWM)
#'
#' ET-SCI heatwave family index. Reports the mean magnitude of daily
#' Tmax across all heatwave days in the year. `mode = "excess"`
#' (default) gives the mean of (Tmax - threshold), matching the
#' ET-SCI / climpact convention. `mode = "absolute"` gives the mean
#' raw Tmax across heatwave days, matching Perkins-Alexander (2013).
#' Returns `NA` for years with no heatwaves.
#'
#' @inheritParams ck_hwn
#' @param mode One of `"excess"` (default) or `"absolute"`. See details.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' set.seed(1)
#' dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
#' tmax <- 15 + 10 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
#'         rnorm(length(dates))
#' tail(ck_hwm(tmax, dates))
ck_hwm <- function(tmax, dates, ref_start = 1961L, ref_end = 1990L,
                   min_spell = 3L, mode = c("excess", "absolute")) {
  validate_temperature(tmax, "tmax")
  validate_dates(dates, length(tmax))
  mode <- .validate_spell_mode(mode)
  stats <- .spell_family_stats(tmax, dates, ref_start, ref_end,
                               percentile = 0.90, op = ">",
                               min_spell = min_spell)
  vals <- if (mode == "excess") stats$m_excess else stats$m_value
  build_result(as.character(stats$year), vals,
               "hwm", "\u00b0C", "annual")
}

#' Heatwave Amplitude (HWA)
#'
#' ET-SCI heatwave family index. Reports the peak magnitude of daily
#' Tmax across all heatwave days in the year. `mode = "excess"`
#' (default) gives the maximum of (Tmax - threshold). `mode =
#' "absolute"` gives the maximum raw Tmax across heatwave days
#' (matching Perkins-Alexander 2013). Returns `NA` for years with
#' no heatwaves.
#'
#' @inheritParams ck_hwn
#' @param mode One of `"excess"` (default) or `"absolute"`. See details.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' set.seed(1)
#' dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
#' tmax <- 15 + 10 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
#'         rnorm(length(dates))
#' tail(ck_hwa(tmax, dates))
ck_hwa <- function(tmax, dates, ref_start = 1961L, ref_end = 1990L,
                   min_spell = 3L, mode = c("excess", "absolute")) {
  validate_temperature(tmax, "tmax")
  validate_dates(dates, length(tmax))
  mode <- .validate_spell_mode(mode)
  stats <- .spell_family_stats(tmax, dates, ref_start, ref_end,
                               percentile = 0.90, op = ">",
                               min_spell = min_spell)
  vals <- if (mode == "excess") stats$a_excess else stats$a_extreme
  build_result(as.character(stats$year), vals,
               "hwa", "\u00b0C", "annual")
}

# Cold-wave family (TN < 10th percentile, calendar-day base) ----------------

#' Cold-Wave Number (CWN)
#'
#' ET-SCI cold-wave family index. Annual count of distinct cold-wave
#' events, where a cold wave is a span of at least three consecutive
#' days with daily Tmin below the 10th percentile of the calendar-day
#' distribution from a reference period (default 1961 to 1990).
#'
#' @inheritParams ck_tn10p
#' @param min_spell Integer. Minimum spell length in days (default 3,
#'   the ET-SCI standard).
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' set.seed(1)
#' dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
#' tmin <- 5 + 8 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
#'         rnorm(length(dates))
#' tail(ck_cwn(tmin, dates))
ck_cwn <- function(tmin, dates, ref_start = 1961L, ref_end = 1990L,
                   min_spell = 3L) {
  validate_temperature(tmin, "tmin")
  validate_dates(dates, length(tmin))
  stats <- .spell_family_stats(tmin, dates, ref_start, ref_end,
                               percentile = 0.10, op = "<",
                               min_spell = min_spell)
  build_result(as.character(stats$year), as.numeric(stats$n),
               "cwn", "events", "annual")
}

#' Cold-Wave Frequency (CWF)
#'
#' ET-SCI cold-wave family index. Annual total number of days inside any
#' cold-wave event (see [ck_cwn()] for the cold-wave definition).
#'
#' @inheritParams ck_cwn
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' set.seed(1)
#' dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
#' tmin <- 5 + 8 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
#'         rnorm(length(dates))
#' tail(ck_cwf(tmin, dates))
ck_cwf <- function(tmin, dates, ref_start = 1961L, ref_end = 1990L,
                   min_spell = 3L) {
  validate_temperature(tmin, "tmin")
  validate_dates(dates, length(tmin))
  stats <- .spell_family_stats(tmin, dates, ref_start, ref_end,
                               percentile = 0.10, op = "<",
                               min_spell = min_spell)
  build_result(as.character(stats$year), as.numeric(stats$f),
               "cwf", "days", "annual")
}

#' Cold-Wave Duration (CWD, ET-SCI)
#'
#' ET-SCI cold-wave family index. Length in days of the longest
#' cold-wave event in each year (see [ck_cwn()] for the cold-wave
#' definition).
#'
#' Note: the same letters CWD also denote the ETCCDI **Consecutive Wet
#' Days** precipitation index, which is unrelated and is implemented in
#' [ck_wet_days()]. These are two different indices that share an
#' acronym in the climate-extremes literature.
#'
#' @inheritParams ck_cwn
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' set.seed(1)
#' dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
#' tmin <- 5 + 8 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
#'         rnorm(length(dates))
#' tail(ck_cwd(tmin, dates))
ck_cwd <- function(tmin, dates, ref_start = 1961L, ref_end = 1990L,
                   min_spell = 3L) {
  validate_temperature(tmin, "tmin")
  validate_dates(dates, length(tmin))
  stats <- .spell_family_stats(tmin, dates, ref_start, ref_end,
                               percentile = 0.10, op = "<",
                               min_spell = min_spell)
  build_result(as.character(stats$year), as.numeric(stats$d),
               "cwd", "days", "annual")
}

#' Cold-Wave Magnitude (CWM)
#'
#' ET-SCI cold-wave family index. `mode = "excess"` (default) returns
#' the mean of (threshold - Tmin) across cold-wave days, expressed as
#' a positive magnitude. `mode = "absolute"` returns the mean raw
#' Tmin across cold-wave days. Returns `NA` for years with no cold
#' waves.
#'
#' @inheritParams ck_cwn
#' @param mode One of `"excess"` (default) or `"absolute"`. See details.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' set.seed(1)
#' dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
#' tmin <- 5 + 8 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
#'         rnorm(length(dates))
#' tail(ck_cwm(tmin, dates))
ck_cwm <- function(tmin, dates, ref_start = 1961L, ref_end = 1990L,
                   min_spell = 3L, mode = c("excess", "absolute")) {
  validate_temperature(tmin, "tmin")
  validate_dates(dates, length(tmin))
  mode <- .validate_spell_mode(mode)
  stats <- .spell_family_stats(tmin, dates, ref_start, ref_end,
                               percentile = 0.10, op = "<",
                               min_spell = min_spell)
  vals <- if (mode == "excess") stats$m_excess else stats$m_value
  build_result(as.character(stats$year), vals,
               "cwm", "\u00b0C", "annual")
}

#' Cold-Wave Amplitude (CWA)
#'
#' ET-SCI cold-wave family index. `mode = "excess"` (default) returns
#' the peak (threshold - Tmin) across cold-wave days, expressed as a
#' positive magnitude. `mode = "absolute"` returns the minimum raw
#' Tmin across cold-wave days (the coldest event-day value). Returns
#' `NA` for years with no cold waves.
#'
#' @inheritParams ck_cwn
#' @param mode One of `"excess"` (default) or `"absolute"`. See details.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @export
#' @examples
#' set.seed(1)
#' dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
#' tmin <- 5 + 8 * sin(2 * pi * as.integer(format(dates, "%j")) / 365) +
#'         rnorm(length(dates))
#' tail(ck_cwa(tmin, dates))
ck_cwa <- function(tmin, dates, ref_start = 1961L, ref_end = 1990L,
                   min_spell = 3L, mode = c("excess", "absolute")) {
  validate_temperature(tmin, "tmin")
  validate_dates(dates, length(tmin))
  mode <- .validate_spell_mode(mode)
  stats <- .spell_family_stats(tmin, dates, ref_start, ref_end,
                               percentile = 0.10, op = "<",
                               min_spell = min_spell)
  vals <- if (mode == "excess") stats$a_excess else stats$a_extreme
  build_result(as.character(stats$year), vals,
               "cwa", "\u00b0C", "annual")
}
