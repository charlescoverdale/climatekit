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

    if (!any(in_y, na.rm = TRUE)) {
      return(c(yr, 0L, 0L, 0L, NA_real_, NA_real_))
    }

    r <- rle(in_y)
    seg_lens <- r$lengths[!is.na(r$values) & r$values]
    n_events <- length(seg_lens)
    f_total  <- sum(seg_lens)
    d_max    <- max(seg_lens)

    ex_in <- ex_y[in_y & !is.na(in_y) & !is.na(ex_y)]
    if (length(ex_in) > 0L) {
      m_mean <- mean(ex_in)
      a_max  <- max(ex_in)
    } else {
      m_mean <- NA_real_
      a_max  <- NA_real_
    }

    c(yr, n_events, f_total, d_max, m_mean, a_max)
  })

  m <- do.call(rbind, rows)
  data.frame(
    year   = as.integer(m[, 1]),
    n      = as.integer(m[, 2]),
    f      = as.integer(m[, 3]),
    d      = as.integer(m[, 4]),
    m_mean = m[, 5],
    a_max  = m[, 6],
    stringsAsFactors = FALSE
  )
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
  validate_numeric(tmax, "tmax")
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
  validate_numeric(tmax, "tmax")
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
  validate_numeric(tmax, "tmax")
  validate_dates(dates, length(tmax))
  stats <- .spell_family_stats(tmax, dates, ref_start, ref_end,
                               percentile = 0.90, op = ">",
                               min_spell = min_spell)
  build_result(as.character(stats$year), as.numeric(stats$d),
               "hwd", "days", "annual")
}

#' Heatwave Magnitude (HWM)
#'
#' ET-SCI heatwave family index. Mean excess of daily Tmax over the
#' calendar-day 90th percentile threshold across all heatwave days in
#' the year. Returns `NA` for years with no heatwaves.
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
#' tail(ck_hwm(tmax, dates))
ck_hwm <- function(tmax, dates, ref_start = 1961L, ref_end = 1990L,
                   min_spell = 3L) {
  validate_numeric(tmax, "tmax")
  validate_dates(dates, length(tmax))
  stats <- .spell_family_stats(tmax, dates, ref_start, ref_end,
                               percentile = 0.90, op = ">",
                               min_spell = min_spell)
  build_result(as.character(stats$year), stats$m_mean,
               "hwm", "\u00b0C", "annual")
}

#' Heatwave Amplitude (HWA)
#'
#' ET-SCI heatwave family index. Peak excess of daily Tmax over the
#' calendar-day 90th percentile threshold across all heatwave days in
#' the year. Returns `NA` for years with no heatwaves.
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
#' tail(ck_hwa(tmax, dates))
ck_hwa <- function(tmax, dates, ref_start = 1961L, ref_end = 1990L,
                   min_spell = 3L) {
  validate_numeric(tmax, "tmax")
  validate_dates(dates, length(tmax))
  stats <- .spell_family_stats(tmax, dates, ref_start, ref_end,
                               percentile = 0.90, op = ">",
                               min_spell = min_spell)
  build_result(as.character(stats$year), stats$a_max,
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
  validate_numeric(tmin, "tmin")
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
  validate_numeric(tmin, "tmin")
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
  validate_numeric(tmin, "tmin")
  validate_dates(dates, length(tmin))
  stats <- .spell_family_stats(tmin, dates, ref_start, ref_end,
                               percentile = 0.10, op = "<",
                               min_spell = min_spell)
  build_result(as.character(stats$year), as.numeric(stats$d),
               "cwd", "days", "annual")
}

#' Cold-Wave Magnitude (CWM)
#'
#' ET-SCI cold-wave family index. Mean of (threshold - daily Tmin)
#' across all cold-wave days in the year, expressed as a positive
#' magnitude. Returns `NA` for years with no cold waves.
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
#' tail(ck_cwm(tmin, dates))
ck_cwm <- function(tmin, dates, ref_start = 1961L, ref_end = 1990L,
                   min_spell = 3L) {
  validate_numeric(tmin, "tmin")
  validate_dates(dates, length(tmin))
  stats <- .spell_family_stats(tmin, dates, ref_start, ref_end,
                               percentile = 0.10, op = "<",
                               min_spell = min_spell)
  build_result(as.character(stats$year), stats$m_mean,
               "cwm", "\u00b0C", "annual")
}

#' Cold-Wave Amplitude (CWA)
#'
#' ET-SCI cold-wave family index. Peak of (threshold - daily Tmin)
#' across all cold-wave days in the year, expressed as a positive
#' magnitude. Returns `NA` for years with no cold waves.
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
#' tail(ck_cwa(tmin, dates))
ck_cwa <- function(tmin, dates, ref_start = 1961L, ref_end = 1990L,
                   min_spell = 3L) {
  validate_numeric(tmin, "tmin")
  validate_dates(dates, length(tmin))
  stats <- .spell_family_stats(tmin, dates, ref_start, ref_end,
                               percentile = 0.10, op = "<",
                               min_spell = min_spell)
  build_result(as.character(stats$year), stats$a_max,
               "cwa", "\u00b0C", "annual")
}
