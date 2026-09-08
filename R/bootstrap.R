# Zhang et al. (2005) in-base bootstrap for percentile-day indices.
#
# When a percentile-based index (TX10p / TN10p / TX90p / TN90p) is
# computed for a year that lies inside the reference period, the year
# being analysed contributes to its own threshold and biases the
# result toward the central tendency. The Zhang (2005) bootstrap
# corrects this by leaving the analysis year out and substituting one
# of the other (N - 1) reference years in turn, then averaging the
# resulting exceedance proportions.
#
# Reference: Zhang, X., Hegerl, G. C., Zwiers, F. W., & Kenyon, J.
# (2005). Avoiding inhomogeneity in percentile-based indices of
# temperature extremes. *Journal of Climate*, 18(11), 1641-1651.
# \doi{10.1175/JCLI3366.1}.

#' Bootstrap-corrected per-day exceedance proportions
#'
#' Returns a numeric vector aligned with `values` and `dates`.
#' Out-of-base years yield 0/1 booleans (NA where the value or
#' threshold is missing). In-base years yield averages in `[0, 1]`
#' from the leave-one-out resampling. Used by [ck_tx10p()],
#' [ck_tn10p()], [ck_tx90p()], and [ck_tn90p()] when
#' `bootstrap = TRUE`.
#' @noRd
.bootstrap_pct_exceedance <- function(values, dates, percentile, op,
                                      ref_start, ref_end, window = 5L) {
  doy <- as.integer(format(dates, "%j"))
  years <- as.integer(format(dates, "%Y"))

  in_ref <- years >= ref_start & years <= ref_end
  if (!any(in_ref)) {
    cli::cli_abort(
      "No data in reference period {ref_start}-{ref_end}; supply data covering the period or pass {.arg ref_start} / {.arg ref_end}."
    )
  }

  # Standard full-period thresholds for out-of-base years.
  std_thresh <- .calendar_day_percentile(values, dates, percentile,
                                         ref_start, ref_end, window = window)

  out <- rep(NA_real_, length(values))

  # --- Out-of-base years: standard binary exceedance ---
  oob_idx <- which(!in_ref)
  if (length(oob_idx) > 0L) {
    oob_thr <- std_thresh[doy[oob_idx]]
    oob_vals <- values[oob_idx]
    valid <- !is.na(oob_vals) & !is.na(oob_thr)
    if (identical(op, ">")) {
      out[oob_idx] <- ifelse(valid, as.numeric(oob_vals > oob_thr), NA_real_)
    } else {
      out[oob_idx] <- ifelse(valid, as.numeric(oob_vals < oob_thr), NA_real_)
    }
  }

  # --- In-base years: leave-one-out bootstrap ---
  #
  # The naive implementation rebuilds the calendar-day percentile pool
  # from the full `values` vector for every (y, w) pair, costing
  # O(N^2 * 366 * N_window) operations for an N-year reference. The
  # optimised implementation precomputes per-year-per-DOY contribution
  # vectors once, then for each (y, w) simply concatenates the slices
  # for "all years except y, plus year w again" and runs `quantile` on
  # the result. That cuts the inner cost to O(366) per (y, w) and the
  # total to O(N^2 * 366), roughly a 30x speedup for a 30-year base.
  ref_years <- seq(ref_start, ref_end)
  n_ref <- length(ref_years)
  half  <- (window - 1L) %/% 2L

  # Per-year-per-DOY contribution: year_doy_pool[[i]][[d]] = numeric
  # vector of values from ref-year `ref_years[i]` whose day-of-year
  # falls in the (window)-day window centred on calendar day d.
  year_doy_pool <- vector("list", n_ref)
  for (i in seq_along(ref_years)) {
    yr <- ref_years[i]
    yr_idx_full <- which(years == yr)
    yr_doy_full <- doy[yr_idx_full]
    yr_vals_full <- values[yr_idx_full]
    pool_i <- vector("list", 366L)
    for (d in seq_len(366L)) {
      target_doys <- ((d - half - 1L):(d + half - 1L)) %% 366L + 1L
      slice <- yr_vals_full[yr_doy_full %in% target_doys]
      slice <- slice[!is.na(slice)]
      pool_i[[d]] <- slice
    }
    year_doy_pool[[i]] <- pool_i
  }

  for (i_y in seq_along(ref_years)) {
    yr <- ref_years[i_y]
    yr_idx <- which(years == yr)
    if (length(yr_idx) == 0L) next

    yr_doy <- doy[yr_idx]
    yr_vals <- values[yr_idx]

    other_idx <- setdiff(seq_along(ref_years), i_y)

    # Single-year reference: bootstrap is undefined; fall back to standard.
    if (length(other_idx) == 0L) {
      yr_thr <- std_thresh[yr_doy]
      valid <- !is.na(yr_vals) & !is.na(yr_thr)
      if (identical(op, ">")) {
        out[yr_idx] <- ifelse(valid, as.numeric(yr_vals > yr_thr), NA_real_)
      } else {
        out[yr_idx] <- ifelse(valid, as.numeric(yr_vals < yr_thr), NA_real_)
      }
      next
    }

    # Pool with year `i_y` removed, per DOY (one-time per y).
    minus_y_pool <- vector("list", 366L)
    for (d in seq_len(366L)) {
      minus_y_pool[[d]] <- unlist(
        lapply(other_idx, function(j) year_doy_pool[[j]][[d]]),
        use.names = FALSE
      )
    }

    bs_sum   <- rep(0, length(yr_idx))
    bs_count <- rep(0L, length(yr_idx))

    for (i_w in other_idx) {
      # Synthetic ref pool: everything except year y, with year w's
      # values appended (representing the substitution for year y).
      mod_thresh <- vapply(seq_len(366L), function(d) {
        pool <- c(minus_y_pool[[d]], year_doy_pool[[i_w]][[d]])
        if (length(pool) == 0L) return(NA_real_)
        stats::quantile(pool, percentile, na.rm = TRUE,
                        names = FALSE, type = 8L)
      }, numeric(1))

      yr_thr <- mod_thresh[yr_doy]
      valid <- !is.na(yr_vals) & !is.na(yr_thr)
      if (identical(op, ">")) {
        ex <- ifelse(valid, as.numeric(yr_vals > yr_thr), NA_real_)
      } else {
        ex <- ifelse(valid, as.numeric(yr_vals < yr_thr), NA_real_)
      }
      bs_sum[valid]   <- bs_sum[valid]   + ex[valid]
      bs_count[valid] <- bs_count[valid] + 1L
    }

    out[yr_idx] <- ifelse(bs_count > 0L, bs_sum / bs_count, NA_real_)
  }

  out
}

#' Per-day exceedance proportions for percentile indices
#'
#' Single entry point for the percentile-day functions. When
#' `bootstrap = FALSE`, returns standard 0/1 booleans (NA where the
#' value or threshold is missing). When `bootstrap = TRUE`, applies
#' the Zhang (2005) leave-one-out bootstrap for in-base years.
#' @noRd
.pct_exceedance_per_day <- function(values, dates, percentile, op,
                                    ref_start, ref_end,
                                    bootstrap = FALSE, window = 5L) {
  if (isTRUE(bootstrap)) {
    return(.bootstrap_pct_exceedance(values, dates, percentile, op,
                                     ref_start, ref_end, window))
  }

  thresholds <- .calendar_day_percentile(values, dates, percentile,
                                         ref_start, ref_end, window = window)
  doy <- as.integer(format(dates, "%j"))
  thr <- thresholds[doy]
  valid <- !is.na(values) & !is.na(thr)

  if (identical(op, ">")) {
    ifelse(valid, as.numeric(values > thr), NA_real_)
  } else if (identical(op, "<")) {
    ifelse(valid, as.numeric(values < thr), NA_real_)
  } else {
    cli::cli_abort("Internal: unsupported {.arg op}.")
  }
}

#' Aggregate per-day exceedance proportions to per-period percentages
#'
#' Helper used by the percentile-day functions to convert a daily
#' soft exceedance vector (in `[0, 1]` or NA) into a per-period
#' percentage. Periods with no valid days return NA.
#' @noRd
.aggregate_pct_exceedance <- function(soft_ex, dates, period) {
  periods <- get_periods(dates, period)
  unique_periods <- unique(periods)
  values <- vapply(unique_periods, function(p) {
    e <- soft_ex[periods == p]
    e <- e[!is.na(e)]
    if (length(e) == 0L) return(NA_real_)
    100 * mean(e)
  }, numeric(1))
  list(periods = unique_periods, values = values)
}
