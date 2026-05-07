# Excess Heat Factor (EHF) following Nairn & Fawcett (2013).
#
# EHF combines a "significance" component (excess of recent 3-day mean
# daily temperature over the long-run 95th percentile from a reference
# period) with an "acclimatisation" component (excess of recent 3-day
# mean over the previous 30-day mean). EHF > 0 indicates heatwave
# conditions; larger values indicate more severe / less-acclimatised
# events.
#
#   DMT(i)        = (TX(i) + TN(i)) / 2
#   T(i, 3)       = mean(DMT(i-2 .. i))
#   T(i, 30)      = mean(DMT(i-32 .. i-3))
#   T95           = 95th percentile of DMT over the reference period
#   EHIsig(i)     = T(i, 3) - T95
#   EHIaccl(i)    = T(i, 3) - T(i, 30)
#   EHF(i)        = max(EHIsig(i), 1) * EHIaccl(i)
#
# Reference: Nairn JR, Fawcett RJB (2013). Defining heatwaves: heatwave
# defined as a heat-impact event servicing all community and business
# sectors in Australia. CAWCR Technical Report No. 060.

#' Compute daily Excess Heat Factor (Nairn & Fawcett 2013)
#'
#' Internal helper that returns the daily EHF time series alongside
#' its `T(i, 3)` and `T95` building blocks. Used by [ck_ehf()] and
#' available for users who need event-level rather than annual output.
#' @noRd
.ck_ehf_daily <- function(tmax, tmin, dates,
                          ref_start = 1961L, ref_end = 1990L) {
  validate_numeric(tmax, "tmax")
  validate_numeric(tmin, "tmin")
  validate_dates(dates, length(tmax))
  if (length(tmax) != length(tmin)) {
    cli::cli_abort("{.arg tmax} and {.arg tmin} must have the same length.")
  }

  dmt <- (tmax + tmin) / 2
  n <- length(dmt)

  # T(i, 3): trailing 3-day mean of DMT (i-2..i).
  t3 <- rep(NA_real_, n)
  for (i in 3:n) {
    win <- dmt[(i - 2L):i]
    if (all(is.na(win))) next
    t3[i] <- mean(win, na.rm = TRUE)
  }

  # T(i, 30): mean of DMT over the 30 days BEFORE the recent 3-day window
  # (i-32..i-3). Requires at least 33 days of history.
  t30 <- rep(NA_real_, n)
  for (i in 33:n) {
    win <- dmt[(i - 32L):(i - 3L)]
    if (all(is.na(win))) next
    t30[i] <- mean(win, na.rm = TRUE)
  }

  # T95: 95th percentile of DMT over the reference period.
  years <- as.integer(format(dates, "%Y"))
  in_ref <- years >= ref_start & years <= ref_end
  if (!any(in_ref)) {
    cli::cli_abort(
      "No data in reference period {ref_start}-{ref_end}; supply data covering the period or pass {.arg ref_start} / {.arg ref_end}."
    )
  }
  t95 <- stats::quantile(dmt[in_ref], 0.95, na.rm = TRUE,
                         names = FALSE, type = 8L)

  ehi_sig  <- t3 - t95
  ehi_accl <- t3 - t30
  ehf <- pmax(ehi_sig, 1) * ehi_accl

  list(dates = dates, dmt = dmt, t3 = t3, t30 = t30, t95 = t95,
       ehi_sig = ehi_sig, ehi_accl = ehi_accl, ehf = ehf)
}

#' Excess Heat Factor (EHF, Nairn & Fawcett 2013)
#'
#' Annual summary of the daily Excess Heat Factor heatwave intensity
#' metric. EHF combines a 3-day mean daily temperature anomaly above
#' the 95th percentile of the reference period with an acclimatisation
#' term (3-day mean minus previous 30-day mean). Positive EHF days
#' indicate heatwave conditions; larger values indicate more severe
#' or less-acclimatised events. This is the operational heatwave
#' metric used by the Australian Bureau of Meteorology.
#'
#' Three annual summaries are exposed via the `stat` argument:
#' \itemize{
#'   \item \code{"max"} (default): peak EHF in the year. Strongest
#'         single-day intensity.
#'   \item \code{"n_positive"}: count of days with `EHF > 0`. A
#'         frequency-of-heatwave-conditions measure.
#'   \item \code{"sum_positive"}: sum of EHF on days with `EHF > 0`.
#'         A severity-weighted total.
#' }
#'
#' @param tmax Numeric vector of daily maximum temperatures (degrees C).
#' @param tmin Numeric vector of daily minimum temperatures (degrees C).
#' @param dates Date vector of the same length as `tmax`. Must contain
#'   data covering the reference period.
#' @param ref_start,ref_end Integer. Reference period boundary years
#'   (inclusive). Defaults to 1961 and 1990.
#' @param stat One of `"max"`, `"n_positive"`, or `"sum_positive"`.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @references
#' Nairn, J. R., & Fawcett, R. J. B. (2013). Defining heatwaves:
#' heatwave defined as a heat-impact event servicing all community and
#' business sectors in Australia. *CAWCR Technical Report No. 060*.
#'
#' Perkins, S. E., & Alexander, L. V. (2013). On the measurement of
#' heatwaves. *Journal of Climate*, 26(13), 4500-4517.
#' \doi{10.1175/JCLI-D-12-00383.1}.
#'
#' @export
#' @examples
#' set.seed(1)
#' dates <- seq(as.Date("1961-01-01"), as.Date("1991-12-31"), by = "day")
#' s <- 2 * pi * as.integer(format(dates, "%j")) / 365
#' tmax <- 20 + 10 * sin(s) + rnorm(length(dates))
#' tmin <- 10 +  8 * sin(s) + rnorm(length(dates))
#' tail(ck_ehf(tmax, tmin, dates))
ck_ehf <- function(tmax, tmin, dates,
                   ref_start = 1961L, ref_end = 1990L,
                   stat = c("max", "n_positive", "sum_positive")) {
  stat <- match.arg(stat)

  daily <- .ck_ehf_daily(tmax, tmin, dates, ref_start, ref_end)
  ehf <- daily$ehf

  years <- as.integer(format(dates, "%Y"))
  unique_years <- unique(years)

  values <- vapply(unique_years, function(yr) {
    sel <- years == yr
    e <- ehf[sel]
    e <- e[!is.na(e)]
    if (length(e) == 0L) return(NA_real_)
    switch(stat,
      "max"          = max(e),
      "n_positive"   = as.numeric(sum(e > 0)),
      "sum_positive" = sum(e[e > 0])
    )
  }, numeric(1))

  unit <- switch(stat,
    "max"          = "\u00b0C^2",
    "n_positive"   = "days",
    "sum_positive" = "\u00b0C^2"
  )

  build_result(as.character(unique_years), values,
               paste0("ehf_", stat), unit, "annual")
}
