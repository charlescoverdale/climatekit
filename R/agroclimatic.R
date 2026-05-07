#' Huglin Heliothermal Index
#'
#' The Huglin index is used in viticulture to characterise the thermal
#' potential of a region for grape growing. It is computed over the growing
#' season (April 1 to September 30 in the Northern Hemisphere; October 1 to
#' March 31 in the Southern Hemisphere).
#'
#' @param tmin Numeric vector of daily minimum temperatures (degrees C).
#' @param tmax Numeric vector of daily maximum temperatures (degrees C).
#' @param dates Date vector of the same length as `tmin`.
#' @param lat Numeric. Latitude in decimal degrees (used to determine
#'   hemisphere and day-length coefficient).
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @references Huglin, P. (1978). Nouveau mode d'evaluation des possibilites
#'   heliothermiques d'un milieu viticole. *Comptes Rendus de l'Academie
#'   d'Agriculture de France*, 64, 1117-1126.
#'
#' @export
#' @examples
#' dates <- seq(as.Date("2024-04-01"), as.Date("2024-09-30"), by = "day")
#' set.seed(42)
#' tmin <- rnorm(length(dates), mean = 12, sd = 3)
#' tmax <- tmin + runif(length(dates), 8, 15)
#' ck_huglin(tmin, tmax, dates, lat = 45)
ck_huglin <- function(tmin, tmax, dates, lat) {
  validate_numeric(tmin, "tmin")
  validate_numeric(tmax, "tmax")
  validate_dates(dates, length(tmin))
  if (length(tmin) != length(tmax)) {
    cli::cli_abort("{.arg tmin} and {.arg tmax} must have the same length.")
  }
  if (!is.numeric(lat) || length(lat) != 1) {
    cli::cli_abort("{.arg lat} must be a single numeric value.")
  }

  # Day-length coefficient (k) based on latitude (Huglin 1978)
  abs_lat <- abs(lat)
  k <- if (abs_lat <= 40) {
    1.02
  } else if (abs_lat >= 50) {
    1.06
  } else {
    1.02 + (abs_lat - 40) * 0.004
  }

  years <- as.integer(format(dates, "%Y"))
  unique_years <- unique(years)
  months <- as.integer(format(dates, "%m"))

  values <- vapply(unique_years, function(yr) {
    # Growing season months
    if (lat >= 0) {
      gs_mask <- years == yr & months >= 4 & months <= 9
    } else {
      # Southern hemisphere: Oct-Mar spans two calendar years
      gs_mask <- (years == yr & months >= 10) | (years == (yr + 1L) & months <= 3)
    }

    if (!any(gs_mask)) return(NA_real_)

    tavg_gs <- (tmin[gs_mask] + tmax[gs_mask]) / 2
    tmax_gs <- tmax[gs_mask]

    daily_hi <- pmax((tavg_gs - 10) + (tmax_gs - 10), 0) / 2 * k
    sum(daily_hi, na.rm = TRUE)
  }, numeric(1))

  build_result(as.character(unique_years), values, "huglin", "degree-days", "annual")
}

#' Winkler Index
#'
#' The Winkler index (also called growing degree days for viticulture)
#' accumulates daily mean temperature above 10 degrees C during the growing season
#' (April-October in NH, October-April in SH).
#'
#' @param tavg Numeric vector of daily average temperatures (degrees C).
#' @param dates Date vector of the same length as `tavg`.
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @references Amerine, M. A., & Winkler, A. J. (1944). Composition and
#'   quality of musts and wines of California grapes.
#'
#' @export
#' @examples
#' dates <- seq(as.Date("2024-04-01"), as.Date("2024-10-31"), by = "day")
#' set.seed(42)
#' tavg <- rnorm(length(dates), mean = 18, sd = 4)
#' ck_winkler(tavg, dates)
ck_winkler <- function(tavg, dates) {
  validate_numeric(tavg, "tavg")
  validate_dates(dates, length(tavg))

  years <- as.integer(format(dates, "%Y"))
  months <- as.integer(format(dates, "%m"))
  unique_years <- unique(years)

  values <- vapply(unique_years, function(yr) {
    gs_mask <- years == yr & months >= 4 & months <= 10
    if (!any(gs_mask)) return(NA_real_)
    sum(pmax(tavg[gs_mask] - 10, 0), na.rm = TRUE)
  }, numeric(1))

  build_result(as.character(unique_years), values, "winkler", "degree-days", "annual")
}

#' Branas Hydrothermal Index
#'
#' The Branas index combines temperature and precipitation during the
#' growing season to estimate disease pressure (especially downy mildew)
#' in vineyards. It is the sum of (monthly mean temperature) times
#' (monthly precipitation total) over the five months of the growing
#' season: April-August in the Northern Hemisphere; October-February
#' in the Southern Hemisphere. The Southern Hemisphere season spans
#' two calendar years and is reported under the year in which it
#' starts.
#'
#' @param precip Numeric vector of daily precipitation (mm).
#' @param tavg Numeric vector of daily mean temperatures (degrees C).
#' @param dates Date vector of the same length as `precip`.
#' @param lat Numeric. Latitude in decimal degrees, used to select the
#'   hemisphere convention. Default 50 (Northern Hemisphere).
#'
#' @return A data frame with columns `period`, `value`, `index`, and `unit`.
#'
#' @references Branas, J., Bernon, G., & Levadoux, L. (1946).
#'   Elements de viticulture generale.
#'
#' @export
#' @examples
#' dates <- seq(as.Date("2024-04-01"), as.Date("2024-08-31"), by = "day")
#' set.seed(42)
#' tavg <- rnorm(length(dates), mean = 12, sd = 3)
#' precip <- rgamma(length(dates), shape = 0.5, rate = 0.2)
#' ck_branas(precip, tavg, dates)
ck_branas <- function(precip, tavg, dates, lat = 50) {
  validate_numeric(precip, "precip")
  validate_numeric(tavg, "tavg")
  validate_dates(dates, length(precip))
  if (length(precip) != length(tavg)) {
    cli::cli_abort("{.arg precip} and {.arg tavg} must have the same length.")
  }
  if (!is.numeric(lat) || length(lat) != 1L) {
    cli::cli_abort("{.arg lat} must be a single numeric value.")
  }

  years <- as.integer(format(dates, "%Y"))
  month_labels <- format(dates, "%Y-%m")
  unique_years <- unique(years)

  # Per-year list of "year-month" labels covering the five-month
  # growing season for each hemisphere.
  season_months <- function(yr) {
    if (lat >= 0) {
      # NH: April through August of the same calendar year.
      paste0(yr, "-", sprintf("%02d", 4:8))
    } else {
      # SH: October-December of yr, January-February of yr + 1.
      c(paste0(yr,    "-", sprintf("%02d", 10:12)),
        paste0(yr + 1L, "-", sprintf("%02d",  1:2)))
    }
  }

  values <- vapply(unique_years, function(yr) {
    total <- 0
    for (m in season_months(yr)) {
      idx <- which(month_labels == m)
      if (length(idx) == 0) next
      monthly_t <- mean(tavg[idx], na.rm = TRUE)
      monthly_p <- sum(precip[idx], na.rm = TRUE)
      total <- total + monthly_t * monthly_p
    }
    total
  }, numeric(1))

  build_result(as.character(unique_years), values, "branas",
               "mm\u00b7\u00b0C", "annual")
}

#' First Frost Date
#'
#' Day of year of the first autumn frost (Tmin < 0 degrees C) in each
#' year. Hemisphere is selected by `lat`: in the Northern Hemisphere
#' (`lat >= 0`) the search starts at July 1 (DOY 183); in the Southern
#' Hemisphere it starts at March 1 (DOY 60), matching the autumn entry
#' for each.
#'
#' @param tmin Numeric vector of daily minimum temperatures (degrees C).
#' @param dates Date vector of the same length as `tmin`.
#' @param lat Numeric. Latitude in decimal degrees, used to select the
#'   hemisphere convention. Default 50 (Northern Hemisphere).
#'
#' @return A data frame with columns `period`, `value` (day of year),
#'   `date` (the frost date), `index`, and `unit`.
#'
#' @export
#' @examples
#' dates <- seq(as.Date("2024-07-01"), as.Date("2024-12-31"), by = "day")
#' set.seed(42)
#' tmin <- 15 - seq_along(dates) * 0.15 + rnorm(length(dates), sd = 3)
#' ck_first_frost(tmin, dates)
ck_first_frost <- function(tmin, dates, lat = 50) {
  validate_numeric(tmin, "tmin")
  validate_dates(dates, length(tmin))
  if (!is.numeric(lat) || length(lat) != 1L) {
    cli::cli_abort("{.arg lat} must be a single numeric value.")
  }

  years <- as.integer(format(dates, "%Y"))
  doy <- as.integer(format(dates, "%j"))
  unique_years <- unique(years)

  # NH autumn: search after July 1 (DOY 183).
  # SH autumn: search after March 1 (DOY 60).
  cutoff <- if (lat >= 0) 183L else 60L

  rows <- lapply(unique_years, function(yr) {
    mask <- years == yr & doy >= cutoff & !is.na(tmin) & tmin < 0
    if (!any(mask)) {
      data.frame(period = as.Date(paste0(yr, "-01-01")),
                 value = NA_real_, date = as.Date(NA),
                 index = "first_frost", unit = "day of year",
                 stringsAsFactors = FALSE)
    } else {
      first_idx <- which(mask)[1]
      data.frame(period = as.Date(paste0(yr, "-01-01")),
                 value = as.double(doy[first_idx]),
                 date = dates[first_idx],
                 index = "first_frost", unit = "day of year",
                 stringsAsFactors = FALSE)
    }
  })
  do.call(rbind, rows)
}

#' Last Frost Date
#'
#' Day of year of the last spring frost (Tmin < 0 degrees C) in each
#' year. Hemisphere is selected by `lat`: in the Northern Hemisphere
#' the search runs up to July 1 (DOY 183); in the Southern Hemisphere
#' up to October 1 (DOY 274), matching the spring boundary for each.
#'
#' @param tmin Numeric vector of daily minimum temperatures (degrees C).
#' @param dates Date vector of the same length as `tmin`.
#' @param lat Numeric. Latitude in decimal degrees, used to select the
#'   hemisphere convention. Default 50 (Northern Hemisphere).
#'
#' @return A data frame with columns `period`, `value` (day of year),
#'   `date` (the frost date), `index`, and `unit`.
#'
#' @export
#' @examples
#' dates <- seq(as.Date("2024-01-01"), as.Date("2024-06-30"), by = "day")
#' set.seed(42)
#' tmin <- -10 + seq_along(dates) * 0.12 + rnorm(length(dates), sd = 3)
#' ck_last_frost(tmin, dates)
ck_last_frost <- function(tmin, dates, lat = 50) {
  validate_numeric(tmin, "tmin")
  validate_dates(dates, length(tmin))
  if (!is.numeric(lat) || length(lat) != 1L) {
    cli::cli_abort("{.arg lat} must be a single numeric value.")
  }

  years <- as.integer(format(dates, "%Y"))
  doy <- as.integer(format(dates, "%j"))
  unique_years <- unique(years)

  # NH spring: search up to July 1 (DOY 183).
  # SH spring: search up to October 1 (DOY 274).
  cutoff <- if (lat >= 0) 183L else 274L

  rows <- lapply(unique_years, function(yr) {
    mask <- years == yr & doy < cutoff & !is.na(tmin) & tmin < 0
    if (!any(mask)) {
      data.frame(period = as.Date(paste0(yr, "-01-01")),
                 value = NA_real_, date = as.Date(NA),
                 index = "last_frost", unit = "day of year",
                 stringsAsFactors = FALSE)
    } else {
      last_idx <- max(which(mask))
      data.frame(period = as.Date(paste0(yr, "-01-01")),
                 value = as.double(doy[last_idx]),
                 date = dates[last_idx],
                 index = "last_frost", unit = "day of year",
                 stringsAsFactors = FALSE)
    }
  })
  do.call(rbind, rows)
}
