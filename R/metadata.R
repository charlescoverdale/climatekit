#' List All Available Climate Indices
#'
#' Returns a data frame listing every index that `climatekit` can compute,
#' along with its category, unit, and a short description.
#'
#' @return A data frame with columns `index`, `category`, `unit`, and
#'   `description`.
#'
#' @export
#' @examples
#' ck_available()
ck_available <- function() {
  d <- .ck_index_df()
  data.frame(
    index = d$index,
    category = d$category,
    unit = d$unit,
    description = d$description,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' Get Metadata for a Climate Index
#'
#' Returns metadata (unit, category, description, reference) for a named
#' climate index.
#'
#' @param index Character string. The index name (e.g. `"frost_days"`).
#'   Use [ck_available()] to see valid names.
#'
#' @return A list with elements `index`, `category`, `unit`, `description`,
#'   and `reference`.
#'
#' @export
#' @examples
#' ck_metadata("frost_days")
ck_metadata <- function(index) {
  if (!is.character(index) || length(index) != 1) {
    cli::cli_abort("{.arg index} must be a single character string.")
  }
  index <- .ck_resolve_index(index)
  d <- .ck_index_df()
  pos <- match(index, d$index)
  if (is.na(pos)) {
    cli::cli_abort(
      c("Unknown index {.val {index}}.",
        "i" = "Run {.fn ck_available} to see valid index names.")
    )
  }
  list(
    index = d$index[pos],
    category = d$category[pos],
    unit = d$unit[pos],
    description = d$description[pos],
    reference = d$reference[pos]
  )
}

#' Canonical ETCCDI 27 Indices
#'
#' Returns the 27 canonical Expert Team on Climate Change Detection and
#' Indices (ETCCDI) indices as documented by Alexander et al. (2006) and
#' Zhang et al. (2011), with each row showing the canonical short code,
#' full name, input variable, unit, definition, and the corresponding
#' `climatekit` function (or `NA` where the index is not yet
#' implemented).
#'
#' Use this table to audit coverage, locate the `ck_*` function for a
#' given ETCCDI code, or filter to indices that climatekit currently
#' implements: `subset(ck_etccdi_27(), !is.na(ck_function))`.
#'
#' @return A data frame with one row per canonical ETCCDI index and
#'   columns `code`, `name`, `variable`, `unit`, `definition`,
#'   `ck_function`, and `status`.
#'
#' @references
#' Alexander, L. V. et al. (2006). Global observed changes in daily
#' climate extremes of temperature and precipitation. *Journal of
#' Geophysical Research: Atmospheres*, 111(D5).
#' \doi{10.1029/2005JD006290}.
#'
#' Zhang, X. et al. (2011). Indices for monitoring changes in extremes
#' based on daily temperature and precipitation data. *Wiley
#' Interdisciplinary Reviews: Climate Change*, 2(6), 851-870.
#' \doi{10.1002/wcc.147}.
#'
#' @export
#' @examples
#' tab <- ck_etccdi_27()
#' head(tab)
#' # Indices currently implemented in climatekit:
#' subset(tab, !is.na(ck_function))[, c("code", "ck_function")]
#' # Coverage:
#' table(tab$status)
ck_etccdi_27 <- function() {
  rows <- list(
    # Temperature: extremes (4)
    c("TXx", "Max Tmax", "Tmax", "\u00b0C",
      "Maximum value of daily maximum temperature in the period.",
      "ck_txx", "implemented"),
    c("TNx", "Max Tmin", "Tmin", "\u00b0C",
      "Maximum value of daily minimum temperature in the period (warmest night).",
      "ck_tnx", "implemented"),
    c("TXn", "Min Tmax", "Tmax", "\u00b0C",
      "Minimum value of daily maximum temperature in the period (coldest day).",
      "ck_txn", "implemented"),
    c("TNn", "Min Tmin", "Tmin", "\u00b0C",
      "Minimum value of daily minimum temperature in the period (coldest night).",
      "ck_tnn", "implemented"),

    # Temperature: thresholds (4)
    c("FD", "Frost days", "Tmin", "days",
      "Annual count of days when Tmin < 0\u00b0C.",
      "ck_frost_days", "implemented"),
    c("ID", "Ice days", "Tmax", "days",
      "Annual count of days when Tmax < 0\u00b0C.",
      "ck_ice_days", "implemented"),
    c("SU", "Summer days", "Tmax", "days",
      "Annual count of days when Tmax > 25\u00b0C.",
      "ck_summer_days", "implemented"),
    c("TR", "Tropical nights", "Tmin", "days",
      "Annual count of days when Tmin > 20\u00b0C.",
      "ck_tropical_nights", "implemented"),

    # Temperature: percentile and spell (5)
    c("TX10p", "Cool days", "Tmax", "%",
      "Percentage of days when Tmax < 10th percentile of 1961-1990 calendar-day base.",
      "ck_tx10p", "implemented"),
    c("TN10p", "Cool nights", "Tmin", "%",
      "Percentage of days when Tmin < 10th percentile of 1961-1990 calendar-day base.",
      "ck_tn10p", "implemented"),
    c("TX90p", "Warm days", "Tmax", "%",
      "Percentage of days when Tmax > 90th percentile of 1961-1990 calendar-day base.",
      "ck_tx90p", "implemented"),
    c("TN90p", "Warm nights", "Tmin", "%",
      "Percentage of days when Tmin > 90th percentile of 1961-1990 calendar-day base.",
      "ck_tn90p", "implemented"),
    c("DTR", "Diurnal range", "Tmax, Tmin", "\u00b0C",
      "Mean of (Tmax - Tmin) across the period.",
      "ck_diurnal_range", "implemented"),

    # Temperature: spells (3)
    c("WSDI", "Warm spell duration", "Tmax", "days",
      "Annual count of days in spans of >=6 consecutive days with Tmax > 90th percentile of 1961-1990 calendar-day base.",
      "ck_wsdi", "implemented"),
    c("CSDI", "Cold spell duration", "Tmin", "days",
      "Annual count of days in spans of >=6 consecutive days with Tmin < 10th percentile of 1961-1990 calendar-day base.",
      "ck_csdi", "implemented"),
    c("GSL", "Growing season length", "Tmean", "days",
      "Length of growing season as defined by the first 6-day spell with Tmean > 5\u00b0C and the first 6-day spell with Tmean < 5\u00b0C after mid-year.",
      "ck_growing_season", "implemented"),

    # Precipitation: maxima and intensity (3)
    c("RX1day", "Max 1-day precipitation", "Precip", "mm",
      "Maximum precipitation amount in a single day within the period.",
      "ck_max_1day_precip", "implemented"),
    c("RX5day", "Max 5-day precipitation", "Precip", "mm",
      "Maximum precipitation total over any 5 consecutive days within the period.",
      "ck_max_5day_precip", "implemented"),
    c("SDII", "Simple daily intensity", "Precip", "mm/day",
      "Mean precipitation amount on wet days (Precip >= 1 mm).",
      "ck_precip_intensity", "implemented"),

    # Precipitation: thresholds (3)
    c("R10mm", "Heavy precipitation days", "Precip", "days",
      "Annual count of days when precipitation >= 10 mm.",
      "ck_heavy_precip", "implemented"),
    c("R20mm", "Very heavy precipitation days", "Precip", "days",
      "Annual count of days when precipitation >= 20 mm.",
      "ck_very_heavy_precip", "implemented"),
    c("Rnnmm", "Days with precipitation above threshold", "Precip", "days",
      "Annual count of days when precipitation >= user-supplied threshold.",
      "ck_heavy_precip", "implemented"),

    # Precipitation: spells (2)
    c("CDD", "Consecutive dry days", "Precip", "days",
      "Maximum number of consecutive days with precipitation < 1 mm.",
      "ck_dry_days", "implemented"),
    c("CWD", "Consecutive wet days", "Precip", "days",
      "Maximum number of consecutive days with precipitation >= 1 mm.",
      "ck_wet_days", "implemented"),

    # Precipitation: percentile and total (3)
    c("R95p", "Very wet days total", "Precip", "mm",
      "Annual total precipitation when daily precip > 95th percentile of 1961-1990 wet-day baseline.",
      "ck_r95p", "implemented"),
    c("R99p", "Extremely wet days total", "Precip", "mm",
      "Annual total precipitation when daily precip > 99th percentile of 1961-1990 wet-day baseline.",
      "ck_r99p", "implemented"),
    c("PRCPTOT", "Annual total wet-day precip", "Precip", "mm",
      "Annual total precipitation in wet days (Precip >= 1 mm).",
      "ck_total_precip", "implemented")
  )

  m <- do.call(rbind, rows)
  data.frame(
    code = m[, 1],
    name = m[, 2],
    variable = m[, 3],
    unit = m[, 4],
    definition = m[, 5],
    ck_function = m[, 6],
    status = m[, 7],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' Browse the climatekit Index Catalogue
#'
#' Filter the comprehensive climatekit index catalogue (see
#' [ck_catalogue()]) by sector, applicable standard (ETCCDI / ET-SCI /
#' agroclimatic / comfort / drought / energy), or a free-text search
#' across the function name, full name, and ETCCDI code.
#'
#' @param sector Character (length 1) or `NULL`. Filter to indices tagged
#'   with this sector. Common values: `"agriculture"`, `"health"`,
#'   `"water"`, `"energy"`. `NULL` (default) returns all rows.
#' @param standard Character (length 1) or `NULL`. Filter to indices
#'   under this standard. Common values: `"ETCCDI"`, `"ET-SCI"`,
#'   `"ETCCDI-approx"`, `"agroclimatic"`, `"comfort"`, `"drought"`,
#'   `"energy"`. `NULL` (default) returns all rows.
#' @param search Character (length 1) or `NULL`. Free-text search; rows
#'   are kept where the term appears (case-insensitive) in the function
#'   name, the full name, or the ETCCDI code.
#'
#' @return The catalogue, filtered to matching rows. Same column structure
#'   as [ck_catalogue()].
#'
#' @export
#' @examples
#' ck_browse(standard = "ETCCDI")
#' ck_browse(sector = "agriculture")
#' ck_browse(search = "heat")
ck_browse <- function(sector = NULL, standard = NULL, search = NULL) {
  cat <- ck_catalogue()

  if (!is.null(sector)) {
    if (!is.character(sector) || length(sector) != 1L) {
      cli::cli_abort("{.arg sector} must be a single character string or NULL.")
    }
    keep <- !is.na(cat$sector) & cat$sector == sector
    cat <- cat[keep, , drop = FALSE]
  }

  if (!is.null(standard)) {
    if (!is.character(standard) || length(standard) != 1L) {
      cli::cli_abort("{.arg standard} must be a single character string or NULL.")
    }
    cat <- cat[cat$standard == standard, , drop = FALSE]
  }

  if (!is.null(search)) {
    if (!is.character(search) || length(search) != 1L) {
      cli::cli_abort("{.arg search} must be a single character string or NULL.")
    }
    pat <- tolower(search)
    in_fn   <- grepl(pat, tolower(cat$ck_function), fixed = TRUE)
    in_name <- grepl(pat, tolower(cat$name),        fixed = TRUE)
    in_code <- grepl(pat, tolower(cat$code),        fixed = TRUE)
    in_code[is.na(in_code)] <- FALSE
    cat <- cat[in_fn | in_name | in_code, , drop = FALSE]
  }

  rownames(cat) <- NULL
  cat
}

#' climatekit Index Catalogue
#'
#' Returns the complete catalogue of climate indices implemented by
#' `climatekit`, with one row per `ck_*` function and columns covering
#' the canonical short code (where applicable), the full name, the
#' index family, the relevant sector, the unit, the source standard,
#' and the principal citation key.
#'
#' Use [ck_browse()] to filter by sector or standard.
#'
#' @return A data frame with columns `ck_function`, `code`, `name`,
#'   `category`, `sector`, `unit`, `standard`, and `citation_key`.
#'
#' @export
#' @examples
#' tab <- ck_catalogue()
#' head(tab)
#' # Tally indices by standard:
#' table(tab$standard)
ck_catalogue <- function() {
  d <- .ck_index_df()
  data.frame(
    ck_function  = d$ck_function,
    code         = d$code,
    name         = d$name,
    category     = d$category,
    sector       = d$sector,
    unit         = d$unit,
    standard     = d$standard,
    citation_key = d$citation_key,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
