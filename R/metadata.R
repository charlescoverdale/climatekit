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
  idx <- .index_registry()
  data.frame(
    index = vapply(idx, `[[`, character(1), "index"),
    category = vapply(idx, `[[`, character(1), "category"),
    unit = vapply(idx, `[[`, character(1), "unit"),
    description = vapply(idx, `[[`, character(1), "description"),
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
  registry <- .index_registry()
  names_vec <- vapply(registry, `[[`, character(1), "index")
  pos <- match(index, names_vec)
  if (is.na(pos)) {
    cli::cli_abort(
      c("Unknown index {.val {index}}.",
        "i" = "Run {.fn ck_available} to see valid index names.")
    )
  }
  registry[[pos]]
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
  rows <- list(
    # Temperature: ETCCDI canonical
    c("ck_frost_days",     "FD",     "Frost days",                              "temperature",   "agriculture", "days",       "ETCCDI",        "alexander2006global"),
    c("ck_ice_days",       "ID",     "Ice days",                                "temperature",   NA_character_, "days",       "ETCCDI",        "alexander2006global"),
    c("ck_summer_days",    "SU",     "Summer days",                             "temperature",   "health",      "days",       "ETCCDI",        "alexander2006global"),
    c("ck_tropical_nights","TR",     "Tropical nights",                         "temperature",   "health",      "days",       "ETCCDI",        "alexander2006global"),
    c("ck_txx",            "TXx",    "Max Tmax (warmest day)",                  "temperature",   NA_character_, "\u00b0C",    "ETCCDI",        "alexander2006global"),
    c("ck_tnx",            "TNx",    "Max Tmin (warmest night)",                "temperature",   NA_character_, "\u00b0C",    "ETCCDI",        "alexander2006global"),
    c("ck_txn",            "TXn",    "Min Tmax (coldest day)",                  "temperature",   NA_character_, "\u00b0C",    "ETCCDI",        "alexander2006global"),
    c("ck_tnn",            "TNn",    "Min Tmin (coldest night)",                "temperature",   NA_character_, "\u00b0C",    "ETCCDI",        "alexander2006global"),
    c("ck_tx10p",          "TX10p",  "Cool days",                               "temperature",   "health",      "%",          "ETCCDI",        "zhang2011indices"),
    c("ck_tn10p",          "TN10p",  "Cool nights",                             "temperature",   "health",      "%",          "ETCCDI",        "zhang2011indices"),
    c("ck_tx90p",          "TX90p",  "Warm days",                               "temperature",   "health",      "%",          "ETCCDI",        "zhang2011indices"),
    c("ck_tn90p",          "TN90p",  "Warm nights",                             "temperature",   "health",      "%",          "ETCCDI",        "zhang2011indices"),
    c("ck_wsdi",           "WSDI",   "Warm spell duration index",               "temperature",   "health",      "days",       "ETCCDI",        "zhang2011indices"),
    c("ck_csdi",           "CSDI",   "Cold spell duration index",               "temperature",   "health",      "days",       "ETCCDI",        "zhang2011indices"),
    c("ck_diurnal_range",  "DTR",    "Diurnal temperature range",               "temperature",   NA_character_, "\u00b0C",    "ETCCDI",        "alexander2006global"),
    c("ck_growing_season", "GSL",    "Growing season length",                   "temperature",   "agriculture", "days",       "ETCCDI",        "alexander2006global"),

    # Temperature: ETCCDI approximation
    c("ck_warm_spell",     "WSDI*",  "Warm spell days (series-quantile approx.)","temperature",  NA_character_, "days",       "ETCCDI-approx", "zhang2011indices"),

    # Temperature: energy and agriculture
    c("ck_heating_degree_days", NA_character_, "Heating degree days", "temperature", "energy",      "degree-days", "energy",       NA_character_),
    c("ck_cooling_degree_days", NA_character_, "Cooling degree days", "temperature", "energy",      "degree-days", "energy",       NA_character_),
    c("ck_growing_degree_days", NA_character_, "Growing degree days", "temperature", "agriculture", "degree-days", "agroclimatic", NA_character_),

    # Precipitation: ETCCDI canonical
    c("ck_total_precip",      "PRCPTOT",     "Total wet-day precipitation",        "precipitation", "water", "mm",     "ETCCDI", "alexander2006global"),
    c("ck_dry_days",          "CDD",         "Consecutive dry days",               "precipitation", "water", "days",   "ETCCDI", "alexander2006global"),
    c("ck_wet_days",          "CWD",         "Consecutive wet days",               "precipitation", "water", "days",   "ETCCDI", "alexander2006global"),
    c("ck_heavy_precip",      "R10mm/Rnnmm", "Heavy-precipitation days",           "precipitation", "water", "days",   "ETCCDI", "alexander2006global"),
    c("ck_very_heavy_precip", "R20mm",       "Very heavy precipitation days",      "precipitation", "water", "days",   "ETCCDI", "alexander2006global"),
    c("ck_max_1day_precip",   "RX1day",      "Max 1-day precipitation",            "precipitation", "water", "mm",     "ETCCDI", "alexander2006global"),
    c("ck_max_5day_precip",   "RX5day",      "Max 5-day precipitation",            "precipitation", "water", "mm",     "ETCCDI", "alexander2006global"),
    c("ck_precip_intensity",  "SDII",        "Simple daily intensity",             "precipitation", "water", "mm/day", "ETCCDI", "alexander2006global"),
    c("ck_r95p",              "R95p",        "Very wet days total",                "precipitation", "water", "mm",     "ETCCDI", "zhang2011indices"),
    c("ck_r99p",              "R99p",        "Extremely wet days total",           "precipitation", "water", "mm",     "ETCCDI", "zhang2011indices"),

    # Drought
    c("ck_spi",  NA_character_, "Standardised Precipitation Index",                 "drought", "water", "dimensionless", "drought", "mckee1993relationship"),
    c("ck_spei", NA_character_, "Standardised Precipitation-Evapotranspiration Index","drought","water","dimensionless", "drought", "vicente2010multiscalar"),
    c("ck_pet",  NA_character_, "Potential evapotranspiration (Hargreaves method)", "drought", "water", "mm",            "drought", "hargreaves1985reference"),

    # Agroclimatic
    c("ck_huglin",      NA_character_, "Huglin heliothermal index",    "agroclimatic", "agriculture", "degree-days",      "agroclimatic", "huglin1978nouveau"),
    c("ck_winkler",     NA_character_, "Winkler index",                "agroclimatic", "agriculture", "degree-days",      "agroclimatic", "winkler1974general"),
    c("ck_branas",      NA_character_, "Branas hydrothermal index",    "agroclimatic", "agriculture", "mm\u00b7\u00b0C",  "agroclimatic", NA_character_),
    c("ck_first_frost", NA_character_, "First autumn frost date",      "agroclimatic", "agriculture", "day of year",      "agroclimatic", NA_character_),
    c("ck_last_frost",  NA_character_, "Last spring frost date",       "agroclimatic", "agriculture", "day of year",      "agroclimatic", NA_character_),

    # Comfort
    c("ck_heat_index",  NA_character_, "Heat index (NWS apparent temperature)", "comfort", "health", "\u00b0C",   "comfort", "rothfusz1990heat"),
    c("ck_humidex",     NA_character_, "Canadian humidex",                      "comfort", "health", "unitless",  "comfort", "masterton1979humidex"),
    c("ck_wind_chill",  NA_character_, "Wind chill",                            "comfort", "health", "\u00b0C",   "comfort", "osczevski2005new"),
    c("ck_fire_danger", NA_character_, "Fire danger proxy",                     "comfort", NA_character_, "unitless", "comfort", NA_character_)
  )

  m <- do.call(rbind, rows)
  data.frame(
    ck_function  = m[, 1],
    code         = m[, 2],
    name         = m[, 3],
    category     = m[, 4],
    sector       = m[, 5],
    unit         = m[, 6],
    standard     = m[, 7],
    citation_key = m[, 8],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' Index registry (internal)
#' @noRd
.index_registry <- function() {
  list(
    # Temperature
    list(index = "frost_days", category = "temperature", unit = "days",
         description = "Count of days where Tmin < 0\u00b0C",
         reference = "ETCCDI"),
    list(index = "ice_days", category = "temperature", unit = "days",
         description = "Count of days where Tmax < 0\u00b0C",
         reference = "ETCCDI"),
    list(index = "summer_days", category = "temperature", unit = "days",
         description = "Count of days where Tmax > 25\u00b0C",
         reference = "ETCCDI"),
    list(index = "tropical_nights", category = "temperature", unit = "days",
         description = "Count of days where Tmin > 20\u00b0C",
         reference = "ETCCDI"),
    list(index = "growing_season", category = "temperature", unit = "days",
         description = "Growing season length (ETCCDI: 6-day spells of Tmean > 5\u00b0C)",
         reference = "ETCCDI"),
    list(index = "txx", category = "temperature", unit = "\u00b0C",
         description = "Annual or monthly maximum of daily Tmax (ETCCDI TXx)",
         reference = "ETCCDI"),
    list(index = "tnx", category = "temperature", unit = "\u00b0C",
         description = "Annual or monthly maximum of daily Tmin (ETCCDI TNx, warmest night)",
         reference = "ETCCDI"),
    list(index = "txn", category = "temperature", unit = "\u00b0C",
         description = "Annual or monthly minimum of daily Tmax (ETCCDI TXn, coldest day)",
         reference = "ETCCDI"),
    list(index = "tnn", category = "temperature", unit = "\u00b0C",
         description = "Annual or monthly minimum of daily Tmin (ETCCDI TNn, coldest night)",
         reference = "ETCCDI"),
    list(index = "tx10p", category = "temperature", unit = "%",
         description = "Percentage of cool days (ETCCDI TX10p, Tmax below calendar-day 10th percentile)",
         reference = "ETCCDI"),
    list(index = "tn10p", category = "temperature", unit = "%",
         description = "Percentage of cool nights (ETCCDI TN10p, Tmin below calendar-day 10th percentile)",
         reference = "ETCCDI"),
    list(index = "tx90p", category = "temperature", unit = "%",
         description = "Percentage of warm days (ETCCDI TX90p, Tmax above calendar-day 90th percentile)",
         reference = "ETCCDI"),
    list(index = "tn90p", category = "temperature", unit = "%",
         description = "Percentage of warm nights (ETCCDI TN90p, Tmin above calendar-day 90th percentile)",
         reference = "ETCCDI"),
    list(index = "heating_degree_days", category = "temperature", unit = "degree-days",
         description = "Sum of (base - Tavg) for days below base temperature",
         reference = "ASHRAE"),
    list(index = "cooling_degree_days", category = "temperature", unit = "degree-days",
         description = "Sum of (Tavg - base) for days above base temperature",
         reference = "ASHRAE"),
    list(index = "growing_degree_days", category = "temperature", unit = "degree-days",
         description = "Sum of (Tavg - base) for days above base temperature",
         reference = "McMaster & Wilhelm 1997"),
    list(index = "diurnal_range", category = "temperature", unit = "\u00b0C",
         description = "Mean daily temperature range (Tmax - Tmin)",
         reference = "ETCCDI"),
    list(index = "warm_spell", category = "temperature", unit = "days",
         description = "Warm spell days (simplified, see ck_wsdi for ETCCDI WSDI)",
         reference = "Simplified (see documentation)"),
    list(index = "wsdi", category = "temperature", unit = "days",
         description = "Warm spell duration index (ETCCDI WSDI, 1961-1990 calendar-day Tmax 90p base)",
         reference = "ETCCDI"),
    list(index = "csdi", category = "temperature", unit = "days",
         description = "Cold spell duration index (ETCCDI CSDI, 1961-1990 calendar-day Tmin 10p base)",
         reference = "ETCCDI"),

    # Precipitation
    list(index = "dry_days", category = "precipitation", unit = "days",
         description = "Maximum consecutive dry days (precip < threshold)",
         reference = "ETCCDI"),
    list(index = "wet_days", category = "precipitation", unit = "days",
         description = "Maximum consecutive wet days (precip >= threshold)",
         reference = "ETCCDI"),
    list(index = "total_precip", category = "precipitation", unit = "mm",
         description = "Total precipitation by period",
         reference = "ETCCDI"),
    list(index = "heavy_precip", category = "precipitation", unit = "days",
         description = "Count of days with precipitation >= threshold",
         reference = "ETCCDI"),
    list(index = "very_heavy_precip", category = "precipitation", unit = "days",
         description = "Count of days with precipitation >= threshold",
         reference = "ETCCDI"),
    list(index = "max_1day_precip", category = "precipitation", unit = "mm",
         description = "Maximum 1-day precipitation",
         reference = "ETCCDI"),
    list(index = "max_5day_precip", category = "precipitation", unit = "mm",
         description = "Maximum 5-day precipitation total",
         reference = "ETCCDI"),
    list(index = "precip_intensity", category = "precipitation", unit = "mm/day",
         description = "Mean precipitation on wet days (SDII)",
         reference = "ETCCDI"),
    list(index = "r95p", category = "precipitation", unit = "mm",
         description = "Annual total precipitation on days above 95th percentile of 1961-1990 wet-day baseline (ETCCDI R95p)",
         reference = "ETCCDI"),
    list(index = "r99p", category = "precipitation", unit = "mm",
         description = "Annual total precipitation on days above 99th percentile of 1961-1990 wet-day baseline (ETCCDI R99p)",
         reference = "ETCCDI"),

    # Drought
    list(index = "spi", category = "drought", unit = "dimensionless",
         description = "Standardized Precipitation Index",
         reference = "McKee et al. 1993"),
    list(index = "spei", category = "drought", unit = "dimensionless",
         description = "Standardized Precipitation-Evapotranspiration Index",
         reference = "Vicente-Serrano et al. 2010"),
    list(index = "pet", category = "drought", unit = "mm",
         description = "Potential evapotranspiration (Hargreaves method)",
         reference = "Hargreaves & Samani 1985"),

    # Agroclimatic
    list(index = "huglin", category = "agroclimatic", unit = "degree-days",
         description = "Huglin heliothermal index for viticulture",
         reference = "Huglin 1978"),
    list(index = "winkler", category = "agroclimatic", unit = "degree-days",
         description = "Winkler index (growing degree days for wine regions)",
         reference = "Amerine & Winkler 1944"),
    list(index = "branas", category = "agroclimatic", unit = "mm\u00b7\u00b0C",
         description = "Branas hydrothermal index",
         reference = "Branas et al. 1946"),
    list(index = "first_frost", category = "agroclimatic", unit = "day of year",
         description = "Date of first autumn frost (Tmin < 0\u00b0C)",
         reference = ""),
    list(index = "last_frost", category = "agroclimatic", unit = "day of year",
         description = "Date of last spring frost (Tmin < 0\u00b0C)",
         reference = ""),

    # Comfort
    list(index = "wind_chill", category = "comfort", unit = "\u00b0C",
         description = "Wind chill temperature",
         reference = "Environment Canada / NWS"),
    list(index = "heat_index", category = "comfort", unit = "\u00b0C",
         description = "Heat index (apparent temperature)",
         reference = "Rothfusz 1990"),
    list(index = "humidex", category = "comfort", unit = "unitless",
         description = "Canadian humidex",
         reference = "Masterson & Richardson 1979"),
    list(index = "fire_danger", category = "comfort", unit = "unitless",
         description = "Simplified fire danger proxy (not FWI)",
         reference = "")
  )
}
