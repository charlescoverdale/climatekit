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
         description = "Growing season length (first to last 5-day spell > 5\u00b0C)",
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
         description = "Warm spell duration index",
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
    list(index = "fire_weather", category = "comfort", unit = "unitless",
         description = "Simplified fire weather index",
         reference = "Van Wagner 1987")
  )
}
