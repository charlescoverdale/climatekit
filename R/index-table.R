# Single source of truth for the climatekit index table.
#
# Every public view of "what indices exist" is derived from `.ck_index_table()`:
# `ck_available()` and `ck_metadata()` (the discovery layer), `ck_catalogue()`
# and `ck_browse()` (the catalogue layer), and the `ck_compute()` dispatch.
# Before v0.2.1 these were three hand-maintained lists that had drifted apart,
# so the v0.2.0 heatwave, cold-wave, EHF and FAO-56 additions were reachable
# through `ck_compute()` but invisible to `ck_available()` and `ck_metadata()`.
# Add a new index here and all four views pick it up.
#
# Columns:
#   index        short name used by ck_compute() and ck_metadata()
#   ck_function  exported function that computes it
#   code         ETCCDI / ET-SCI code, NA where the index has none
#   name         full human-readable name
#   category     temperature / precipitation / drought / comfort / agroclimatic
#   sector       agriculture / health / water / energy, NA where not sector-tagged
#   unit         unit of the returned value
#   standard     ETCCDI / ETCCDI-approx / ET-SCI / agroclimatic / comfort /
#                drought / energy
#   citation_key BibTeX key for the defining reference
#   args         comma-separated data columns ck_compute() requires
#   description  one-line description, shown by ck_available()
#   reference    short citation, shown by ck_metadata()

#' The climatekit index table
#'
#' @return A character matrix with one row per index and the columns listed
#'   above. Internal; use [ck_catalogue()] or [ck_available()] instead.
#' @noRd
.ck_index_table <- function() {
  rows <- list(
    c("frost_days", "ck_frost_days", "FD", "Frost days", "temperature", "agriculture", "days", "ETCCDI", "alexander2006global", "tmin,dates", "Count of days where Tmin < 0\u00b0C", "ETCCDI"),
    c("ice_days", "ck_ice_days", "ID", "Ice days", "temperature", NA_character_, "days", "ETCCDI", "alexander2006global", "tmax,dates", "Count of days where Tmax < 0\u00b0C", "ETCCDI"),
    c("summer_days", "ck_summer_days", "SU", "Summer days", "temperature", "health", "days", "ETCCDI", "alexander2006global", "tmax,dates", "Count of days where Tmax > 25\u00b0C", "ETCCDI"),
    c("tropical_nights", "ck_tropical_nights", "TR", "Tropical nights", "temperature", "health", "days", "ETCCDI", "alexander2006global", "tmin,dates", "Count of days where Tmin > 20\u00b0C", "ETCCDI"),
    c("txx", "ck_txx", "TXx", "Max Tmax (warmest day)", "temperature", NA_character_, "\u00b0C", "ETCCDI", "alexander2006global", "tmax,dates", "Annual or monthly maximum of daily Tmax (ETCCDI TXx)", "ETCCDI"),
    c("tnx", "ck_tnx", "TNx", "Max Tmin (warmest night)", "temperature", NA_character_, "\u00b0C", "ETCCDI", "alexander2006global", "tmin,dates", "Annual or monthly maximum of daily Tmin (ETCCDI TNx, warmest night)", "ETCCDI"),
    c("txn", "ck_txn", "TXn", "Min Tmax (coldest day)", "temperature", NA_character_, "\u00b0C", "ETCCDI", "alexander2006global", "tmax,dates", "Annual or monthly minimum of daily Tmax (ETCCDI TXn, coldest day)", "ETCCDI"),
    c("tnn", "ck_tnn", "TNn", "Min Tmin (coldest night)", "temperature", NA_character_, "\u00b0C", "ETCCDI", "alexander2006global", "tmin,dates", "Annual or monthly minimum of daily Tmin (ETCCDI TNn, coldest night)", "ETCCDI"),
    c("tx10p", "ck_tx10p", "TX10p", "Cool days", "temperature", "health", "%", "ETCCDI", "zhang2011indices", "tmax,dates", "Percentage of cool days (ETCCDI TX10p, Tmax below calendar-day 10th percentile)", "ETCCDI"),
    c("tn10p", "ck_tn10p", "TN10p", "Cool nights", "temperature", "health", "%", "ETCCDI", "zhang2011indices", "tmin,dates", "Percentage of cool nights (ETCCDI TN10p, Tmin below calendar-day 10th percentile)", "ETCCDI"),
    c("tx90p", "ck_tx90p", "TX90p", "Warm days", "temperature", "health", "%", "ETCCDI", "zhang2011indices", "tmax,dates", "Percentage of warm days (ETCCDI TX90p, Tmax above calendar-day 90th percentile)", "ETCCDI"),
    c("tn90p", "ck_tn90p", "TN90p", "Warm nights", "temperature", "health", "%", "ETCCDI", "zhang2011indices", "tmin,dates", "Percentage of warm nights (ETCCDI TN90p, Tmin above calendar-day 90th percentile)", "ETCCDI"),
    c("wsdi", "ck_wsdi", "WSDI", "Warm spell duration index", "temperature", "health", "days", "ETCCDI", "zhang2011indices", "tmax,dates", "Warm spell duration index (ETCCDI WSDI, 1961-1990 calendar-day Tmax 90p base)", "ETCCDI"),
    c("csdi", "ck_csdi", "CSDI", "Cold spell duration index", "temperature", "health", "days", "ETCCDI", "zhang2011indices", "tmin,dates", "Cold spell duration index (ETCCDI CSDI, 1961-1990 calendar-day Tmin 10p base)", "ETCCDI"),
    c("diurnal_range", "ck_diurnal_range", "DTR", "Diurnal temperature range", "temperature", NA_character_, "\u00b0C", "ETCCDI", "alexander2006global", "tmin,tmax,dates", "Mean daily temperature range (Tmax - Tmin)", "ETCCDI"),
    c("growing_season", "ck_growing_season", "GSL", "Growing season length", "temperature", "agriculture", "days", "ETCCDI", "alexander2006global", "tavg,dates", "Growing season length (ETCCDI: 6-day spells of Tmean > 5\u00b0C)", "ETCCDI"),
    c("warm_spell", "ck_warm_spell", "WSDI*", "Warm spell days (series-quantile approx.)", "temperature", NA_character_, "days", "ETCCDI-approx", "zhang2011indices", "tmax,dates", "Warm spell days (simplified, see ck_wsdi for ETCCDI WSDI)", "Simplified (see documentation)"),
    c("hwn", "ck_hwn", "HWN", "Heatwave number", "temperature", "health", "events", "ET-SCI", "perkins2013measurement", "tmax,dates", "Annual count of heatwave events (ET-SCI HWN; Tmax above the calendar-day 90th percentile for at least 3 days)", "Perkins & Alexander 2013"),
    c("hwf", "ck_hwf", "HWF", "Heatwave frequency", "temperature", "health", "days", "ET-SCI", "perkins2013measurement", "tmax,dates", "Annual count of days contributing to heatwaves (ET-SCI HWF)", "Perkins & Alexander 2013"),
    c("hwd", "ck_hwd", "HWD", "Heatwave duration", "temperature", "health", "days", "ET-SCI", "perkins2013measurement", "tmax,dates", "Length of the longest heatwave in the period (ET-SCI HWD)", "Perkins & Alexander 2013"),
    c("hwm", "ck_hwm", "HWM", "Heatwave magnitude", "temperature", "health", "\u00b0C", "ET-SCI", "perkins2013measurement", "tmax,dates", "Mean temperature across heatwave days (ET-SCI HWM)", "Perkins & Alexander 2013"),
    c("hwa", "ck_hwa", "HWA", "Heatwave amplitude", "temperature", "health", "\u00b0C", "ET-SCI", "perkins2013measurement", "tmax,dates", "Peak temperature of the hottest heatwave (ET-SCI HWA)", "Perkins & Alexander 2013"),
    c("cwn", "ck_cwn", "CWN", "Cold-wave number", "temperature", "health", "events", "ET-SCI", "perkins2013measurement", "tmin,dates", "Annual count of cold-wave events (ET-SCI CWN; Tmin below the calendar-day 10th percentile for at least 3 days)", "Perkins & Alexander 2013"),
    c("cwf", "ck_cwf", "CWF", "Cold-wave frequency", "temperature", "health", "days", "ET-SCI", "perkins2013measurement", "tmin,dates", "Annual count of days contributing to cold waves (ET-SCI CWF)", "Perkins & Alexander 2013"),
    c("cwd", "ck_cwd", "CWD", "Cold-wave duration", "temperature", "health", "days", "ET-SCI", "perkins2013measurement", "tmin,dates", "Length of the longest cold wave in the period (ET-SCI CWD; not the ETCCDI consecutive wet days index, which is ck_wet_days)", "Perkins & Alexander 2013"),
    c("cwm", "ck_cwm", "CWM", "Cold-wave magnitude", "temperature", "health", "\u00b0C", "ET-SCI", "perkins2013measurement", "tmin,dates", "Mean temperature across cold-wave days (ET-SCI CWM)", "Perkins & Alexander 2013"),
    c("cwa", "ck_cwa", "CWA", "Cold-wave amplitude", "temperature", "health", "\u00b0C", "ET-SCI", "perkins2013measurement", "tmin,dates", "Lowest temperature of the coldest cold wave (ET-SCI CWA)", "Perkins & Alexander 2013"),
    c("ehf", "ck_ehf", "EHF", "Excess Heat Factor", "temperature", "health", "\u00b0C^2", "ET-SCI", "nairn2013defining", "tmax,tmin,dates", "Excess Heat Factor, the Bureau of Meteorology operational heatwave metric", "Nairn & Fawcett 2013"),
    c("heating_degree_days", "ck_heating_degree_days", NA_character_, "Heating degree days", "temperature", "energy", "degree-days", "energy", NA_character_, "tavg,dates", "Sum of (base - Tavg) for days below base temperature", "ASHRAE"),
    c("cooling_degree_days", "ck_cooling_degree_days", NA_character_, "Cooling degree days", "temperature", "energy", "degree-days", "energy", NA_character_, "tavg,dates", "Sum of (Tavg - base) for days above base temperature", "ASHRAE"),
    c("growing_degree_days", "ck_growing_degree_days", NA_character_, "Growing degree days", "temperature", "agriculture", "degree-days", "agroclimatic", NA_character_, "tavg,dates", "Sum of (Tavg - base) for days above base temperature", "McMaster & Wilhelm 1997"),
    c("total_precip", "ck_total_precip", "PRCPTOT", "Total wet-day precipitation", "precipitation", "water", "mm", "ETCCDI", "alexander2006global", "precip,dates", "Total precipitation by period", "ETCCDI"),
    c("dry_days", "ck_dry_days", "CDD", "Consecutive dry days", "precipitation", "water", "days", "ETCCDI", "alexander2006global", "precip,dates", "Maximum consecutive dry days (precip < threshold)", "ETCCDI"),
    c("wet_days", "ck_wet_days", "CWD", "Consecutive wet days", "precipitation", "water", "days", "ETCCDI", "alexander2006global", "precip,dates", "Maximum consecutive wet days (precip >= threshold)", "ETCCDI"),
    c("heavy_precip", "ck_heavy_precip", "R10mm/Rnnmm", "Heavy-precipitation days", "precipitation", "water", "days", "ETCCDI", "alexander2006global", "precip,dates", "Count of days with precipitation >= threshold", "ETCCDI"),
    c("very_heavy_precip", "ck_very_heavy_precip", "R20mm", "Very heavy precipitation days", "precipitation", "water", "days", "ETCCDI", "alexander2006global", "precip,dates", "Count of days with precipitation >= threshold", "ETCCDI"),
    c("max_1day_precip", "ck_max_1day_precip", "RX1day", "Max 1-day precipitation", "precipitation", "water", "mm", "ETCCDI", "alexander2006global", "precip,dates", "Maximum 1-day precipitation", "ETCCDI"),
    c("max_5day_precip", "ck_max_5day_precip", "RX5day", "Max 5-day precipitation", "precipitation", "water", "mm", "ETCCDI", "alexander2006global", "precip,dates", "Maximum 5-day precipitation total", "ETCCDI"),
    c("precip_intensity", "ck_precip_intensity", "SDII", "Simple daily intensity", "precipitation", "water", "mm/day", "ETCCDI", "alexander2006global", "precip,dates", "Mean precipitation on wet days (SDII)", "ETCCDI"),
    c("r95p", "ck_r95p", "R95p", "Very wet days total", "precipitation", "water", "mm", "ETCCDI", "zhang2011indices", "precip,dates", "Annual total precipitation on days above 95th percentile of 1961-1990 wet-day baseline (ETCCDI R95p)", "ETCCDI"),
    c("r99p", "ck_r99p", "R99p", "Extremely wet days total", "precipitation", "water", "mm", "ETCCDI", "zhang2011indices", "precip,dates", "Annual total precipitation on days above 99th percentile of 1961-1990 wet-day baseline (ETCCDI R99p)", "ETCCDI"),
    c("spi", "ck_spi", NA_character_, "Standardised Precipitation Index", "drought", "water", "dimensionless", "drought", "mckee1993relationship", "precip,dates", "Standardized Precipitation Index", "McKee et al. 1993"),
    c("spei", "ck_spei", NA_character_, "Standardised Precipitation-Evapotranspiration Index", "drought", "water", "dimensionless", "drought", "vicente2010multiscalar", "precip,pet,dates", "Standardized Precipitation-Evapotranspiration Index", "Vicente-Serrano et al. 2010"),
    c("pet", "ck_pet", NA_character_, "Potential evapotranspiration (Hargreaves)", "drought", "water", "mm", "drought", "hargreaves1985reference", "tmin,tmax,lat,dates", "Potential evapotranspiration (Hargreaves method)", "Hargreaves & Samani 1985"),
    c("pet_pm", "ck_pet_pm", NA_character_, "Reference evapotranspiration (FAO-56 Penman-Monteith)", "drought", "water", "mm", "drought", "allen1998crop", "tmin,tmax,lat,dates", "Reference evapotranspiration (FAO-56 Penman-Monteith method)", "Allen et al. 1998"),
    c("huglin", "ck_huglin", NA_character_, "Huglin heliothermal index", "agroclimatic", "agriculture", "degree-days", "agroclimatic", "huglin1978nouveau", "tmin,tmax,dates,lat", "Huglin heliothermal index for viticulture", "Huglin 1978"),
    c("winkler", "ck_winkler", NA_character_, "Winkler index", "agroclimatic", "agriculture", "degree-days", "agroclimatic", "winkler1974general", "tavg,dates", "Winkler index (growing degree days for wine regions)", "Amerine & Winkler 1944"),
    c("branas", "ck_branas", NA_character_, "Branas hydrothermal index", "agroclimatic", "agriculture", "mm\u00b7\u00b0C", "agroclimatic", NA_character_, "precip,tavg,dates", "Branas hydrothermal index", "Branas et al. 1946"),
    c("first_frost", "ck_first_frost", NA_character_, "First autumn frost date", "agroclimatic", "agriculture", "day of year", "agroclimatic", NA_character_, "tmin,dates", "Date of first autumn frost (Tmin < 0\u00b0C)", ""),
    c("last_frost", "ck_last_frost", NA_character_, "Last spring frost date", "agroclimatic", "agriculture", "day of year", "agroclimatic", NA_character_, "tmin,dates", "Date of last spring frost (Tmin < 0\u00b0C)", ""),
    c("heat_index", "ck_heat_index", NA_character_, "Heat index (NWS apparent temperature)", "comfort", "health", "\u00b0C", "comfort", "rothfusz1990heat", "tavg,humidity", "Heat index (apparent temperature)", "Rothfusz 1990"),
    c("humidex", "ck_humidex", NA_character_, "Canadian humidex", "comfort", "health", "unitless", "comfort", "masterton1979humidex", "tavg,dewpoint", "Canadian humidex", "Masterson & Richardson 1979"),
    c("wind_chill", "ck_wind_chill", NA_character_, "Wind chill", "comfort", "health", "\u00b0C", "comfort", "osczevski2005new", "tavg,wind_speed", "Wind chill temperature", "Environment Canada / NWS"),
    c("fire_danger", "ck_fire_danger", NA_character_, "Fire danger proxy", "comfort", NA_character_, "unitless", "comfort", NA_character_, "tavg,humidity,wind_speed,precip", "Simplified fire danger proxy (not FWI)", "")
  )
  m <- do.call(rbind, rows)
  colnames(m) <- c("index", "ck_function", "code", "name", "category",
                   "sector", "unit", "standard", "citation_key", "args",
                   "description", "reference")
  m
}

#' The index table as a data frame
#' @noRd
.ck_index_df <- function() {
  m <- .ck_index_table()
  as.data.frame(m, stringsAsFactors = FALSE)
}

#' Required ck_compute() data columns for one index
#' @noRd
.ck_index_args <- function(index) {
  m <- .ck_index_table()
  i <- match(index, m[, "index"])
  if (is.na(i)) {
    return(NULL)
  }
  strsplit(m[i, "args"], ",", fixed = TRUE)[[1]]
}

#' Unambiguous aliases accepted by ck_compute() and ck_metadata()
#'
#' ETCCDI and ET-SCI both use the abbreviation CWD, for consecutive wet days
#' and for cold-wave duration respectively, and CDD is ETCCDI consecutive dry
#' days but climpact cooling degree days. `climatekit` resolves `"cwd"` to the
#' ET-SCI cold-wave duration and `"cdd"` to the ETCCDI consecutive dry days
#' index, which are the meanings the corresponding `ck_*` functions carry.
#' These aliases let callers say which one they mean without relying on that.
#'
#' @return A named character vector mapping alias to canonical index name.
#' @noRd
.ck_index_aliases <- function() {
  c(
    cdd                  = "dry_days",
    consecutive_dry_days = "dry_days",
    consecutive_wet_days = "wet_days",
    cold_wave_duration   = "cwd",
    cold_wave_number     = "cwn",
    cold_wave_frequency  = "cwf",
    cold_wave_magnitude  = "cwm",
    cold_wave_amplitude  = "cwa"
  )
}

#' Resolve an index name or alias to a canonical index name
#' @noRd
.ck_resolve_index <- function(index) {
  aliases <- .ck_index_aliases()
  if (index %in% names(aliases)) aliases[[index]] else index
}
