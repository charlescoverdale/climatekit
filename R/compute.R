#' Compute a Climate Index by Name
#'
#' A generic dispatcher that calls the appropriate `ck_*` function based on
#' a string index name. Useful for programmatic workflows where the index
#' is selected at runtime.
#'
#' @param data A named list or data frame containing the required input
#'   vectors. Column names should match function argument names (e.g.
#'   `tmin`, `tmax`, `precip`, `dates`).
#' @param index Character. Name of the index to compute (e.g.
#'   `"frost_days"`). Use [ck_available()] to see valid names.
#' @param ... Additional arguments passed to the underlying function (e.g.
#'   `period`, `threshold`, `base`).
#'
#' @return A data frame as returned by the underlying `ck_*` function.
#'
#' @export
#' @examples
#' d <- data.frame(
#'   dates = as.Date("2024-01-01") + 0:9,
#'   tmin = c(-2, 3, -1, 5, -3, 0, 2, -4, 1, -1)
#' )
#' ck_compute(d, "frost_days")
ck_compute <- function(data, index, ...) {
  if (!is.character(index) || length(index) != 1) {
    cli::cli_abort("{.arg index} must be a single character string.")
  }

  # Map index names to functions and required columns
  dispatch <- list(
    frost_days       = list(fn = ck_frost_days, args = c("tmin", "dates")),
    ice_days         = list(fn = ck_ice_days, args = c("tmax", "dates")),
    summer_days      = list(fn = ck_summer_days, args = c("tmax", "dates")),
    tropical_nights  = list(fn = ck_tropical_nights, args = c("tmin", "dates")),
    growing_season   = list(fn = ck_growing_season, args = c("tavg", "dates")),
    txx              = list(fn = ck_txx, args = c("tmax", "dates")),
    tnx              = list(fn = ck_tnx, args = c("tmin", "dates")),
    txn              = list(fn = ck_txn, args = c("tmax", "dates")),
    tnn              = list(fn = ck_tnn, args = c("tmin", "dates")),
    tx10p            = list(fn = ck_tx10p, args = c("tmax", "dates")),
    tn10p            = list(fn = ck_tn10p, args = c("tmin", "dates")),
    tx90p            = list(fn = ck_tx90p, args = c("tmax", "dates")),
    tn90p            = list(fn = ck_tn90p, args = c("tmin", "dates")),
    r95p             = list(fn = ck_r95p, args = c("precip", "dates")),
    r99p             = list(fn = ck_r99p, args = c("precip", "dates")),
    wsdi             = list(fn = ck_wsdi, args = c("tmax", "dates")),
    csdi             = list(fn = ck_csdi, args = c("tmin", "dates")),
    hwn              = list(fn = ck_hwn, args = c("tmax", "dates")),
    hwf              = list(fn = ck_hwf, args = c("tmax", "dates")),
    hwd              = list(fn = ck_hwd, args = c("tmax", "dates")),
    hwm              = list(fn = ck_hwm, args = c("tmax", "dates")),
    hwa              = list(fn = ck_hwa, args = c("tmax", "dates")),
    cwn              = list(fn = ck_cwn, args = c("tmin", "dates")),
    cwf              = list(fn = ck_cwf, args = c("tmin", "dates")),
    cwd              = list(fn = ck_cwd, args = c("tmin", "dates")),
    cwm              = list(fn = ck_cwm, args = c("tmin", "dates")),
    cwa              = list(fn = ck_cwa, args = c("tmin", "dates")),
    heating_degree_days = list(fn = ck_heating_degree_days, args = c("tavg", "dates")),
    cooling_degree_days = list(fn = ck_cooling_degree_days, args = c("tavg", "dates")),
    growing_degree_days = list(fn = ck_growing_degree_days, args = c("tavg", "dates")),
    diurnal_range    = list(fn = ck_diurnal_range, args = c("tmin", "tmax", "dates")),
    warm_spell       = list(fn = ck_warm_spell, args = c("tmax", "dates")),
    dry_days         = list(fn = ck_dry_days, args = c("precip", "dates")),
    wet_days         = list(fn = ck_wet_days, args = c("precip", "dates")),
    total_precip     = list(fn = ck_total_precip, args = c("precip", "dates")),
    heavy_precip     = list(fn = ck_heavy_precip, args = c("precip", "dates")),
    very_heavy_precip = list(fn = ck_very_heavy_precip, args = c("precip", "dates")),
    max_1day_precip  = list(fn = ck_max_1day_precip, args = c("precip", "dates")),
    max_5day_precip  = list(fn = ck_max_5day_precip, args = c("precip", "dates")),
    precip_intensity = list(fn = ck_precip_intensity, args = c("precip", "dates")),
    spi              = list(fn = ck_spi, args = c("precip", "dates")),
    spei             = list(fn = ck_spei, args = c("precip", "pet", "dates")),
    pet              = list(fn = ck_pet, args = c("tmin", "tmax", "lat", "dates")),
    huglin           = list(fn = ck_huglin, args = c("tmin", "tmax", "dates", "lat")),
    winkler          = list(fn = ck_winkler, args = c("tavg", "dates")),
    branas           = list(fn = ck_branas, args = c("precip", "tavg", "dates")),
    first_frost      = list(fn = ck_first_frost, args = c("tmin", "dates")),
    last_frost       = list(fn = ck_last_frost, args = c("tmin", "dates")),
    wind_chill       = list(fn = ck_wind_chill, args = c("tavg", "wind_speed")),
    heat_index       = list(fn = ck_heat_index, args = c("tavg", "humidity")),
    humidex          = list(fn = ck_humidex, args = c("tavg", "dewpoint")),
    fire_danger      = list(fn = ck_fire_danger,
                            args = c("tavg", "humidity", "wind_speed", "precip"))
  )

  if (!index %in% names(dispatch)) {
    cli::cli_abort(
      c("Unknown index {.val {index}}.",
        "i" = "Run {.fn ck_available} to see valid index names.")
    )
  }

  entry <- dispatch[[index]]
  required <- entry$args

  # Extract columns from data
  if (is.data.frame(data)) {
    data <- as.list(data)
  }

  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Missing required column{?s}: {.field {missing_cols}}."
    )
  }

  call_args <- data[required]
  call_args <- c(call_args, list(...))
  do.call(entry$fn, call_args)
}
