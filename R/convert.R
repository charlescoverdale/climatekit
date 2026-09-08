#' Convert Temperature Units
#'
#' Convert between Celsius, Fahrenheit, and Kelvin.
#'
#' @param x Numeric vector of temperatures.
#' @param from Character. Source unit: `"C"`, `"F"`, or `"K"`.
#' @param to Character. Target unit: `"C"`, `"F"`, or `"K"`.
#'
#' @return Numeric vector of converted temperatures.
#'
#' @export
#' @examples
#' ck_convert_temp(c(0, 100), from = "C", to = "F")
#' ck_convert_temp(32, from = "F", to = "C")
ck_convert_temp <- function(x, from, to) {
  validate_numeric(x, "x")
  from <- match.arg(from, c("C", "F", "K"))
  to <- match.arg(to, c("C", "F", "K"))

  if (from == to) return(x)

  # Convert to Celsius first
  celsius <- switch(from,
    "C" = x,
    "F" = (x - 32) * 5 / 9,
    "K" = x - 273.15
  )

  # Convert from Celsius to target
  switch(to,
    "C" = celsius,
    "F" = celsius * 9 / 5 + 32,
    "K" = celsius + 273.15
  )
}

#' Clear Cache (Deprecated)
#'
#' Deprecated in v0.2.1 and scheduled for removal in v0.4.0. `climatekit`
#' performs no I/O and never writes a cache, so this function has never had
#' anything to clear. It remains only so that existing scripts keep running.
#'
#' If you are caching downloaded weather data, clear it with the tools of
#' whichever data package fetched it (for example `readnoaa`), not here.
#'
#' @return Invisibly returns `FALSE`. Prior to v0.2.1 this returned `TRUE`
#'   when a directory set through `options(climatekit.cache_dir = ...)`
#'   contained files, which only ever happened if something outside
#'   `climatekit` had written there.
#'
#' @export
#' @examples
#' \donttest{
#' # Deprecated: this warns and does nothing.
#' suppressWarnings(clear_cache())
#' }
clear_cache <- function() {
  cli::cli_warn(c(
    "{.fn clear_cache} is deprecated and will be removed in climatekit 0.4.0.",
    "i" = "{.pkg climatekit} is pure computation and never writes a cache.",
    "i" = "To clear cached weather data, use the data package that fetched it."
  ))
  invisible(FALSE)
}
