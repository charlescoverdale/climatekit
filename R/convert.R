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

#' Clear Cache
#'
#' Removes any cached reference data stored by `climatekit`.
#'
#' @return Invisibly returns `TRUE` if cache was cleared, `FALSE` if no cache
#'   existed.
#'
#' @export
#' @examples
#' clear_cache()
clear_cache <- function() {
  cache_dir <- tools::R_user_dir("climatekit", "cache")
  if (dir.exists(cache_dir)) {
    unlink(cache_dir, recursive = TRUE)
    cli::cli_alert_success("Cache cleared.")
    invisible(TRUE)
  } else {
    cli::cli_alert_info("No cache to clear.")
    invisible(FALSE)
  }
}
