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
#'
#'   Where ETCCDI and ET-SCI share an acronym, `climatekit` follows the
#'   meaning its own `ck_*` function carries: `"cwd"` is ET-SCI cold-wave
#'   duration ([ck_cwd()]), not ETCCDI consecutive wet days, and `"cdd"`
#'   is ETCCDI consecutive dry days ([ck_dry_days()]), not cooling degree
#'   days. The unambiguous aliases `"consecutive_wet_days"`,
#'   `"consecutive_dry_days"` and `"cold_wave_duration"` are also accepted.
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

  index <- .ck_resolve_index(index)
  tbl <- .ck_index_table()
  pos <- match(index, tbl[, "index"])
  if (is.na(pos)) {
    cli::cli_abort(
      c("Unknown index {.val {index}}.",
        "i" = "Run {.fn ck_available} to see valid index names.")
    )
  }

  fn <- get(tbl[pos, "ck_function"], envir = asNamespace("climatekit"))
  required <- .ck_index_args(index)

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
  do.call(fn, call_args)
}
