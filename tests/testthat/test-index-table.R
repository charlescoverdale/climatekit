# Regression guards for the single index table introduced in v0.2.1.
# Before that release ck_available()/ck_metadata(), ck_catalogue() and the
# ck_compute() dispatch were three hand-maintained lists that had drifted.

test_that("every catalogued index is discoverable and computable", {
  cat_idx <- sub("^ck_", "", ck_catalogue()$ck_function)

  # ck_available() lists all of them
  expect_setequal(ck_available()$index, cat_idx)

  # ck_metadata() resolves all of them, with the documented shape
  for (i in cat_idx) {
    m <- ck_metadata(i)
    expect_type(m, "list")
    expect_named(m, c("index", "category", "unit", "description", "reference"))
    expect_identical(m$index, i)
    expect_true(nzchar(m$description))
  }

  # ck_compute() dispatches all of them
  for (i in cat_idx) {
    expect_false(is.null(climatekit:::.ck_index_args(i)),
                 info = paste("no ck_compute args for", i))
  }
})

test_that("the v0.2.0 additions reach the discovery layer", {
  added <- c("hwn", "hwf", "hwd", "hwm", "hwa",
             "cwn", "cwf", "cwd", "cwm", "cwa", "ehf", "pet_pm")
  expect_true(all(added %in% ck_available()$index))
  expect_identical(ck_metadata("ehf")$unit, "°C^2")
  expect_identical(ck_metadata("hwn")$category, "temperature")
})

test_that("every index table function is an actual export", {
  tbl <- climatekit:::.ck_index_table()
  expect_true(all(tbl[, "ck_function"] %in% getNamespaceExports("climatekit")))
})

test_that("index table rows are complete and unique", {
  tbl <- climatekit:::.ck_index_table()
  expect_false(anyDuplicated(tbl[, "index"]) > 0)
  expect_false(anyDuplicated(tbl[, "ck_function"]) > 0)
  for (col in c("index", "ck_function", "name", "category", "unit",
                "standard", "args", "description")) {
    expect_false(anyNA(tbl[, col]), info = col)
    expect_true(all(nzchar(tbl[, col])), info = col)
  }
})

test_that("ck_compute args name real arguments of the target function", {
  tbl <- climatekit:::.ck_index_table()
  for (i in seq_len(nrow(tbl))) {
    fn <- get(tbl[i, "ck_function"], envir = asNamespace("climatekit"))
    expect_true(all(climatekit:::.ck_index_args(tbl[i, "index"]) %in% names(formals(fn))),
                info = tbl[i, "index"])
  }
})

test_that("acronym-collision aliases resolve to the right index", {
  # cwd keeps its ET-SCI meaning; wet days need the explicit alias
  expect_identical(climatekit:::.ck_resolve_index("cwd"), "cwd")
  expect_identical(climatekit:::.ck_resolve_index("consecutive_wet_days"), "wet_days")
  expect_identical(climatekit:::.ck_resolve_index("consecutive_dry_days"), "dry_days")
  expect_identical(climatekit:::.ck_resolve_index("cdd"), "dry_days")
  expect_identical(climatekit:::.ck_resolve_index("cold_wave_duration"), "cwd")
  expect_identical(climatekit:::.ck_resolve_index("frost_days"), "frost_days")

  expect_identical(ck_metadata("cdd")$index, "dry_days")
  expect_identical(ck_metadata("cold_wave_duration")$index, "cwd")
})

test_that("ck_compute honours the aliases", {
  d <- data.frame(
    dates = as.Date("2024-01-01") + 0:9,
    precip = c(0, 0, 5, 0, 0, 0, 2, 3, 4, 0)
  )
  expect_equal(ck_compute(d, "cdd"), ck_compute(d, "dry_days"))
  expect_equal(ck_compute(d, "consecutive_dry_days"), ck_compute(d, "dry_days"))
  expect_equal(ck_compute(d, "consecutive_wet_days"), ck_compute(d, "wet_days"))
})

test_that("every percentile in the package uses the type 8 estimator", {
  # ETCCDI convention; ck_warm_spell used the default type 7 before v0.2.1
  r_files <- list.files("../../R", pattern = "[.]R$", full.names = TRUE)
  if (!length(r_files)) skip("package sources not available in this check layout")
  src <- unlist(lapply(r_files, readLines))
  calls <- grep("stats::quantile", src)
  expect_gt(length(calls), 0)
  # the type argument may sit on the call line or the one after it
  windows <- vapply(calls, function(i) paste(src[i:min(i + 2L, length(src))],
                                             collapse = " "), character(1))
  expect_true(all(grepl("type = 8L", windows, fixed = TRUE)))
})

test_that("clear_cache is deprecated but still returns invisibly", {
  expect_warning(res <- clear_cache(), "deprecated")
  expect_false(res)
})
