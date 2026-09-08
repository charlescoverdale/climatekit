## Submission

This is a patch release from 0.2.0 to 0.2.1. It fixes two defects, adds
one deprecation, and drops one dependency. No new exported functions.

## R CMD check results

0 errors | 0 warnings | 1 note

The note is `checking for future file timestamps ... unable to verify
current time`, which is the local check machine failing to reach
worldclockapi.com rather than anything in the package.

## Test environments

* local macOS (aarch64-apple-darwin), R 4.5.2
* win-builder (devel and release)

## What is new in 0.2.1

* Bug fix: `ck_available()` and `ck_metadata()` were driven by an index
  registry that had not been updated when 0.2.0 added twelve functions,
  so `ck_available()` reported 42 of 54 indices and
  `ck_metadata("ehf")` failed with "Unknown index". All four views of
  the index set are now derived from one internal table.
  `ck_catalogue()` and `ck_etccdi_27()` output is unchanged.
* Bug fix: `ck_warm_spell()` used R's default Hyndman-Fan type 7
  quantile estimator while every other percentile in the package uses
  type 8, the 'ETCCDI' convention. It now uses type 8. The estimators
  differ by order 1/n, so counts are typically unchanged on multi-year
  daily series.
* Deprecation: `clear_cache()` warns and returns `FALSE` invisibly. The
  package performs no I/O and never wrote a cache. It is retained so
  existing scripts keep running and is scheduled for removal in 0.4.0.
* `tools` has been removed from Imports; it was used only by
  `clear_cache()`.
* Documentation: `ck_dry_days()` and `ck_wet_days()` now state their
  'ETCCDI' codes and the two acronym collisions in the literature (CWD
  is both consecutive wet days and cold-wave duration; CDD is both
  consecutive dry days and cooling degree days). `ck_compute()` also
  accepts unambiguous aliases.

## Notes

* No external API calls. All computation is local.
* The CRAN URL check may flag `https://www.bom.gov.au/climate/data/`
  in README.md as 403 Forbidden. The Australian Bureau of
  Meteorology server blocks programmatic User-Agents (including
  R / libcurl) but the page is reachable from any browser. The
  URL was present in the v0.1.0 and v0.2.0 releases accepted to CRAN.
