## Submission

This is a patch release. The previous CRAN version is 0.2.0; 0.2.1 was
prepared but never submitted, so this release carries both sets of
changes and NEWS.md documents them separately.

0.2.2 corrects three defects that produced wrong numbers in default code
paths, found by auditing each index against its published definition.

## R CMD check results

0 errors | 0 warnings | 1 note

The note is `checking for future file timestamps ... unable to verify
current time`, which is the local check machine failing to reach
worldclockapi.com rather than anything in the package.

## Test environments

* local macOS (aarch64-apple-darwin), R 4.5.2
* win-builder (devel and release)

## What is new in 0.2.2

* `ck_spei()` with its default log-logistic distribution did not return
  a standardised index (mean 1.61, standard deviation 3.10 on an
  80-year series, where standard normal is required by construction).
  Two errors in the L-moment fit are corrected against the appendix of
  Vicente-Serrano et al. (2010). Output is now mean 0.00, standard
  deviation 0.99. `ck_spi()` and the GEV option were unaffected.
* `ck_ehf()` had the two Excess Heat Factor terms transposed relative to
  Nairn and Fawcett (2013), so the sign of the index was driven by
  acclimatisation rather than by heat. Days below the reference 95th
  percentile were being counted as heatwave days.
* Extraterrestrial radiation chose the polar-day and polar-night cases
  by hemisphere rather than by the sunset hour angle, returning negative
  radiation at 80 degrees N in December. The hour-angle argument is now
  clamped to `[-1, 1]`. This also removes `acos()` "NaNs produced"
  warnings. It affected `ck_pet()`, `ck_pet_pm()` and any `ck_spei()`
  built on them beyond about 66.5 degrees latitude.
* `ck_max_5day_precip()` returned a short period's sum as though it were
  a five-day maximum; periods shorter than five days now return `NA`.
* New input validation. Negative precipitation and a `tmin` above `tmax`
  are now errors, and temperatures outside -100 to 70 degrees C warn.
  Missing-data sentinels such as -999 were previously accepted and
  counted as observations.

## What was in the unreleased 0.2.1

* `ck_available()` and `ck_metadata()` did not know about the twelve
  indices added in 0.2.0. All views of the index set are now derived
  from one internal table.
* `ck_warm_spell()` used quantile type 7 where the rest of the package
  uses type 8.
* `clear_cache()` is deprecated; `tools` is no longer a dependency.

## Notes

* No external API calls. All computation is local.
* The CRAN URL check may flag `https://www.bom.gov.au/climate/data/`
  in README.md as 403 Forbidden. The Australian Bureau of Meteorology
  server blocks programmatic User-Agents (including R / libcurl) but
  the page is reachable from any browser. The URL was present in the
  v0.1.0 and v0.2.0 releases accepted to CRAN.
