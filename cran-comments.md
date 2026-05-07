## Submission

This is a minor version update from 0.1.0 to 0.2.0.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local macOS (aarch64-apple-darwin), R 4.5.2
* win-builder (devel and release)
* GitHub Actions (ubuntu-latest, R release)

## What is new in 0.2.0

* 27 new exported functions, taking total user-facing exports from
  35 to 64.
* Full canonical 'ETCCDI' 27 coverage: extreme values, calendar-day
  percentile indices, and proper spell-duration indices.
* 'ET-SCI' heatwave and cold-wave family (HWN / HWF / HWD / HWM /
  HWA, plus cold-wave duals) and the Nairn and Fawcett (2013)
  Excess Heat Factor `ck_ehf()`.
* `ck_etccdi_27()` audit table and `ck_catalogue()` /
  `ck_browse()` discovery surfaces.
* Zhang (2005) leave-one-out in-base bootstrap available on the
  percentile-day indices via `bootstrap = TRUE`.
* `ck_pet_pm()` adds FAO-56 Penman-Monteith reference
  evapotranspiration alongside the existing Hargreaves estimator.
* `ck_spi()` / `ck_spei()` gain a `distribution` argument (gamma /
  Pearson III for SPI; log-logistic / GEV for SPEI).
* Bug fixes: `ck_total_precip()` now applies the canonical 1 mm
  wet-day filter; `ck_first_frost()` / `ck_last_frost()` /
  `ck_branas()` gain a `lat` argument with correct Southern
  Hemisphere defaults; `ck_precip_intensity()` returns NA on
  all-NA periods.
* Optional gridded support via `ck_apply_grid()` and
  `ck_from_netcdf()`. 'terra' and 'ncdf4' are in 'Suggests' only.
* `inst/CITATION` and a `climdex-migration` vignette.

## Notes

* No external API calls. All computation is local.
* DOIs in the Description field point to the canonical Alexander
  et al. (2006) and Zhang et al. (2011) ETCCDI references plus
  Zhang et al. (2005) for the in-base bootstrap.
* The CRAN URL check may flag `https://www.bom.gov.au/climate/data/`
  in README.md as 403 Forbidden. The Australian Bureau of
  Meteorology server blocks programmatic User-Agents (including
  R / libcurl) but the page is reachable from any browser. The
  URL was present in the v0.1.0 release accepted to CRAN. Manually
  verified 2026-05-07 with a `Mozilla/5.0` UA: 200.
