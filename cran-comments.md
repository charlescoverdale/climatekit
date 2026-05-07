## Submission

This is a minor version update from 0.1.0 to 0.2.0.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local macOS (aarch64-apple-darwin), R 4.5.2
* win-builder (devel and release)
* GitHub Actions (ubuntu-latest, R release)

## What is new in 0.2.0

* 25 new exported functions, taking total user-facing exports from
  35 to 60.
* Full canonical 'ETCCDI' 27 coverage: extreme values, calendar-day
  percentile indices, and proper spell-duration indices.
* 'ET-SCI' heatwave and cold-wave family (HWN / HWF / HWD / HWM /
  HWA, plus cold-wave duals).
* `ck_etccdi_27()` audit table and `ck_catalogue()` /
  `ck_browse()` discovery surfaces.
* Optional gridded support via `ck_apply_grid()` and
  `ck_from_netcdf()`. 'terra' and 'ncdf4' are in 'Suggests' only.
* `inst/CITATION` and a `climdex-migration` vignette.

## Notes

* No external API calls. All computation is local.
* DOIs in the Description field point to the canonical Alexander
  et al. (2006) and Zhang et al. (2011) ETCCDI references.
