# climatekit 0.2.0

## ETCCDI canonical 27 coverage

* Added 13 new functions completing the full canonical 'ETCCDI' 27 set:
  `ck_txx()`, `ck_tnx()`, `ck_txn()`, `ck_tnn()` (extreme values);
  `ck_tx10p()`, `ck_tn10p()`, `ck_tx90p()`, `ck_tn90p()`,
  `ck_r95p()`, `ck_r99p()` (calendar-day percentile base, default
  reference period 1961-1990); `ck_wsdi()`, `ck_csdi()` (proper
  calendar-day spell duration). `ck_growing_season()` was already
  ETCCDI-compliant.
* `ck_warm_spell()` is retained as a quick series-quantile
  approximation; its documentation now points to `ck_wsdi()` for the
  canonical ETCCDI definition.

## ET-SCI heatwave and cold-wave families

* Added 10 new functions implementing the 'ET-SCI' heatwave and
  cold-wave families on a calendar-day percentile base. Heatwave
  family: `ck_hwn()`, `ck_hwf()`, `ck_hwd()`, `ck_hwm()`, `ck_hwa()`
  (number, frequency, duration, magnitude, amplitude). Cold-wave
  duals: `ck_cwn()`, `ck_cwf()`, `ck_cwd()`, `ck_cwm()`, `ck_cwa()`.
* The 'CWD' acronym is used by both 'ETCCDI' (Consecutive Wet
  Days) and 'ET-SCI' (Cold Wave Duration). `ck_cwd()` here is the
  ET-SCI cold-wave version; `ck_wet_days()` is the ETCCDI
  precipitation index. Both function-level documentation pages
  cross-reference the other.

## Discovery surfaces

* `ck_etccdi_27()` returns the canonical 27 'ETCCDI' indices as a
  data frame with `code`, `name`, `variable`, `unit`, `definition`,
  `ck_function`, and `status` columns. Use it to audit coverage or
  to locate the function for a given short code.
* `ck_catalogue()` returns the full implementation catalogue (51
  rows). `ck_browse(sector, standard, search)` filters by sector
  ('agriculture', 'health', 'water', 'energy'), standard ('ETCCDI',
  'ET-SCI', 'agroclimatic', etc.), or free-text search.

## Gridded support

* `ck_apply_grid(x, fun, dates, ...)` applies any `ck_*` function
  over the cells of a 'terra' 'SpatRaster' and returns a SpatRaster
  with one layer per output period.
* `ck_from_netcdf(path, var)` is a thin convenience wrapper around
  `terra::rast()` for 'netCDF' inputs with file-existence and
  argument validation.
* 'terra' and 'ncdf4' are in 'Suggests'; loading climatekit alone
  does not pull them in.

## Documentation

* `inst/CITATION` provides 'bibentry' records for the package and
  for Alexander et al. (2006) and Zhang et al. (2011).
* 'CITATION.cff' at the repository root supports GitHub's citation
  widget.
* New vignette `climdex-migration` documents the function-name
  crosswalk from 'climdex.pcic' to climatekit, plus interface-shift
  notes (numeric-vector inputs, tidy data-frame outputs, default
  reference period).

## Documented limitations

* The percentile-based indices use the standard ±2-day calendar
  window but do not implement the Zhang et al. (2005) in-base
  bootstrap. Years inside the reference period therefore have a
  small self-inclusion bias. This is documented in each function's
  documentation page.

# climatekit 0.1.0

* Initial release.
* 10 temperature indices: frost days, ice days, summer days, tropical nights,
  growing season length, heating/cooling/growing degree days, diurnal
  temperature range, warm spell duration.
* 8 precipitation indices: consecutive dry/wet days, total precipitation,
  heavy/very heavy precipitation days, max 1-day and 5-day precipitation,
  precipitation intensity (SDII).
* 3 drought indices: Standardized Precipitation Index (SPI), Standardized
  Precipitation-Evapotranspiration Index (SPEI), potential evapotranspiration
  (Hargreaves method).
* 5 agroclimatic indices: Huglin, Winkler, Branas, first/last frost dates.
* 4 comfort indices: wind chill, heat index, humidex, fire danger index.
* Generic dispatcher `ck_compute()` for programmatic index selection.
* `ck_available()` and `ck_metadata()` for index discovery.
* `ck_convert_temp()` for temperature unit conversion.
