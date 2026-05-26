# Changelog

## climatekit 0.2.0

CRAN release: 2026-05-09

### Bug fixes

- [`ck_total_precip()`](https://charlescoverdale.github.io/climatekit/reference/ck_total_precip.md)
  now applies the canonical ‘ETCCDI’ ‘PRCPTOT’ wet-day filter
  (precipitation \>= 1 mm) by default. Pass `wet_day_threshold = 0` to
  recover the previous raw-sum behaviour.
- [`ck_first_frost()`](https://charlescoverdale.github.io/climatekit/reference/ck_first_frost.md)
  and
  [`ck_last_frost()`](https://charlescoverdale.github.io/climatekit/reference/ck_last_frost.md)
  gain a `lat` argument and use hemisphere-appropriate cutoffs. Southern
  Hemisphere users no longer get silent NA results.
- [`ck_branas()`](https://charlescoverdale.github.io/climatekit/reference/ck_branas.md)
  gains a `lat` argument; Southern Hemisphere growing season is now
  October-February of the following year.
- [`ck_precip_intensity()`](https://charlescoverdale.github.io/climatekit/reference/ck_precip_intensity.md)
  (SDII) returns NA for periods with no valid observations rather than
  NaN.

### In-base bootstrap (Zhang 2005)

- [`ck_tx10p()`](https://charlescoverdale.github.io/climatekit/reference/ck_tx10p.md),
  [`ck_tn10p()`](https://charlescoverdale.github.io/climatekit/reference/ck_tn10p.md),
  [`ck_tx90p()`](https://charlescoverdale.github.io/climatekit/reference/ck_tx90p.md),
  [`ck_tn90p()`](https://charlescoverdale.github.io/climatekit/reference/ck_tn90p.md)
  gain a `bootstrap = FALSE` argument. When `TRUE`, the leave-one-out
  resampling of Zhang et al. (2005) is applied to remove self-inclusion
  bias for analysis years inside the reference period. This is the
  canonical ‘climdex.pcic’ / ‘climpact’ behaviour and is required for
  climate-change attribution work spanning the base period.

### ET-SCI heatwave family extensions

- [`ck_hwm()`](https://charlescoverdale.github.io/climatekit/reference/ck_hwm.md),
  [`ck_hwa()`](https://charlescoverdale.github.io/climatekit/reference/ck_hwa.md),
  [`ck_cwm()`](https://charlescoverdale.github.io/climatekit/reference/ck_cwm.md),
  [`ck_cwa()`](https://charlescoverdale.github.io/climatekit/reference/ck_cwa.md)
  gain a `mode = c( "excess", "absolute")` argument. `"excess"`
  (default) preserves the existing ‘ET-SCI’ / ‘climpact’ convention.
  `"absolute"` returns mean / peak raw temperature on event days,
  matching Perkins-Alexander (2013).
- New function
  [`ck_ehf()`](https://charlescoverdale.github.io/climatekit/reference/ck_ehf.md)
  implements the Excess Heat Factor of Nairn and Fawcett (2013), the
  Australian Bureau of Meteorology operational heatwave metric. Three
  annual statistics are exposed via
  `stat = c("max", "n_positive", "sum_positive")`.

### SPI / SPEI distribution choice

- [`ck_spi()`](https://charlescoverdale.github.io/climatekit/reference/ck_spi.md)
  gains `distribution = c("gamma", "pearsonIII")`. Pearson III is
  preferred in arid regions where the wet-day distribution is highly
  skewed (Stagge et al. 2015).
- [`ck_spei()`](https://charlescoverdale.github.io/climatekit/reference/ck_spei.md)
  gains `distribution = c("log-logistic", "gev")`. GEV is fitted via
  Hosking (1985) L-moments.

### FAO-56 Penman-Monteith reference ET

- New function
  [`ck_pet_pm()`](https://charlescoverdale.github.io/climatekit/reference/ck_pet_pm.md)
  implements the FAO-56 Penman-Monteith reference evapotranspiration
  (Allen et al. 1998), the international standard. Optional inputs
  include relative humidity, wind speed, incoming solar radiation, and
  elevation; FAO-56 fallbacks are used where these are unavailable.
  [`ck_pet()`](https://charlescoverdale.github.io/climatekit/reference/ck_pet.md)
  remains as the simpler temperature-only Hargreaves estimator.

### ETCCDI canonical 27 coverage

- Added 13 new functions completing the full canonical ‘ETCCDI’ 27 set:
  [`ck_txx()`](https://charlescoverdale.github.io/climatekit/reference/ck_txx.md),
  [`ck_tnx()`](https://charlescoverdale.github.io/climatekit/reference/ck_tnx.md),
  [`ck_txn()`](https://charlescoverdale.github.io/climatekit/reference/ck_txn.md),
  [`ck_tnn()`](https://charlescoverdale.github.io/climatekit/reference/ck_tnn.md)
  (extreme values);
  [`ck_tx10p()`](https://charlescoverdale.github.io/climatekit/reference/ck_tx10p.md),
  [`ck_tn10p()`](https://charlescoverdale.github.io/climatekit/reference/ck_tn10p.md),
  [`ck_tx90p()`](https://charlescoverdale.github.io/climatekit/reference/ck_tx90p.md),
  [`ck_tn90p()`](https://charlescoverdale.github.io/climatekit/reference/ck_tn90p.md),
  [`ck_r95p()`](https://charlescoverdale.github.io/climatekit/reference/ck_r95p.md),
  [`ck_r99p()`](https://charlescoverdale.github.io/climatekit/reference/ck_r99p.md)
  (calendar-day percentile base, default reference period 1961-1990);
  [`ck_wsdi()`](https://charlescoverdale.github.io/climatekit/reference/ck_wsdi.md),
  [`ck_csdi()`](https://charlescoverdale.github.io/climatekit/reference/ck_csdi.md)
  (proper calendar-day spell duration).
  [`ck_growing_season()`](https://charlescoverdale.github.io/climatekit/reference/ck_growing_season.md)
  was already ETCCDI-compliant.
- [`ck_warm_spell()`](https://charlescoverdale.github.io/climatekit/reference/ck_warm_spell.md)
  is retained as a quick series-quantile approximation; its
  documentation now points to
  [`ck_wsdi()`](https://charlescoverdale.github.io/climatekit/reference/ck_wsdi.md)
  for the canonical ETCCDI definition.

### ET-SCI heatwave and cold-wave families

- Added 10 new functions implementing the ‘ET-SCI’ heatwave and
  cold-wave families on a calendar-day percentile base. Heatwave family:
  [`ck_hwn()`](https://charlescoverdale.github.io/climatekit/reference/ck_hwn.md),
  [`ck_hwf()`](https://charlescoverdale.github.io/climatekit/reference/ck_hwf.md),
  [`ck_hwd()`](https://charlescoverdale.github.io/climatekit/reference/ck_hwd.md),
  [`ck_hwm()`](https://charlescoverdale.github.io/climatekit/reference/ck_hwm.md),
  [`ck_hwa()`](https://charlescoverdale.github.io/climatekit/reference/ck_hwa.md)
  (number, frequency, duration, magnitude, amplitude). Cold-wave duals:
  [`ck_cwn()`](https://charlescoverdale.github.io/climatekit/reference/ck_cwn.md),
  [`ck_cwf()`](https://charlescoverdale.github.io/climatekit/reference/ck_cwf.md),
  [`ck_cwd()`](https://charlescoverdale.github.io/climatekit/reference/ck_cwd.md),
  [`ck_cwm()`](https://charlescoverdale.github.io/climatekit/reference/ck_cwm.md),
  [`ck_cwa()`](https://charlescoverdale.github.io/climatekit/reference/ck_cwa.md).
- The ‘CWD’ acronym is used by both ‘ETCCDI’ (Consecutive Wet Days) and
  ‘ET-SCI’ (Cold Wave Duration).
  [`ck_cwd()`](https://charlescoverdale.github.io/climatekit/reference/ck_cwd.md)
  here is the ET-SCI cold-wave version;
  [`ck_wet_days()`](https://charlescoverdale.github.io/climatekit/reference/ck_wet_days.md)
  is the ETCCDI precipitation index. Both function-level documentation
  pages cross-reference the other.

### Discovery surfaces

- [`ck_etccdi_27()`](https://charlescoverdale.github.io/climatekit/reference/ck_etccdi_27.md)
  returns the canonical 27 ‘ETCCDI’ indices as a data frame with `code`,
  `name`, `variable`, `unit`, `definition`, `ck_function`, and `status`
  columns. Use it to audit coverage or to locate the function for a
  given short code.
- [`ck_catalogue()`](https://charlescoverdale.github.io/climatekit/reference/ck_catalogue.md)
  returns the full implementation catalogue (51 rows).
  `ck_browse(sector, standard, search)` filters by sector
  (‘agriculture’, ‘health’, ‘water’, ‘energy’), standard (‘ETCCDI’,
  ‘ET-SCI’, ‘agroclimatic’, etc.), or free-text search.

### Gridded support

- `ck_apply_grid(x, fun, dates, ...)` applies any `ck_*` function over
  the cells of a ‘terra’ ‘SpatRaster’ and returns a SpatRaster with one
  layer per output period.
- `ck_from_netcdf(path, var)` is a thin convenience wrapper around
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  for ‘netCDF’ inputs with file-existence and argument validation.
- ‘terra’ and ‘ncdf4’ are in ‘Suggests’; loading climatekit alone does
  not pull them in.

### Documentation

- `inst/CITATION` provides ‘bibentry’ records for the package and for
  Alexander et al. (2006) and Zhang et al. (2011).
- ‘CITATION.cff’ at the repository root supports GitHub’s citation
  widget.
- New vignette `climdex-migration` documents the function-name crosswalk
  from ‘climdex.pcic’ to climatekit, plus interface-shift notes
  (numeric-vector inputs, tidy data-frame outputs, default reference
  period).

### Documented limitations

- The percentile-based indices use the standard ±2-day calendar window
  but do not implement the Zhang et al. (2005) in-base bootstrap. Years
  inside the reference period therefore have a small self-inclusion
  bias. This is documented in each function’s documentation page.

## climatekit 0.1.0

CRAN release: 2026-03-23

- Initial release.
- 10 temperature indices: frost days, ice days, summer days, tropical
  nights, growing season length, heating/cooling/growing degree days,
  diurnal temperature range, warm spell duration.
- 8 precipitation indices: consecutive dry/wet days, total
  precipitation, heavy/very heavy precipitation days, max 1-day and
  5-day precipitation, precipitation intensity (SDII).
- 3 drought indices: Standardized Precipitation Index (SPI),
  Standardized Precipitation-Evapotranspiration Index (SPEI), potential
  evapotranspiration (Hargreaves method).
- 5 agroclimatic indices: Huglin, Winkler, Branas, first/last frost
  dates.
- 4 comfort indices: wind chill, heat index, humidex, fire danger index.
- Generic dispatcher
  [`ck_compute()`](https://charlescoverdale.github.io/climatekit/reference/ck_compute.md)
  for programmatic index selection.
- [`ck_available()`](https://charlescoverdale.github.io/climatekit/reference/ck_available.md)
  and
  [`ck_metadata()`](https://charlescoverdale.github.io/climatekit/reference/ck_metadata.md)
  for index discovery.
- [`ck_convert_temp()`](https://charlescoverdale.github.io/climatekit/reference/ck_convert_temp.md)
  for temperature unit conversion.
