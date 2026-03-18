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
