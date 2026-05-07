# climatekit: Unified Climate Indices for Temperature, Precipitation, and Drought

Compute 60 standard climate indices from daily weather observations.
Provides the canonical 'ETCCDI' 27 set (Expert Team on Climate Change
Detection and Indices) and the 'ET-SCI' heatwave and cold-wave families
(Expert Team on Sector-specific Climate Indices), together with
agroclimatic indices (Huglin, Winkler, Branas), drought indices ('SPI'
and 'SPEI'), and human-comfort indices (wind chill, heat index, humidex,
fire danger). Daily inputs are numeric vectors plus a 'Date' vector;
outputs are tidy data frames with period, value, index, and unit
columns. Optional gridded support via 'terra' applies any index over a
'SpatRaster' and reads 'netCDF' input. No external API calls; pairs with
data packages such as 'readnoaa' for acquisition. References: Alexander
et al. (2006)
[doi:10.1029/2005JD006290](https://doi.org/10.1029/2005JD006290) ; Zhang
et al. (2011) [doi:10.1002/wcc.147](https://doi.org/10.1002/wcc.147) .

## See also

Useful links:

- <https://charlescoverdale.github.io/climatekit/>

- <https://github.com/charlescoverdale/climatekit>

- Report bugs at <https://github.com/charlescoverdale/climatekit/issues>

## Author

**Maintainer**: Charles Coverdale <charlesfcoverdale@gmail.com>
