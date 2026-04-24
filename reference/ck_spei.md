# Standardized Precipitation-Evapotranspiration Index (SPEI)

Compute the SPEI by fitting a log-logistic distribution to the monthly
climatic water balance (precipitation minus potential
evapotranspiration) accumulated over a rolling window.

## Usage

``` r
ck_spei(precip, pet, dates, scale = 3)
```

## Arguments

- precip:

  Numeric vector of daily precipitation (mm).

- pet:

  Numeric vector of daily potential evapotranspiration (mm).

- dates:

  Date vector of the same length as `precip` and `pet`.

- scale:

  Integer. Accumulation period in months (default 3).

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## References

Vicente-Serrano, S. M., Begueria, S., & Lopez-Moreno, J. I. (2010). A
multiscalar drought index sensitive to global warming: the Standardized
Precipitation Evapotranspiration Index. *Journal of Climate*, 23(7),
1696-1718.

## Examples

``` r
dates <- seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day")
set.seed(42)
precip <- rgamma(length(dates), shape = 0.5, rate = 0.1)
pet <- rep(3, length(dates))
ck_spei(precip, pet, dates, scale = 3)
#>        period      value index          unit
#> 3  2020-03-01  2.1193625  spei dimensionless
#> 4  2020-04-01  1.4220682  spei dimensionless
#> 5  2020-05-01 -0.3093550  spei dimensionless
#> 6  2020-06-01  1.0631301  spei dimensionless
#> 7  2020-07-01 -3.0902323  spei dimensionless
#> 8  2020-08-01 -3.0902323  spei dimensionless
#> 9  2020-09-01 -3.0902323  spei dimensionless
#> 10 2020-10-01 -3.0902323  spei dimensionless
#> 11 2020-11-01  0.6390013  spei dimensionless
#> 12 2020-12-01  2.1442727  spei dimensionless
#> 13 2021-01-01  3.5218786  spei dimensionless
#> 14 2021-02-01  4.6125134  spei dimensionless
#> 15 2021-03-01  2.6779788  spei dimensionless
#> 16 2021-04-01  2.1264650  spei dimensionless
#> 17 2021-05-01  2.1370960  spei dimensionless
#> 18 2021-06-01  2.1400116  spei dimensionless
#> 19 2021-07-01  3.6479147  spei dimensionless
#> 20 2021-08-01  3.0167892  spei dimensionless
#> 21 2021-09-01  0.6995575  spei dimensionless
#> 22 2021-10-01 -3.0902323  spei dimensionless
#> 23 2021-11-01 -3.0902323  spei dimensionless
#> 24 2021-12-01  1.7721403  spei dimensionless
#> 25 2022-01-01  3.2889542  spei dimensionless
#> 26 2022-02-01  4.4196992  spei dimensionless
#> 27 2022-03-01 -3.0902323  spei dimensionless
#> 28 2022-04-01 -3.0902323  spei dimensionless
#> 29 2022-05-01 -3.0902323  spei dimensionless
#> 30 2022-06-01 -3.0902323  spei dimensionless
#> 31 2022-07-01  3.4702767  spei dimensionless
#> 32 2022-08-01  2.9164981  spei dimensionless
#> 33 2022-09-01  2.2322146  spei dimensionless
#> 34 2022-10-01  2.0394546  spei dimensionless
#> 35 2022-11-01  2.1231246  spei dimensionless
#> 36 2022-12-01  2.0759059  spei dimensionless
#> 37 2023-01-01 -3.0902323  spei dimensionless
#> 38 2023-02-01 -3.0902323  spei dimensionless
#> 39 2023-03-01  2.1424597  spei dimensionless
#> 40 2023-04-01 -3.0902323  spei dimensionless
#> 41 2023-05-01  1.3252879  spei dimensionless
#> 42 2023-06-01  0.7930845  spei dimensionless
#> 43 2023-07-01  3.2530970  spei dimensionless
#> 44 2023-08-01 -3.0902323  spei dimensionless
#> 45 2023-09-01  1.5391048  spei dimensionless
#> 46 2023-10-01  0.9768425  spei dimensionless
#> 47 2023-11-01  1.0430753  spei dimensionless
#> 48 2023-12-01 -3.0902323  spei dimensionless
```
