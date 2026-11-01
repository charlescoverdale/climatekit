# Standardised Precipitation-Evapotranspiration Index (SPEI)

Compute the SPEI by fitting a log-logistic distribution to the monthly
climatic water balance (precipitation minus potential
evapotranspiration) accumulated over a rolling window.

## Usage

``` r
ck_spei(precip, pet, dates, scale = 3, distribution = c("log-logistic", "gev"))
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

- distribution:

  Character. Either `"log-logistic"` (default, Vicente-Serrano et
  al. 2010) or `"gev"` (Generalised Extreme Value, fitted by L-moments;
  preferred for water-balance series with heavy upper or lower tails).

  The three-parameter log-logistic is fitted by L-moments, which
  requires an L-skewness in `(0, 1)`. A calendar month whose water
  balance is symmetric or left-skewed falls outside that range; the fit
  warns and returns `NA` for that month rather than reporting a value
  from a distribution that does not describe the data. Use
  `distribution = "gev"`, which accommodates both tails, when this
  happens often in your series.

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
#>        period        value index          unit
#> 3  2020-03-01  0.093502089  spei dimensionless
#> 4  2020-04-01  0.654130498  spei dimensionless
#> 5  2020-05-01 -0.130164941  spei dimensionless
#> 6  2020-06-01  0.349064306  spei dimensionless
#> 7  2020-07-01 -1.131718122  spei dimensionless
#> 8  2020-08-01 -0.718395087  spei dimensionless
#> 9  2020-09-01 -0.889619094  spei dimensionless
#> 10 2020-10-01 -0.324701783  spei dimensionless
#> 11 2020-11-01  0.153643269  spei dimensionless
#> 12 2020-12-01  0.798306865  spei dimensionless
#> 13 2021-01-01  0.875630337  spei dimensionless
#> 14 2021-02-01  0.903167756  spei dimensionless
#> 15 2021-03-01  1.291617930  spei dimensionless
#> 16 2021-04-01  1.560017834  spei dimensionless
#> 17 2021-05-01  1.562499545  spei dimensionless
#> 18 2021-06-01  1.583975560  spei dimensionless
#> 19 2021-07-01  1.088059255  spei dimensionless
#> 20 2021-08-01  1.075854758  spei dimensionless
#> 21 2021-09-01 -0.064921279  spei dimensionless
#> 22 2021-10-01 -0.604732222  spei dimensionless
#> 23 2021-11-01 -1.010523471  spei dimensionless
#> 24 2021-12-01 -0.549632124  spei dimensionless
#> 25 2022-01-01 -0.021811213  spei dimensionless
#> 26 2022-02-01 -0.002107092  spei dimensionless
#> 27 2022-03-01 -1.106556426  spei dimensionless
#> 28 2022-04-01 -0.645674890  spei dimensionless
#> 29 2022-05-01 -0.792418831  spei dimensionless
#> 30 2022-06-01 -1.050232892  spei dimensionless
#> 31 2022-07-01  0.228939468  spei dimensionless
#> 32 2022-08-01  0.712806165  spei dimensionless
#> 33 2022-09-01  1.464830196  spei dimensionless
#> 34 2022-10-01  1.746688969  spei dimensionless
#> 35 2022-11-01  1.604585801  spei dimensionless
#> 36 2022-12-01  0.290547628  spei dimensionless
#> 37 2023-01-01 -0.937710126  spei dimensionless
#> 38 2023-02-01 -0.909162220  spei dimensionless
#> 39 2023-03-01  0.122027007  spei dimensionless
#> 40 2023-04-01 -0.411403289  spei dimensionless
#> 41 2023-05-01  0.542612546  spei dimensionless
#> 42 2023-06-01  0.184225032  spei dimensionless
#> 43 2023-07-01 -0.260915796  spei dimensionless
#> 44 2023-08-01 -0.865330867  spei dimensionless
#> 45 2023-09-01  0.452546865  spei dimensionless
#> 46 2023-10-01  0.670326962  spei dimensionless
#> 47 2023-11-01  0.388214919  spei dimensionless
#> 48 2023-12-01 -1.402827757  spei dimensionless
```
