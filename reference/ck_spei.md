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
#> Warning: SPEI fitting failed: L-skewness out of range. Returning NAs.
#> Warning: SPEI fitting failed: L-skewness out of range. Returning NAs.
#> Warning: SPEI fitting failed: L-skewness out of range. Returning NAs.
#> Warning: SPEI fitting failed: L-skewness out of range. Returning NAs.
#>        period       value index          unit
#> 3  2020-03-01  0.09350209  spei dimensionless
#> 4  2020-04-01  0.65413050  spei dimensionless
#> 5  2020-05-01 -0.13016494  spei dimensionless
#> 6  2020-06-01  0.34906431  spei dimensionless
#> 8  2020-08-01 -0.71839509  spei dimensionless
#> 9  2020-09-01 -0.88961909  spei dimensionless
#> 10 2020-10-01 -0.32470178  spei dimensionless
#> 11 2020-11-01  0.15364327  spei dimensionless
#> 15 2021-03-01  1.29161793  spei dimensionless
#> 16 2021-04-01  1.56001783  spei dimensionless
#> 17 2021-05-01  1.56249955  spei dimensionless
#> 18 2021-06-01  1.58397556  spei dimensionless
#> 20 2021-08-01  1.07585476  spei dimensionless
#> 21 2021-09-01 -0.06492128  spei dimensionless
#> 22 2021-10-01 -0.60473222  spei dimensionless
#> 23 2021-11-01 -1.01052347  spei dimensionless
#> 27 2022-03-01 -1.10655643  spei dimensionless
#> 28 2022-04-01 -0.64567489  spei dimensionless
#> 29 2022-05-01 -0.79241883  spei dimensionless
#> 30 2022-06-01 -1.05023289  spei dimensionless
#> 32 2022-08-01  0.71280617  spei dimensionless
#> 33 2022-09-01  1.46483020  spei dimensionless
#> 34 2022-10-01  1.74668897  spei dimensionless
#> 35 2022-11-01  1.60458580  spei dimensionless
#> 39 2023-03-01  0.12202701  spei dimensionless
#> 40 2023-04-01 -0.41140329  spei dimensionless
#> 41 2023-05-01  0.54261255  spei dimensionless
#> 42 2023-06-01  0.18422503  spei dimensionless
#> 44 2023-08-01 -0.86533087  spei dimensionless
#> 45 2023-09-01  0.45254686  spei dimensionless
#> 46 2023-10-01  0.67032696  spei dimensionless
#> 47 2023-11-01  0.38821492  spei dimensionless
```
