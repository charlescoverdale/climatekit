# Standardized Precipitation Index (SPI)

Compute the SPI by fitting a gamma distribution to monthly precipitation
totals accumulated over a rolling window, then transforming to standard
normal deviates.

## Usage

``` r
ck_spi(precip, dates, scale = 3)
```

## Arguments

- precip:

  Numeric vector of daily precipitation (mm).

- dates:

  Date vector of the same length as `precip`.

- scale:

  Integer. Accumulation period in months (default 3).

## Value

A data frame with columns `period`, `value`, `index`, and `unit`.

## References

McKee, T. B., Doesken, N. J., & Kleist, J. (1993). The relationship of
drought frequency and duration to time scales.

## Examples

``` r
dates <- seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day")
set.seed(42)
precip <- rgamma(length(dates), shape = 0.5, rate = 0.1)
ck_spi(precip, dates, scale = 3)
#>        period       value index          unit
#> 3  2020-03-01 -0.11011769   spi dimensionless
#> 4  2020-04-01 -0.05373019   spi dimensionless
#> 5  2020-05-01 -0.63179704   spi dimensionless
#> 6  2020-06-01 -0.34386911   spi dimensionless
#> 7  2020-07-01 -1.45546229   spi dimensionless
#> 8  2020-08-01 -0.90296848   spi dimensionless
#> 9  2020-09-01 -0.97445477   spi dimensionless
#> 10 2020-10-01 -0.69220945   spi dimensionless
#> 11 2020-11-01 -0.48619603   spi dimensionless
#> 12 2020-12-01  0.92316398   spi dimensionless
#> 13 2021-01-01  1.17686170   spi dimensionless
#> 14 2021-02-01  1.19675188   spi dimensionless
#> 15 2021-03-01  1.53862378   spi dimensionless
#> 16 2021-04-01  1.64855339   spi dimensionless
#> 17 2021-05-01  1.68464304   spi dimensionless
#> 18 2021-06-01  1.69319000   spi dimensionless
#> 19 2021-07-01  1.30681187   spi dimensionless
#> 20 2021-08-01  1.23614871   spi dimensionless
#> 21 2021-09-01 -0.55620266   spi dimensionless
#> 22 2021-10-01 -0.74501028   spi dimensionless
#> 23 2021-11-01 -0.87901274   spi dimensionless
#> 24 2021-12-01  0.02292495   spi dimensionless
#> 25 2022-01-01  0.09089535   spi dimensionless
#> 26 2022-02-01  0.05428237   spi dimensionless
#> 27 2022-03-01 -1.26200269   spi dimensionless
#> 28 2022-04-01 -0.83629965   spi dimensionless
#> 29 2022-05-01 -0.84903114   spi dimensionless
#> 30 2022-06-01 -0.89901194   spi dimensionless
#> 31 2022-07-01  0.35982039   spi dimensionless
#> 32 2022-08-01  0.72803083   spi dimensionless
#> 33 2022-09-01  1.65083132   spi dimensionless
#> 34 2022-10-01  1.70049667   spi dimensionless
#> 35 2022-11-01  1.69640022   spi dimensionless
#> 36 2022-12-01  0.68912675   spi dimensionless
#> 37 2023-01-01 -1.26757630   spi dimensionless
#> 38 2023-02-01 -1.25093754   spi dimensionless
#> 39 2023-03-01 -0.16684883   spi dimensionless
#> 40 2023-04-01 -0.76134150   spi dimensionless
#> 41 2023-05-01 -0.20469462   spi dimensionless
#> 42 2023-06-01 -0.45403613   spi dimensionless
#> 43 2023-07-01 -0.21079630   spi dimensionless
#> 44 2023-08-01 -1.06138507   spi dimensionless
#> 45 2023-09-01 -0.12378988   spi dimensionless
#> 46 2023-10-01 -0.27006092   spi dimensionless
#> 47 2023-11-01 -0.33551191   spi dimensionless
#> 48 2023-12-01 -1.63473901   spi dimensionless
```
