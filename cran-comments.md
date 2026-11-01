## Submission

A patch release. `ck_spei()` with its default log-logistic distribution
returned `NA` for every value in a calendar month whose climatic water
balance had zero or negative L-skewness. The fit used the
parameterisation `beta = 1 / tau3`, which exists only for positive
skewness; it now uses the equivalent generalised logistic form (Hosking
1997), which covers any L-skewness between -1 and 1, as the SPEI package
does. Values for positively skewed months are unchanged (agreement to
within 1e-13); months that were `NA` now have values. Regression tests
cover both.

## R CMD check results

0 errors | 0 warnings | 0 notes

Local check on macOS (aarch64-apple-darwin), R 4.5.2, with
`--as-cran --run-donttest`. When the check machine cannot reach
worldclockapi.com, "checking for future file timestamps" reports "unable
to verify current time"; that is not a package issue.

## Downstream dependencies

None.
