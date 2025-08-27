# CRAN re-submission 

This is a resubmission of weatherOz version 2.0.2, which fixes R CMD tests errors and compatibility issues with updated dependencies.

## Changes in this version
* Fixed compatibility with `crul` 1.6.0 by updating minimum `vcr` requirement to 2.0.0, resolving VCR cassette testing issues caused by deprecated `mock()` function
* Made station count tests more flexible to accommodate natural changes in weather station networks over time, using ranges instead of exact counts

## Test environments
* local macOS Sequoia 15.6, R 4.4.3
* GitHub Actions (ubuntu-latest, windows-latest, macOS-latest), R release and devel
* R-hub builder

## R CMD check results
There were no ERRORs or WARNINGs.
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

There was 1 NOTE on some platforms. This NOTE is due to the vignettes containing embedded images and examples, which is expected for a package that processes weather data and creates visualizations.

## Test results
* All VCR/cassette HTTP mocking tests now pass with the updated dependencies
* Tests use flexible ranges for station counts to accommodate natural changes in weather station networks
* 402 tests pass

## Additional notes
This submission addresses critical compatibility issues that were preventing the package from working properly with the latest versions of HTTP testing dependencies.
