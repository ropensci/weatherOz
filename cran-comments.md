# CRAN Re-submission 

This is a resubmission of weatherOz version 2.0.2. The package was archived on 2025-08-25 due to VCR/HTTP testing compatibility issues with updated `crul` package dependencies. This submission addresses those compatibility issues and fixes URL problems identified in win-builder checks.

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

## URL Status
Some URLs flagged in the win-builder check have been addressed:
* Updated `https://www.agric.wa.gov.au/web-apis` → `https://www.dpird.wa.gov.au/online-tools/apis/` (301 redirect)
* Updated `https://www.pulseaus.com.au/about/australian-pulse-industry` → `https://grainsaustralia.com.au` (301 redirect)

All previously problematic DPIRD URLs have been updated to their new locations:
* `https://www.agric.wa.gov.au/apis/api-terms-and-conditions` → `https://www.dpird.wa.gov.au/online-tools/apis/api-terms-and-conditions/`
* `https://www.agric.wa.gov.au/form/dpird-api-registration` → `https://www.dpird.wa.gov.au/forms/dpird-api-registration/`

These URLs are now working and accessible following DPIRD's completion of their IT system migration to the new platform.

## Additional notes
This submission addresses critical compatibility issues that were preventing the package from working properly with the latest versions of HTTP testing dependencies. The fixes ensure continued functionality for users and maintain the quality of the test suite.
