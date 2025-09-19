# CRAN Re-submission 

This is a resubmission of weatherOz version 2.0.2. The package was archived on 2025-08-25 (v2.0.1) due CRAN policy violation on internet access.
Further issues were present with VCR/HTTP testing compatibility with updated `crul` package dependencies and redirected URLs. 

## Changes in this version
* Updated URLs to reflect DPIRD's IT migration, new webpage and to fix broken links identified in win-builder checks
* Made station count tests more flexible to accommodate natural changes in weather station networks over time, using ranges instead of exact counts
* Fixed compatibility with `crul` 1.6.0 by updating minimum `vcr` requirement to 2.0.0, resolving VCR cassette testing issues caused by deprecated `mock()` function

## Test environments
* local macOS Sequoia 15.6, R 4.4.3
* GitHub Actions (ubuntu-latest, windows-latest, macOS-latest), R release and devel
* R-hub builder

## R CMD check results
There were no ERRORs or WARNINGs. There was 1 NOTE, as follows:
"CRAN repository db overrides:
X-CRAN-Comment: Archived on 2025-08-25 for policy violation.
On Internet access.""

## URL Status
URLs flagged in the win-builder check have been addressed:
* Updated `https://www.agric.wa.gov.au/web-apis` → `https://www.dpird.wa.gov.au/online-tools/apis/` (301 redirect)
* Updated `https://www.pulseaus.com.au/about/australian-pulse-industry` → `https://grainsaustralia.com.au` (301 redirect)

All previously problematic DPIRD URLs have been updated to their new locations:
* `https://www.agric.wa.gov.au/apis/api-terms-and-conditions` → `https://www.dpird.wa.gov.au/online-tools/apis/api-terms-and-conditions/`
* `https://www.agric.wa.gov.au/form/dpird-api-registration` → `https://www.dpird.wa.gov.au/forms/dpird-api-registration/`
* Weather interface URL updated from `weather.agric.wa.gov.au` → `weather.dpird.wa.gov.au`
* **API endpoints**: Currently maintained on working `api.agric.wa.gov.au`

These URLs are now working and accessible following DPIRD's completion of the IT system migration to the new platform.

## Additional notes
This submission addresses the policy violation, the compatibility issues and fixes URL problems identified in R CMD checks. Also addressed CRAN request to "remove the single quotes around every term in the description that is not a package/software/API name" and removed examples for unexported functions.
