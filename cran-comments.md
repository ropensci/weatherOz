# CRAN Submission - weatherOz 3.0.0

This is a major release with new weather forecast functionality and breaking changes to wind data structure.

## Major changes
### New features
* Added MET Norway (Norwegian Meteorological Institute) forecast support with `get_metno_forecast()` and `get_metno_daily_forecast()`
* New vignette documenting forecast functionality and combining historical and forecast data
* Three new helper functions for forecast data manipulation

### Breaking changes
* Wind data structure in `get_dpird_summaries()` changed from long to wide format
* Wind column names now include `_3m` and `_10m` suffixes (e.g., `wind_max_speed_3m`, `wind_max_speed_10m`)
* Removed `wind_height` column
* Added new wind columns: `wind_max_date` and `wind_max_time_of_day` for each height
* Removed `get_ag_bulletin()` and `parse_ag_bulletin()` from the package in v3.0.0 after being defunct in v2.x because BOM discontinued the agricultural bulletin service

### Bug fixes
* Fixed critical timezone bug causing one-day gap when combining historical and forecast data
* Fixed wind time column parsing for mixed date/time formats

## Test environments
* local macOS Sequoia 15.2, R 4.4.3
* GitHub Actions (ubuntu-latest, windows-latest, macOS-latest), R release and devel

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Breaking changes justification
The wind data structure change is a breaking change but necessary:
1. Previous long format with mixed heights per row caused data interpretation issues
2. Aligns with expectations i.e., one row per observation with consistent column names

The agricultural bulletin function removal is also a breaking change because:
1. The BOM service has been discontinued and URLs now return HTTP 403
2. Functions were already defunct in v2.0.2, so v3.0.0 completes the lifecycle transition to removal
3. Removing these endpoints eliminates a CRAN URL feasibility issue

## Additional notes
* All tests pass successfully
* New vignette provides examples for forecast functionality
* Migration guide included in NEWS.md for users upgrading from v2.x
* `vignettes/weatherOz_for_BOM.Rmd` now includes a short historical note about agricultural bulletin function removal
