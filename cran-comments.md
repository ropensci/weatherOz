# CRAN Submission - weatherOz 3.0.0

This is a major release with new forecast functionality and breaking changes to wind data structure.

## Major changes in this version
### New features
* Added MET Norway (Norwegian Meteorological Institute) forecast support with `get_metno_forecast()` and `get_metno_daily_forecast()`
* New vignette documenting forecast functionality and combining historical and forecast data
* Three new exported helper functions for advanced forecast data manipulation

### Breaking changes
* Wind data structure in `get_dpird_summaries()` changed from long to wide format
* Wind column names now include `_3m` and `_10m` suffixes (e.g., `wind_max_speed_3m`, `wind_max_speed_10m`)
* Removed `wind_height` column
* Added new wind columns: `wind_max_date` and `wind_max_time_of_day` for each height
* This change provides clearer data structure and eliminates issues with mixed-height rows

### Bug fixes
* Fixed critical timezone bug causing one-day gap when combining historical and forecast data
* Fixed wind time column parsing for mixed date/time formats
* Improved internal code structure with extracted helper functions (no user-facing impact)

## Test environments
* local macOS Sequoia 15.2, R 4.4.3
* GitHub Actions (ubuntu-latest, windows-latest, macOS-latest), R release and devel

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:
```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Rodrigo Pires <rodrigo.pires@dpird.wa.gov.au>'
```

## Breaking changes justification
The wind data structure change is a breaking change but necessary for several reasons:
1. Previous long format with mixed heights per row caused data interpretation issues
3. Aligns with common practices ie one row per observation with consistent column names

## Additional notes
* All tests pass successfully
* New vignette provides examples for forecast functionality
* Migration guide included in NEWS.md for users upgrading from v2.x
* No changes to other API functions - only `get_dpird_summaries()` wind data affected
