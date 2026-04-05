# weatherOz 3.0.0

**Major release** - New forecast functionality, breaking changes to wind data structure, and critical bug fixes

## Breaking changes

### Wind data structure changed in `get_dpird_summaries()`

Wind data is now returned in wide format with separate columns for each measurement height instead of long format with a `wind_height` column.

* Update code accessing wind columns to use the new `_3m` or `_10m` suffixed names
* Remove references to the `wind_height` column (no longer exists)
* Wind data now provides one row per period instead of multiple rows per height

**Before (v2.0.2 and earlier):**
```r
# multiple rows per station/date
wind_height                    # column indicating 3m or 10m
wind_avg_speed                 # single column
wind_max_speed                 # single column
wind_max_time                  # single column
wind_max_direction_degrees
wind_max_direction_compass_point
```

**After (v3.0.0):**
```r
# single row per station/date
wind_avg_speed_3m              # separate column for 3m height
wind_avg_speed_10m             # separate column for 10m height
wind_max_speed_3m
wind_max_speed_10m
wind_max_time_3m
wind_max_time_10m
wind_max_date_3m               # NEW: date component
wind_max_date_10m              # NEW: date component
wind_max_time_of_day_3m        # NEW: time-of-day component
wind_max_time_of_day_10m       # NEW: time-of-day component
wind_max_direction_degrees_3m
wind_max_direction_degrees_10m
wind_max_direction_compass_point_3m
wind_max_direction_compass_point_10m
```

## New features

### MET Norway forecast support

* Added `get_metno_forecast()` - Retrieves hourly weather forecasts for any Australian location from the Norwegian Meteorological Institute (MET Norway) Locationforecast 2.0 API
* Added `get_metno_daily_forecast()` - Wrapper function that aggregates hourly forecasts into daily summaries (min/max temperature, total precipitation, average wind speed, etc.)
* Added helper functions for advanced users:
  * `metno_timeseries_to_data_table()` - Converts raw JSON timeseries to tidy data.table
  * `metno_resample_data_table()` - Aggregates hourly data to daily, weekly, or monthly frequencies
  * `metno_get_dominant_symbol()` - Determines most representative weather symbol for a period
* New vignette: "weatherOz for the Locationforecast 2.0 (Norwegian Meteorologisk Institutt)" demonstrating forecast usage and combining historical + forecast data

## Bug fixes

### Critical: Fixed one-day gap between historical and forecast data

When combining historical data (SILO/DPIRD) with MET Norway forecast data, there was a missing day causing a discontinuous timeline due to a timezone double-conversion bug in `metno_resample_data_table()`.

* Modified `metno_resample_data_table()` to check timezone before conversion
* Forecast now correctly starts from today instead of tomorrow

### MET.NO cache and fix

* Added caching in `get_metno_forecast()` to reduce repeat calls to MET.NO (session-scoped)
* Added conditional revalidation using `If-Modified-Since` when cached entries are stale and `Last-Modified` is available
* Added support for using cached payloads on HTTP 304 responses
* Added optional stale-cache fallback for HTTP 429 responses
* `get_metno_daily_forecast()` benefits from hourly cache reuse (built on `get_metno_forecast()`)

### Wind data parsing improvements

* Fixed parsing of wind time columns with mixed date/time formats
* Added `.parse_dpird_time_col()` internal helper for robust time parsing
* Added `.widen_wind_height_cols()` internal helper for reshaping wind data from long to wide format
* Wind max time now correctly parsed for both date-only and datetime values
* Fixed `.parse_summary()` to combine paginated `get_dpird_summaries()` responses when DPIRD returns mixed schemas across pages (old and new mixed). Any missing fields are now filled with `NA` instead of erroring during row binding

## Code improvements

### Internal function refactoring

Extracted helper functions from `get_dpird_summaries()` to improve maintainability and reduce code duplication (no user-facing changes):

* `.load_dpird_metadata_file()` - Eliminated 60 lines of duplicated metadata cache loading code
* `.validate_and_expand_dpird_values()` - Validates and expands "all" to complete value lists
* `.validate_dpird_interval()` - Fuzzy matches intervals and modifies values based on interval constraints
* `.calculate_dpird_request_records()` - Calculates expected records and validates date ranges
* `.prepare_wind_time_columns()` - Parses wind time columns and derives date/time components

## Documentation

* Added vignette for MET Norway forecast functionality with examples of:
  * Hourly and daily forecasts
  * Combining historical and forecast data
  * Agricultural heat stress event planning
  * Custom temporal aggregations
* Updated all vignettes to reflect new forecast capabilities
* Improve docs for wind data structure changes

## Testing

* All 457 tests pass with new functionality
* Added tests for MET Norway forecast functions
* Updated wind data tests to reflect new structure

# weatherOz 2.0.2

**CRAN Re-submission** - Maintenance release addressing archival issues

## Bug fixes
* Made station count tests more flexible to accommodate natural changes in weather station networks over time, using ranges instead of exact counts.

* Fixed compatibility with `crul` 1.6.0 by updating minimum `vcr` requirement to 2.0.0, resolving VCR cassette testing issues caused by deprecated `mock()` function.

* URL updates following DPIRD IT system migration:
  * API endpoints: Maintained on `api.agric.wa.gov.au`
  * Updated DPIRD API documentation URLs: `agric.wa.gov.au/apis/api-terms-and-conditions` to `dpird.wa.gov.au/online-tools/apis/api-terms-and-conditions/` and `agric.wa.gov.au/form/dpird-api-registration` to `dpird.wa.gov.au/forms/dpird-api-registration/`
  * Updated displaced URL: `pulseaus.com.au/about/australian-pulse-industry` to `grainsaustralia.com.au`
  * Updated all references in vignettes, README files, and function documentation

## Additional fixes and improvements:
* Addressed CRAN request to "remove the single quotes around every term in the description that is not a package/software/API name" and removed examples for unexported functions.

* Addressed CRAN URL check issues by updating all redirected links to their new working locations.

* Updated all man pages with corrected URLs; Fixed all broken links in README and vignettes.

* Added `@importFrom curl` directives for all curl functions used in the package to resolve import warnings.

* Flexible station count testing: Made tests more resilient by using ranges instead of exact station counts.
  * Accommodates natural changes in weather station networks over time
  * Prevents test failures due to minor station count variations

# weatherOz 2.0.1

## Minor changes
* Updated `testthat` tests to reflect changes in weather station and BOM Radar imagery availability

## Bug fixes
* `get_stations_metadata()` fails to fetch data with invalid API key but provided unhelpful error.

*  Added check to `query_dpird_api()` when using an invalid DPIRD API key ([Issue 94](https://github.com/ropensci/weatherOz/issues/94)).

## Breaking changes

* Functions `get_ag_bulletin()` and `parse_ag_bulletin()` have been defunct because the underlying BOM agricultural bulletin service is no longer available. This removal constitutes a breaking change; please update your code accordingly.

* Documentation has been updated to reflect the removal of the `get_ag_bulletin()` and `parse_ag_bulletin()` functions.

* Tests for the `get_ag_bulletin()` and `parse_ag_bulletin()` functions now errors given that the functions are now defunct.

## Bug fixes

* Fixes [Issue 90](https://github.com/ropensci/weatherOz/issues/90) with the `get_dpird_summaries()` function where tests had fixed dates and errors were generated. The tests `test-get_dpird_summaries.R` have been updated to use previous year from the current date and the tests now pass.

* Fixes other bugs with the `find_stations_in()` and function where the function would not return the correct number of stations, given recent adjustments to the weather station infrastructure.

## Additional fixes and improvements:

* Re-documented package to reflect changes

* Updated test vcr cassette/fixtures

* Updated BOM-related vignette

* Updated WORDLIST for spelling checks

# weatherOz 1.0.0.9000

## Bug Fixes

* Provides useful feedback when users provide a `NULL` API key value.

* Fixes [Issue 82](https://github.com/ropensci/weatherOz/issues/82) where Average wind speed data extracted using `get_dpird_summaries()` is ranked from lowest to highest and does not match the observation recorded at that time from DPIRD summaries, thanks to @alycest.

* Fixes [Issue 83](https://github.com/ropensci/weatherOz/issues/83) where average wind direction data extracted using `get_dpird_summaries()` do not match the observation average wind direction recorded at that time in DPIRD summaries, thanks to @alycest.

* Fixes an error in documentation about the number of types of objects accepted by `find_stations_in()`.

* Fixes a [typo in the README](https://github.com/ropensci/weatherOz/commit/0c544acd4651fa16a3a3dc2e3e24addf5d41187d) that provided incorrect arguments for `get_key()`, thanks to @johnbaums.

# weatherOz 1.0.0

## Major Changes

* Addressed comments after initial CRAN submission (reduce file size and update links to packages/function in the documentation).

* Published manuscript describing package in the [Journal of Open Source Software](https://doi.org/10.21105/joss.06717)

* Package was [peer reviewed](https://github.com/ropensci/software-review/issues/598).

* Changes the name of the function `get_station_metadata()` to `get_stations_metadata()` to avoid clashes with {stationaRy}.

* Adds new functionality, `find_stations_in()` to assist in finding stations in a bounding box or geospatial polygon.

* Allows user to pass along a factor as the `station_code` when requesting weather data.
This allows for the use of the metadata returned from `get_stations_metadata()` to be used directly in queries without changing the class of the `station_code` column from factor to character.

* Added functionality to get_station_metadata() so that it returns the metadata for only one station if required, [commit](https://github.com/ropensci/weatherOz/commit/8166c92f63ed138ccfed966a09e9537a35324b67).

* Several enhanced checks for user inputs to ensure that valid requests are made before sending the API queries are now in place.

## Bug Fixes

* `find_nearby_stations()`

  * Fixes [bug](https://github.com/ropensci/weatherOz/issues/35) where `find_forecast_towns()` returned all values, not the proper values.

  * Fixes bug where BOM and SILO metadata were not properly merged when using `find_nearby_stations()`

  * Fixes [bug](https://github.com/ropensci/weatherOz/issues/36) where closed stations are included in the nearest stations when using `find_nearby_stations()` and `include_closed = FALSE` (default).
  
  * Fixes the messages when stations aren't found nearby. In certain cases the message indicated that a `station_code` was used when `latitude` and `longitude` had been provided and _vice versa_.
  
  * Fixes bug when there are no stations that meet the criteria set by the user and an error was emitted that "x" must be a {data.table} object. The function now simply errors if there are no stations from either API that meet the criteria.

* `get_dpird_extremes()`

  * Fixes example in `get_dpird_extremes()` that didn't use "your_api_key" as all other examples did for error checking.

  * Fixes example for `get_dpird_extremes()` that didn't follow the package's standard use of "your_api_key" for the `api_key` value in the example.

* Fixes the re-export of `terra::plot()` where checks indicated that it does not have a documented return value or examples.

* Fixes a bug where the package data was not available when this package was called by another, _e.g._, {extractOz} used `get_patched_point()` but could not validate the user-input `values` because `silo_daily_values` could not be found.

* Fixes bug where results from `get_dpird_summaries()` were truncated to only a few of the last results requested rather than the full set from start date to end date.

* Fixes [bug](https://github.com/ropensci/weatherOz/issues/38) where the DPIRD API would not properly respond to requests from Windows OS machines.

* Fixes [bug](https://github.com/ropensci/weatherOz/issues/57) where SILO Data Drill gridded data were not available with enough precision.

* Fixes bug where `include_closed` wasn't passed along to the API when querying DPIRD station weather data.
All queries now included closed stations by default so that a request for a closed station is not denied due to this not being specified, [commit](https://github.com/ropensci/weatherOz/commit/e241057e7c0791a904cccd3029c217b41543f05b).

# weatherOz 0.0.1

* Added a `NEWS.md` file to track changes to the package.
