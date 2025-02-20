# weatherOz 1.1.0

## Breaking changes

* Functions `get_ag_bulletin()` and `parse_ag_bulletin()` have been defunct because the underlying BOM agricultural bulletin service is no longer available. This removal constitutes a breaking change; please update your code accordingly.

* Documentation has been updated to reflect the removal of the `get_ag_bulletin()` and `parse_ag_bulletin()` functions.

* Tests for the `get_ag_bulletin()` and `parse_ag_bulletin()` functions now errors given that the functions are now defunct.

## Bug fixes

* Fixes [Issue 90](https://github.com/ropensci/weatherOz/issues/90) with the `get_dpird_summaries()` function where tests had fixed dates and errors were generated. The tests `test-get_dpird_summaries.R` have been updated to use previous year from the current date and the tests now pass.

* Fixes other bugs with the `find_stations_in()` and function where the function would not return the correct number of stations, given recent adjustments to the weather station infrastructure.

## Additional fixes and improvements:

* Re-documented package to reflect changes

* Updated test vrc cassette/fixtures

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
