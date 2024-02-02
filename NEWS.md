# weatherOz 0.0.1.9000

## Major Changes

* Changes the name of the function `get_station_metadata()` to `get_stations_metadata()` to avoid clashes with {stationaRy}

## Bug Fixes

* `find_nearby_stations()`

  * Fixes [bug](https://github.com/DPIRD-FSI/weatherOz/issues/35) where `find_forecast_towns()` returned all values, not the proper values.

  * Fixes bug where BOM and SILO metadata were not properly merged when using `find_nearby_stations()`

  * Fixes [bug](https://github.com/DPIRD-FSI/weatherOz/issues/36) where closed stations are included in the nearest stations when using `find_nearby_stations()` and `include_closed = FALSE` (default).
  
  * Fixes the messages when stations aren't found nearby. In certain cases the message indicated that a `station_code` was used when `latitude` and `longitude` had been provided and _vice versa_.
  
  * Fixes bug when there are no stations that meet the criteria set by the user and an error was emitted that "x" must be a {data.table} object. The function now simply errors if there are no stations from either API that meet the criteria.

* `get_dpird_extremes()`

  * Fixes example in `get_dpird_extremes()` that didn't use "your_api_key" as all other examples did for error checking.

  * Fixes example for `get_dpird_extremes()` that didn't follow the package's standard use of "your_api_key" for the `api_key` value in the example.


* Fixes the re-export of `terra::plot()` where checks indicated that it does not have a documented return value or examples.

* Fixes an issue where the package data was not available when this package was called by another, _e.g._, {extractOz} used `get_patched_point()` but could not validate the user-input `values` because `silo_daily_values` could not be found.

* Fixes bug where results from `get_dpird_summaries()` were truncated to only a few of the last results requested rather than the full set from start date to end date.

* Fixes [bug](https://github.com/DPIRD-FSI/weatherOz/issues/38) where the DPIRD API would not properly respond to requests from Windows OS machines.

# weatherOz 0.0.1

* Added a `NEWS.md` file to track changes to the package.
