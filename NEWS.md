# weatherOz 0.0.1.9000

## Bug fixes

* Fixed [bug](https://github.com/DPIRD-FSI/weatherOz/issues/35) where `find_forecast_town()` returned all values, not the proper values.

* Fixed example in `get_dpird_extremes()` that didn't use "your_api_key" as all other examples did for error checking.

* Fix the re-export of `terra::plot()`.

* Fix example for `get_dpird_extremes()` that didn't follow the package's standard use of "your_api_key" for the `api_key` value in the example.

* Fix an issue where the package data was not available when this package was called by another, _e.g._ {extractOz} used `get_patched_point()` but could not validate the user-input `values` because `silo_daily_values` could not be found.

* Fix bug where closed stations are included in the nearest stations when using `find_nearby_stations()` and `include_closed = FALSE` (default).

* Fix bug where BOM and SILO metadata were not properly merged when using `find_nearby_stations()`

* Fix bug where results from `get_dpird_summary()` were truncated to only a few of the last results requested rather than the full set from start date to end date.

# weatherOz 0.0.1

* Added a `NEWS.md` file to track changes to the package.
