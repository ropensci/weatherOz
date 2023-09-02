# weatherOz 0.0.1.9000

## Bug fixes

* Fixes [bug](https://github.com/DPIRD-FSI/weatherOz/issues/35) where `find_forecast_towns()` returned all values, not the proper values.

* Fixes example in `get_dpird_extremes()` that didn't use "your_api_key" as all other examples did for error checking.

* Fixes the re-export of `terra::plot()`.

* Fixes example for `get_dpird_extremes()` that didn't follow the package's standard use of "your_api_key" for the `api_key` value in the example.

* Fixes an issue where the package data was not available when this package was called by another, _e.g._ {extractOz} used `get_patched_point()` but could not validate the user-input `values` because `silo_daily_values` could not be found.

* Fixes bug where closed stations are included in the nearest stations when using `find_nearby_stations()` and `include_closed = FALSE` (default).

* Fixes bug where BOM and SILO metadata were not properly merged when using `find_nearby_stations()`

* Fixes bug where results from `get_dpird_summaries()` were truncated to only a few of the last results requested rather than the full set from start date to end date.

* Fixes bug where the DPIRD API would not properly respond to requests from Windows OS machines.

# weatherOz 0.0.1

* Added a `NEWS.md` file to track changes to the package.
