# CRAN Submission - weatherOz 3.0.1

This is a patch release to fix the check ERRORs reported for 3.0.0 on
r-devel (Linux and Windows), r-patched Linux, r-release Linux and r-oldrel
Windows.

## What was wrong

`test-find_stations_in.R` asserted a fixed station count for a test polygon.
The count depends on the live BOM station list, which the package downloads
with `utils::download.file()` and is not recorded by the `vcr` cassette used
in that test. The list changed and the count moved outside the asserted range.

## What changed

* The tests now check behaviour (every returned station lies inside the
  polygon; a substantial number is returned) instead of a fixed count.
* The `get_stations_metadata(which_api = "all")` test carried the same kind
  of fixed-count assertion and is changed the same way.
* The `find_stations_in()`, `find_forecast_towns()` and BOM-reaching
  `get_stations_metadata()` tests now `skip_on_cran()` and `skip_if_offline()`,
  in line with the other tests in the package that reach the BOM servers.
* The satellite imagery tests, which read from the BOM FTP server, now
  `skip_on_cran()` as well. No test that reaches the BOM servers runs on
  CRAN.
* README: removed a reference to the agriculture bulletins, which were
  removed in 3.0.0.
* No changes to package code.

## Test environments

* local macOS, R release
* win-builder: R-devel, R-release and R-oldrelease
* R-hub: Linux R-devel, Windows R-devel
* GitHub Actions: macOS release, Windows release, Ubuntu devel, release
  and oldrel-1

## R CMD check results

0 errors | 0 warnings | 0 notes on win-builder R-devel and R-release.

On win-builder R-oldrelease, the incoming URL check reported
www.bom.gov.au URLs as 403 Forbidden. These URLs are valid and open in a
browser; the Bureau of Meteorology website rejects automated requests.
The same URLs passed the check on R-devel and R-release.
