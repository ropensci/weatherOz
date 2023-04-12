#' Find stations within a given radius of a geographic point or weather station
#'
#' Searches for stations available in the SILO database and returnes values
#' sorted from nearest to farthest.
#'
#' @param distance_km A `numeric` value for the distance in kilometres in which to
#'  search for nearby stations from a given geographic location or known
#'  `station_id`.
#' @param longitude A `numeric` value for longitude expressed as decimal degrees
#'  (DD) (WGS84). Required if `latitude` is used.
#' @param latitude A `numeric` value for latitude expressed as decimal degrees
#'   (DD) (WGS84). Required if `longitude` is used.
#' @param station_id A `character` value for a known station from which the
#' nearby stations are to be identified.
#'
#' @examplesIf interactive()
#' find_nearby_silo_stations <- function(radius = 500,
#'                                       station_id = "015526")
#'
#' @return A `data.table` of available stations and associated metadata.
#'
#' @export
find_nearby_silo_stations <- function(distance_km,
                                      longitude = NULL,
                                      latitude = NULL,
                                      station_id = NULL) {

  if (!is.null(longitude) || !is.null(latitude)) {
    stopifnot("Provide both `latitude` and `longitude` values" =
                length(latitude) == length(longitude))
    .check_lonlat(longitude = longitude, latitude = latitude)
  }

  x <- .get_silo_stations()

  # if lon/lat are requested ----
  if (is.null(station_id) &&
      !is.null(latitude) & !is.null(longitude)) {
    x[, "distance" := .haversine_distance(latitude,
                                          longitude,
                                          lat,
                                          lon)] |>
      data.table::setorderv("distance")
    x[distance %in%
        x[(distance <= distance_km)]$distance]
    return(x[])
  } else {
    .station_id <- station_id
    x <- x[station_id %in% .station_id]
    x[, "distance" := .haversine_distance(latitude,
                                          longitude,
                                          lat,
                                          lon)] |>
      data.table::setorderv("distance")
    x[distance %in%
        x[(distance <= distance_km)]$distance]
  }
}
#' Create a data.table object of all stations available in SILO
#'
#' Uses SILO's cgi-bin web interface to query and download a text file of
#' BOM weather stations in SILO within 2500 km of Finke, NT (near the Lambert
#' centre of Australia). Stations are returned in order of distance from the
#' Finke Post Office.
#'
#' @noRd
.get_silo_stations <- function() {
  curl::curl_download(url = "https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?format=near&station=015526&radius=2500",
                      destfile = file.path(tempdir(), "stations.txt"))
  r <- data.table::fread(file.path(tempdir(), "stations.txt"))[, -7]
  data.table::setnames(r,
                       names(r),
                       c(
                         "station_id",
                         "station_name",
                         "lat",
                         "lon",
                         "state",
                         "elevation"
                       ))
  data.table::setkey(r, "station_id")
  return(r)
}
