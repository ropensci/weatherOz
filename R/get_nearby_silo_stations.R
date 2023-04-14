#' Find stations within a given radius of a geographic point or weather station
#'
#' Searches for stations available in the SILO database and returnes values
#' sorted from nearest to farthest.
#'
#' @param distance_km A `numeric` value for the distance in kilometres in which
#'  to search for nearby stations from a given geographic location or known
#'  `station_id`. Cannot be used when `longitude` and `latitude` are provided.
#' @param longitude A `numeric` value for longitude expressed as decimal degrees
#'  (DD) (WGS84). Required if `latitude` is used. Cannot be used if `station_id`
#'  is provided.
#' @param latitude A `numeric` value for latitude expressed as decimal degrees
#'   (DD) (WGS84). Required if `longitude` is used. Cannot be used if
#'   `station_id` is provided.
#' @param station_id A `character` value for a known station from which the
#' nearby stations are to be identified.
#'
#' @examplesIf interactive()
#' # find stations within 50 km of Finke Post Office, NT
#' find_nearby_silo_stations(distance_km = 50,
#'                          station_id = "015526")
#'
#' # find stations within 500 km of Esperance, WA
#' find_nearby_silo_stations(distance_km = 50,
#'                           longitude = 121.8913,
#'                           latitude = -33.8614)
#'
#' @return A `data.table` of available stations and associated metadata.
#'
#' @export
find_nearby_silo_stations <- function(distance_km = NULL,
                                      longitude = NULL,
                                      latitude = NULL,
                                      station_id = NULL) {
  if (!is.null(station_id) &&
      !is.null(longitude) && !is.null(latitude)) {
    stop(
      call. = FALSE,
      "You have provided `station_id` along with `longitude` and \n",
      "`latitude` values. Please provide only one, either `station_id` \n",
      "or a pairing of `longitude` and `latitude` values."
    )
  }
  if (!is.null(longitude) || !is.null(latitude)) {
    stopifnot("Provide both `latitude` and `longitude` values" =
                length(latitude) == length(longitude))
    .check_lonlat(longitude = longitude, latitude = latitude)
  }

  if (!is.null(station_id)) {
    x <-
      .get_silo_stations(
        .station_id = station_id,
        .distance_km = distance_km
      )

    # Warn user if there are no stations within the input radius and return data
    if (nrow(x) == 0L) message(
      paste0(
        "No SILO stations found around a radius of < ",
        distance_km, " km\n",
        " from station ",
        station_id, ".\n"
      )
    )

    return(x)

  } else if (is.null(station_id)) {
    x <-
      .get_silo_stations(.station_id = NULL)

    x[, "distance" := .haversine_distance(latitude,
                                          longitude,
                                          lat,
                                          lon)] |>
      data.table::setorderv("distance")

    x[, distance_km := NULL]

    x <-
      x[distance %in%
          x[(distance <= distance_km)]$distance]

    if (nrow(x) == 0L) message(
      paste0(
        "No SILO stations found around a radius of < ",
        distance_km, " km\n",
        " from coordinates ",
        latitude, " and ",
        longitude,
        " (lat/lon)\n"
      )
    )

    return(x)
  }
}

#' Create a data.table object of all stations available in SILO
#'
#' @param .station_id A string identifying a BOM station in SILO which should
#' be used as the centre point to determine stations that fall within
#'  `.distance_km`
#' @param .distance_km A `numeric` value for the distance in kilometres in which
#'  to search for nearby stations from a given geographic location or known
#'  `.station_id`.
#'
#' Uses SILO's cgi-bin web interface to query and download a text file of
#' BOM weather stations in SILO within 2500 km of Finke, NT (near the Lambert
#' centre of Australia). Stations are returned in order of distance from the
#' Finke Post Office.
#'
#' @noRd
.get_silo_stations <- function(.station_id, .distance_km) {
  base_url <-
    "https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?format="

  # Define column types
  col_types <- c(
    Number = "factor",
    `Station name` = "character",
    Latitude = "numeric",
    Longitud = "numeric",
    Stat = "factor",
    Elevat. = "numeric",
    `Distance (km)` = "numeric"
  )

  # Define column names
  col_names = c(
    "station_code",
    "station_name",
    "lat",
    "lon",
    "state",
    "elevation",
    "distance_km"
  )

  # if `.station_id` is not provided, fetch all stations in AU. Otherwise, use
  # the cgi-bin interface to find stations within `.distance_km` of the given
  # `.station_id`
  if (is.null(.station_id)) {
    r <- data.table::fread(paste0(base_url,
                                  "near&station=015526&radius=10000"),
                           colClasses = col_types,
                           col.names = col_names)
  } else {
    r <- data.table::fread(paste0(
      base_url,
      "near&station=",
      .station_id,
      "&radius=",
      .distance_km
    ),
    colClasses = col_types,
    col.names = col_names
    )
  }

  # Manipulate cols
  r[, station_name := .cap_names(s = station_name)]
  r[, owner := "BOM"]

  data.table::setkey(r, "station_code")
  data.table::setcolorder(r, c(1:5, 6, 8, 7))

  return(r[])
}
