# file: /R/get_nearby_stations.R
#
# This file is part of the R-package weatherOz
#
# Copyright (C) 2023 DPIRD
#	<https://www.dpird.wa.gov.au>

#' Find nearby weather stations given geographic coordinates or a station code
#'
#' @param latitude A `numeric` value for latitude expressed as decimal degrees
#'   (DD) (WGS84).
#' @param longitude A `numeric` value for longitude expressed as decimal degrees
#'  (DD) (WGS84).
#' @param station_code A `string` with the station code for the station of
#' interest. Optional and defaults to `NULL`.
#' @param distance_km A `numeric` value for distance to limit the search from
#'  the station or location of interest.  Defaults to 100 km.
#' @param api_key A `string` value that is the user's \acronym{API} key from
#'  \acronym{DPIRD} (see <https://www.agric.wa.gov.au/web-apis>).  Only used
#'  when `which_api` is 'DPIRD' or 'all'.
#' @param which_api A `string` value that indicates which API to use.  Defaults
#'  to 'silo'. Valid values are 'all', for both \acronym{SILO} (\acronym{BOM})
#'  and \acronym{DPIRD} weather station networks; "silo" for only stations in
#'  the \acronym{SILO} network; or 'dpird' for stations in the \acronym{DPIRD}
#'  network.
#'
#' @return a [data.table::data.table] with 'station_code', 'station_name',
#'  'latitude', 'longitude', 'elev_m', 'state', 'owner', and 'distance'.
#'  Data are sorted by increasing distance from station or location of interest.
#'
#' @note You can request your own \acronym{API} key from \acronym{DPIRD} for
#' free by filling out the form found at <https://www.agric.wa.gov.au/web-apis>.
#'
#' @examples
#' \dontrun{
#' # Query WA only stations and return both DPIRD's and BOM's stations for
#' # the Northam WA station, returning stations with 50 km of this station
#' wa_stn <- find_nearby_stations(
#'   station_code = "NO",
#'   distance_km = 50,
#'   api_key = "YOUR API KEY",
#'   which_api = "DPIRD"
#' )
#'
#' # Query Wagga Wagga BOM station.
#' wagga_stn <- find_nearby_stations(
#'   latitude = -35.1583,
#'   longitude = 147.4575,
#'   distance_km = 200,
#'   which_api = "SILO"
#' )
#' }
#'
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au}
#' @export

find_nearby_stations <- function(latitude = NULL,
                                 longitude = NULL,
                                 station_code = NULL,
                                 distance_km = 100,
                                 api_key = NULL,
                                 which_api = "silo") {

  which_api <- .check_which_api(which_api)

  .check_location_params(.latitude = latitude,
                         .longitude = longitude,
                         .station_code = station_code)

  if (length(station_code) == 1 ||
      length(latitude) + length(longitude) == 2) {
    if (is.null(station_code)) {
      .check_lonlat(longitude = longitude, latitude = latitude)

      if (which_api == "silo") {
        # Get list of all stations in Australia
        # Calculate distance from lat/lon coordinates, sort and filter data to
        # distance threshold. Round distance in km to .0

        out <- .find_nearby_silo_stations(distance_km = distance_km,
                                          longitude = longitude,
                                          latitude = latitude)
        return(out[])

      } else if (which_api == "dpird") {
        out <-
          .get_dpird_stations(
            .latitude = latitude,
            .longitude = longitude,
            .distance_km = distance_km,
            .api_key = api_key
          )
        return(out[])

      } else if (which_api == "all") {
        # return dpird
        out_dpird <-
          .get_dpird_stations(
            .latitude = latitude,
            .longitude = longitude,
            .distance_km = distance_km,
            .api_key = api_key
          )
        # return silo
        out_silo <-
          .find_nearby_silo_stations(distance_km = distance_km,
                                     longitude = longitude,
                                     latitude = latitude)

        if (!is.null(out_dpird) & nrow(out_silo) != 0L) {
          out <- rbind(out_dpird, out_silo)
          data.table::setorderv(out, "distance")
          return(out[])

        } else if (is.null(out_dpird) & nrow(out_silo) != 0L) {
          return(out_silo[])

        } else if (!is.null(out_dpird) & nrow(out_silo) == 0L) {
          return(out_dpird[])
        }
      }
    }
  }

  if (!is.null(station_code)) {
    if (which_api == "silo" &
        isTRUE(grepl("^\\d+$", station_code))) {
      out <- .find_nearby_silo_stations(distance_km = distance_km,
                                        station_code = station_code)

      out[distance %in%
            out[(distance <= distance_km)]$distance]

      # Rename lat and lon for consistency and return data.table
      data.table::setnames(out, c(3, 4, 8),
                           c("latitude", "longitude", "distance"))
      data.table::setorderv(out, "distance")

      return(out[])

    } else if (which_api == "dpird" &
               isFALSE(grepl("^\\d+$", station_code))) {
      out <-
        .get_dpird_stations(
          .station_code = station_code,
          .distance_km = distance_km,
          .latitude = latitude,
          .longitude = longitude,
          .api_key = api_key
        )
      return(out[])

    } else if (which_api == "all") {
      if (isFALSE(grepl("^\\d+$", station_code))) {
        # return dpird
        out_dpird <-
          .get_dpird_stations(
            .station_code = station_code,
            .distance_km = distance_km,
            .latitude = latitude,
            .longitude = longitude,
            .api_key = api_key
          )

        this_coords <- out_dpird[1, .(latitude, longitude)]

        # return silo
        out_silo <- .find_nearby_silo_stations(
          distance_km = distance_km,
          longitude = this_coords[1, longitude],
          latitude = this_coords[1, latitude]
        )

        if (!is.null(out_dpird) & nrow(out_silo) != 0L) {
          out <- rbind(out_dpird, out_silo)
          data.table::setorderv(out, "distance")
          return(out[])

        } else if (is.null(out_dpird) & nrow(out_silo) != 0L) {
          return(out_silo[])

        } else if (!is.null(out_dpird) & nrow(out_silo) == 0L) {
          return(out_dpird[])
        }


      } else if (grepl("^\\d+$", station_code)) {
        # return silo
        out_silo <- .find_nearby_silo_stations(distance_km = distance_km,
                                               station_code = station_code)

        out_silo[, distance_km := round(distance_km, 1)]

        # return dpird
        this_coords <- out_silo[1, .(latitude, longitude)]

        out_dpird <-
          .get_dpird_stations(
            .latitude = this_coords[1, latitude],
            .longitude = this_coords[1, longitude],
            .distance_km = distance_km,
            .api_key = api_key
          )

        if (!is.null(out_dpird) & nrow(out_silo) != 0L) {
          out <- rbind(out_dpird, out_silo)
          data.table::setorderv(out, "distance")
          return(out[])

        } else if (is.null(out_dpird) & nrow(out_silo) != 0L) {
          return(out_silo[])

        } else if (!is.null(out_dpird) & nrow(out_silo) == 0L) {
          return(out_dpird[])
        }
      }
    }
  }
}

#' Find stations within a given radius of a geographic point or weather station
#'
#' Searches for stations available in the SILO database and returnes values
#' sorted from nearest to farthest.
#'
#' @param distance_km A `numeric` value for the distance in kilometres in which
#'  to search for nearby stations from a given geographic location or known
#'  `station_code`. Cannot be used when `longitude` and `latitude` are provided.
#' @param longitude A `numeric` value for longitude expressed as decimal degrees
#'  (DD) (WGS84). Required if `latitude` is used. Cannot be used if `station_code`
#'  is provided.
#' @param latitude A `numeric` value for latitude expressed as decimal degrees
#'   (DD) (WGS84). Required if `longitude` is used. Cannot be used if
#'   `station_code` is provided.
#' @param station_code A `character` value for a known station from which the
#' nearby stations are to be identified.
#'
#' @examples
#' # find stations within 50 km of Finke Post Office, NT
#' .find_nearby_silo_stations(distance_km = 50,
#'                          station_code = "015526")
#'
#' # find stations within 500 km of Esperance, WA
#' .find_nearby_silo_stations(distance_km = 50,
#'                           longitude = 121.8913,
#'                           latitude = -33.8614)
#'
#' @return A `data.table` of available stations and associated metadata.
#'
#' @noRd
.find_nearby_silo_stations <- function(distance_km = NULL,
                                       longitude = NULL,
                                       latitude = NULL,
                                       station_code = NULL) {

  if (!is.null(station_code) &&
      !is.null(longitude) && !is.null(latitude)) {
    stop(
      call. = FALSE,
      "You have provided `station_code` along with `longitude` and \n",
      "`latitude` values. Please provide only one, either `station_code` \n",
      "or a pairing of `longitude` and `latitude` values."
    )
  }
  if (!is.null(longitude) || !is.null(latitude)) {
    stopifnot("Provide both `latitude` and `longitude` values" =
                length(latitude) == length(longitude))
    .check_lonlat(longitude = longitude, latitude = latitude)
  }

  if (!is.null(station_code)) {
    x <-
      .get_silo_stations(.station_code = station_code,
                         .distance_km = distance_km)

    # Warn user if there are no stations within the input radius and return data
    if (nrow(x) == 0L)
      message(
        paste0(
          "No SILO stations found around a radius of < ",
          distance_km,
          " km\n",
          " from station ",
          station_code,
          ".\n"
        )
      )

    return(x)

  } else if (is.null(station_code)) {
    .latitude = latitude
    .longitude = longitude

    x <-
      .get_silo_stations(.station_code = NULL)

    x[, "distance" := round(
      .haversine_distance(
        lat1 = .latitude,
        lon1 = .longitude,
        lat2 = latitude,
        lon2 = longitude
      ),
      1
    )] |>
      data.table::setorderv("distance")

    x <-
      x[distance %in%
          x[(distance <= distance_km)]$distance]

    if (nrow(x) == 0L)
      message(
        paste0(
          "No SILO stations found around a radius of < ",
          distance_km,
          " km\n",
          " from coordinates ",
          latitude,
          " and ",
          longitude,
          " (lat/lon)\n"
        )
      )

    return(x)
  }
}

#' Create a data.table object of all stations available in DPIRD
#'
#' @param .station_code A string identifying a station in DPIRD's network, which
#'  should be used as the centre point to determine stations that fall within
#'  `.distance_km`
#' @param .distance_km A `numeric` value for the distance in kilometres in which
#'  to search for nearby stations from a given geographic location or known
#' `.station_code`.
#' @param .latitude A `numeric` value (Decimal Degrees) passed from another
#'  function
#' @param .longitude A `numeric` value (Decimal Degrees) passed from another
#'  function
#' @param .api_key A `string` value that is the user's \acronym{API} key from
#'  \acronym{DPIRD} (see <https://www.agric.wa.gov.au/web-apis>).
#'
#' @noRd
.get_dpird_stations <- function(.station_code,
                                .distance_km,
                                .latitude,
                                .longitude,
                                .api_key) {
  # Error if api key not provided
  if (is.null(.api_key)) {
    stop(
      call. = FALSE,
      "Provide a valid DPIRD API key.\n",
      "Visit: https://www.agric.wa.gov.au/web-apis"
    )
  }

  if (!missing(.station_code)) {
    base_url <-
      "https://api.dpird.wa.gov.au/v2/weather/stations?stationCode="

    res <- jsonlite::fromJSON(url(
      paste0(
        base_url,
        .station_code,
        "&api_key=",
        .api_key,
        "&limit=1&group=rtd"
      )
    ))

    .latlon <- .create_latlon(res)

    ret <- jsonlite::fromJSON(url(
      paste0(
        "https://api.dpird.wa.gov.au/v2/weather/stations/nearby?",
        "latitude=",
        .latlon[[1]],
        "&longitude=",
        .latlon[[2]],
        "&radius=",
        .distance_km,
        "&api_key=",
        .api_key,
        "&offset=0",
        "&limit=1000",
        "&group=rtd"
      )
    ))

    if (is.null(nrow(ret$collection))) {
      message(
        paste0(
          "No DPIRD stations found around a radius of < ",
          .distance_km,
          " km\n",
          " from station ",
          .station_code,
          "."
        )
      )

    } else {
      return(.parse_dpird_stations(ret))
    }

  } else if (missing(.station_code)) {
    # get nearby stations from weather api
    ret <- jsonlite::fromJSON(url(
      paste0(
        "https://api.dpird.wa.gov.au/v2/weather/stations/nearby?",
        "latitude=",
        .latitude,
        "&longitude=",
        .longitude,
        "&radius=",
        .distance_km,
        "&api_key=",
        .api_key,
        "&offset=0",
        "&limit=1000",
        "&group=rtd"
      )
    ))

    # Warn user if there are no stations within the input radius and return data
    if (is.null(nrow(ret$collection))) {
      message(
        paste0(
          "No DPIRD stations found around a radius of < ",
          .distance_km,
          " km\n",
          " from coordinates ",
          .latitude,
          " and ",
          .longitude,
          " (lat/lon)\n"
        )
      )
    } else {
      return(.parse_dpird_stations(ret))
    }
  }
}


#' Create a data.table object of all stations available in SILO
#'
#' @param .station_code A string identifying a BOM station in SILO which should
#' be used as the centre point to determine stations that fall within
#'  `.distance_km`
#' @param .distance_km A `numeric` value for the distance in kilometres in which
#'  to search for nearby stations from a given geographic location or known
#'  `.station_code`.
#'
#' Uses SILO's cgi-bin web interface to query and download a text file of
#' BOM weather stations in SILO within 2500 km of Finke, NT (near the Lambert
#' centre of Australia). Stations are returned in order of distance from the
#' Finke Post Office.
#'
#' @noRd
.get_silo_stations <- function(.station_code, .distance_km) {

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
  col_names = c("station_code",
                "station_name",
                "latitude",
                "longitude",
                "state",
                "elev_m",
                "distance")

  # if `.station_code` is not provided, fetch all stations in AU. Otherwise, use
  # the cgi-bin interface to find stations within `.distance_km` of the given
  # `.station_code`
  if (is.null(.station_code)) {
    r <- data.table::fread(
      paste0(base_url,
             "near&station=015526&radius=10000"),
      colClasses = col_types,
      col.names = col_names,
      verbose = FALSE
    )
  } else {
    r <- data.table::fread(
      paste0(
        base_url,
        "near&station=",
        .station_code,
        "&radius=",
        .distance_km
      ),
      colClasses = col_types,
      col.names = col_names,
      verbose = FALSE
    )
  }

  # Manipulate cols
  r[, station_name := .strcap(x = station_name)]
  r[, owner := "BOM"]
  r[, distance := round(distance, 1)]
  data.table::setkey(r, "station_code")
  data.table::setcolorder(r, c(1:5, 6, 8, 7))

  return(r[])
}


#' DPIRD API query parser for stations list
#' Takes results from an API query to the DPIRD Weather API and formats it to a
#' flat `data.table`. The function also converts character columns to date and
#' numerical classes, according to the data represented in the column.
#' @param query_result A list, with metadata and data collection elements.
#' @return A `data.table` with date class column(s) and numeric class columns
#' for the weather variables.
#' @keywords internal
#' @noRd

.parse_dpird_stations <- function(query_result = ret) {

  out <- data.table::data.table(query_result$collection)
  out <- .rename_cols(out, which_api = "dpird")

  out[, distance := round(distance, 1)]
  out[, links := NULL]
  out[, model := NULL]
  out[, owner := NULL]
  out[, start_date := NULL]
  out[, end_date := NULL]
  out[, comments := NULL]
  out[, job_number := NULL]
  out[, online := NULL]
  out[, status := NULL]

  # Filter out missing values in latitude and longitude
  out <-
    out[stats::complete.cases(out[, c("latitude",
                                      "longitude")])]

  # Reverse sign of latitude if it is positive
  out[, latitude := data.table::fifelse(out$latitude > 0,
                                        out$latitude * -1,
                                        out$latitude)]

  out[, state := "WA"]
  data.table::setcolorder(out, c(1:4, 8, 5, 6, 7))
  data.table::setnames(out, c(6, 7), c("elev_m", "owner"))
  out <- out[owner %notin% "DPIRDTST"]

  return(out)
}

#' Internal function to create a data.table
#'
#' @param ret a JSON object returned from the DPIRD API with station information
#' @noRd

.create_distance_out <- function(ret) {
  distance_out <- data.table::data.table(ret$collection)
  distance_out[, links := NULL]
  return(distance_out)
}

#' Internal function to create .lat and .lon objects
#'
#' @param res a json object from a DPIRD API
#' @return a vector of latitude and longitude values
#' @noRd

.create_latlon <- function(res) {
  list(.lat <- res$collection$latitude,
       .lon <- res$collection$longitude)
}
