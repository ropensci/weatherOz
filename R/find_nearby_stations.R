# file: /R/get_nearby_stations.R
#
# This file is part of the R-package wrapique
#
# Copyright (C) 2022 DPIRD
#	<https://www.dpird.wa.gov.au>

#' Get a list of nearby weather stations from coordinates or station ID code
#'
#' @param latitude A `numeric` value for latitude expressed as decimal degrees
#'   (DD) (WGS84).
#' @param longitude A `numeric` value for longitude expressed as decimal degrees
#'  (DD) (WGS84).
#' @param site A `string` with the station ID code for the station of interest.
#' Optional and defaults to `NULL`.
#' @param distance_km A `numeric` value for distance to limit the search from
#'  the station or location of interest.  Defaults to 100 km.
#' @param api_key A `string` value that is the user's \acronym{API} key from
#'  \acronym{DPIRD} (see \url{https://www.agric.wa.gov.au/web-apis}).
#' @param wa_only `Boolean`, return stations in Western Australia only. Defaults
#'  to `TRUE`.
#' @param dpird_only `Boolean`, return only \acronym{DPIRD} owned stations.
#'   Defaults to `FALSE`.
#'
#' @return a `data.table` with 'stationCode', 'stationName', 'latitude',
#' 'longitude', 'state', 'owner' and 'distance'. Data are sorted by increasing
#' distance from station or location of interest.
#'
#' @examples
#' # You must have an DPIRD API key to proceed
#' my_key <- rstudioapi::askForSecret()
#'
#' # Query WA only stations and return both DPIRD's and BOM's stations for
#' # the Northam WA station, returning stations with 50 km of this station
#' wa_stn <- find_nearby_stations(
#'   site = "NO",
#'   distance_km = 50,
#'   api_key = "YOUR API KEY",
#'   dpird_only = FALSE,
#'   wa_only = TRUE
#' )
#'
#' # Query Wagga Wagga BOM station.
#' wagga_stn <- find_nearby_stations(
#'   latitude = -35.1583,
#'   longitude = 147.4575,
#'   distance_km = my_distance,
#'   api_key = "YOUR API KEY",
#'   dpird_only = FALSE,
#'   wa_only = FALSE
#' )
#'
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au}
#' @export


find_nearby_stations <- function(latitude = NULL,
                                 longitude = NULL,
                                 site = NULL,
                                 distance_km = 100,
                                 api_key = NULL,
                                 wa_only = TRUE,
                                 dpird_only = FALSE) {
  # CRAN NOTE avoidance
  owner <- links <- NULL

  .check_lonlat(longitude = longitude, latitude = latitude)

  # Error if api key not provided
  if (is.null(api_key)) {
    stop(
      call. = FALSE,
      "Provide a valid DPIRD API key.\n",
      "Visit: https://www.agric.wa.gov.au/web-apis"
    )
  }

  if (isFALSE(wa_only) & dpird_only) {
    stop(
      call. = FALSE,
      "DPIRD weather stations are only available in Western Australia.
         Cannot proceed if `wa_only` = ",
      wa_only,
      " and `dpird_only` = ",
      dpird_only,
      "."
    )
  }

  if (wa_only) {
    if (!is.null(site)) {
      res <- jsonlite::fromJSON(url(
        paste0(
          "https://api.dpird.wa.gov.au/v2/science/stations?",
          "stationCode=",
          site,
          "&api_key=",
          api_key,
          "&limit=100000&group=rtd"
        )
      ))

      .latlon <- .create_latlon(res)

      ret <- jsonlite::fromJSON(url(
        paste0(
          "https://api.dpird.wa.gov.au/v2/science/stations/nearby?",
          "latitude=",
          .latlon[[1]],
          "&longitude=",
          .latlon[[2]],
          "&radius=",
          distance_km,
          "&api_key=",
          api_key,
          "&offset=0",
          "&limit=1000",
          "&group=rtd"
        )
      ))

      distance_out <- .create_distance_out(ret)

    } else {
      if (((is.null(latitude)) ||
           (is.null(longitude))) && (is.null(site))) {
        stop(
          call. = FALSE,
          "Provide valid latitude and longitude\n",
          "coordinates or a valid station code."
        )
      }

      # get nearby stations from science api
      ret <- jsonlite::fromJSON(url(
        paste0(
          "https://api.dpird.wa.gov.au/v2/science/stations/nearby?",
          "latitude=",
          latitude,
          "&longitude=",
          longitude,
          "&radius=",
          distance_km,
          "&api_key=",
          api_key,
          "&offset=0",
          "&limit=1000",
          "&group=rtd"
        )
      ))

      distance_out <- .create_distance_out(ret)
    }
  }

  if (isFALSE(wa_only)) {
    if (!is.null(site)) {
      res <- jsonlite::fromJSON(url(
        paste0(
          "https://api.dpird.wa.gov.au/v2/science/stations?",
          "stationCode=",
          site,
          "&api_key=",
          api_key,
          "&limit=100000&group=yellowspot"
        )
      ))

      .latlon <- .create_latlon(res)

      ret <- jsonlite::fromJSON(url(
        paste0(
          "https://api.dpird.wa.gov.au/v2/science/stations/nearby?",
          "latitude=",
          .latlon[[1]],
          "&longitude=",
          .latlon[[2]],
          "&radius=",
          distance_km,
          "&api_key=",
          api_key,
          "&offset=0",
          "&limit=100000&group=yellowspot"
        )
      ))

      distance_out <- .create_distance_out(ret)

    } else {
      if (((is.null(latitude)) ||
           (is.null(longitude))) && (is.null(site))) {
        stop(
          call. = FALSE,
          "Provide valid latitude and longitude\n",
          "coordinates or a valid station code."
        )
      }

      # get nearby stations from science api
      ret <- jsonlite::fromJSON(url(
        paste0(
          "https://api.dpird.wa.gov.au/v2/science/stations/nearby?",
          "latitude=",
          latitude,
          "&longitude=",
          longitude,
          "&radius=",
          distance_km,
          "&api_key=",
          api_key,
          "&offset=0",
          "&limit=10000",
          "&group=yellowspot"
        )
      ))

      distance_out <- .create_distance_out(ret)
    }
  }

  if (dpird_only) {
    distance_out <- distance_out[owner %in% "DPIRD"]

  } else {
    # Filter out missing values in latitude and longitude
    distance_out <-
      distance_out[stats::complete.cases(distance_out[, c("latitude",
                                                          "longitude")])]

    # Reverse sign of latitude if it is positive
    distance_out[, latitude := data.table::fifelse(distance_out$latitude > 0,
                                                   distance_out$latitude * -1,
                                                   distance_out$latitude)]
  }
  distance_out <- .rename_cols(distance_out)
  return(distance_out[])
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
  .lat <- res$collection$latitude
  .lon <- res$collection$longitude
}
