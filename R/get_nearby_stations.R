# file: /R/get_nearby_stations.R
#
# This file is part of the R-package wrapique
#
# Copyright (C) 2022 DPIRD
#	<https://www.dpird.wa.gov.au>

#' Get a list of nearby weather stations from coordinates or station ID code
#'
#' @param latitude Latitude expressed as decimal degrees (DD) (WGS84).
#' @param longitude Longitude expressed as decimal degrees (DD) (WGS84).
#' @param site A string with the station ID code for the station of interest.
#' Optional and defaults to `NULL`.
#' @param distance_km Distance to limit the search from station of interest.
#' Defaults to 100 km.
#' @param api_key User's \acronym{API} key from \acronym{DPIRD}
#'  (\url{https://www.agric.wa.gov.au/web-apis})
#' @param wa_only Return stations in Western Australia only. Defaults to `TRUE`.
#' @param dpird_only Return only \acronym{DPIRD} owned stations and defaults to
#'  `FALSE`.
#'
#' @return a `data.frame` with 'stationCode', 'stationName', 'latitude',
#' 'longitude', 'state', 'owner' and 'distance'. Data is sorted by increasing
#' distance from station of interest.
#'
#' @examples
#' # You must have an DPIRD API key to proceed
#' my_key <- rstudioapi::askForSecret()
#'
#' # Query WA only stations and return both DPIRD's and BOM's stations.
#' my_station <- "NO"    # Northam
#' my_distance <- 50    # in km
#'
#' wa_stn <- get_nearby_stations(
#'   site = my_station,
#'   distance_km = my_distance,
#'   api_key = my_key,
#'   dpird_only = FALSE,
#'   wa_only = TRUE
#' )
#'
#' # Query Wagga Wagga BOM station.
#' # Need to set `dpird_only` and `wa_only` to `FALSE`
#' lat <- -35.1583
#' long <- 147.4575
#'
#' wagga_stn <- get_nearby_stations(
#'   latitude = lat,
#'   longitude = long,
#'   distance_km = my_distance,
#'   api_key = mykey,
#'   dpird_only = FALSE,
#'   wa_only = FALSE
#' )
#'
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au}
#' @export


get_nearby_stations <- function(latitude = NULL,
                                longitude = NULL,
                                site = NULL,
                                distance_km = 100,
                                api_key = NULL,
                                wa_only = TRUE,
                                dpird_only = FALSE)
{

  # Error if api key not provided
  if (is.null(api_key)) {
    stop(call. = FALSE,
         "Provide a valid DPIRD API key.\n",
         "Visit: https://www.agric.wa.gov.au/web-apis")
  }

  if (isFALSE(wa_only) & isTRUE(dpird_only)) {
    stop(call. = FALSE,
         "DPIRD weather stations are only available in Western Australia.
         Cannot proceed if `wa_only = ", wa_only,
         " and `dpird_only` = ",
         dpird_only,".")
  }

  if (isTRUE(wa_only)) {
    if (!is.null(site)) {
      res <- jsonlite::fromJSON(
        url(paste0("https://api.dpird.wa.gov.au/v2/science/stations?",
                                           "stationCode=",
                                           site,
                                           "&api_key=",
                                           api_key,
                                           "&limit=100000&group=rtd")))

      .long <- res$collection$longitude
      .lat <- res$collection$latitude

      if ((is.null(.lat)) || (is.null(.long))) {
        stop(call. = FALSE,
             "Error collecting the latitude and longitude\n",
             "details for the station code provided.")
      }

      # get nearby stations from science api
      ret <- jsonlite::fromJSON(
        url(
          paste0("https://api.dpird.wa.gov.au/v2/science/stations/nearby?",
                                           "latitude=", .lat,
                                           "&longitude=", .long,
                                           "&radius=", distance_km,
                                           "&api_key=", api_key,
                                           "&offset=0",
                                           "&limit=100",
                                           "&group=rtd")))

      distance_out <- ret$collection
      distance_out$links <- NULL

    } else if (is.null(site)) {

      if (((is.null(latitude)) || (is.null(longitude))) && (is.null(site))) {
        stop(call. = FALSE,
             "Provide valid latitude and longitude\n",
             "coordinates or a valid station code.")
      }

      # get nearby stations from science api
      ret <- jsonlite::fromJSON(
        url(
          paste0("https://api.dpird.wa.gov.au/v2/science/stations/nearby?",
                                           "latitude=", .lat,
                                           "&longitude=", .long,
                                           "&radius=", distance_km,
                                           "&api_key=", api_key,
                                           "&offset=0",
                                           "&limit=100",
                                           "&group=rtd")))

      distance_out <- ret$collection
      distance_out$links <- NULL
    }

  } else if (isFALSE(wa_only)) {
    if (!is.null(site)) {
      res <- jsonlite::fromJSON(
        url(
          paste0("https://api.dpird.wa.gov.au/v2/science/stations?",
                                           "stationCode=",
                                           site,
                                           "&api_key=",
                                           api_key,
                                           "&limit=100000&group=yellowspot")))

      .long <- res$collection$longitude
      .lat <- res$collection$latitude

      if ((is.null(.lat)) || (is.null(.long))) {
        stop(call. = FALSE,
             "Error collecting the latitude and longitude\n",
             "details for the station code provided.")
      }

      # get nearby stations from science api
      ret <- jsonlite::fromJSON(
        url(
          paste0("https://api.dpird.wa.gov.au/v2/science/stations/nearby?",
                                           "latitude=", .lat,
                                           "&longitude=", .long,
                                           "&radius=", distance_km,
                                           "&api_key=", api_key,
                                           "&offset=0",
                                           "&limit=100",
                                           "&group=yellowspot")))

      distance_out <- ret$collection
      distance_out$links <- NULL

    } else if (is.null(site)) {

      if (((is.null(latitude)) || (is.null(longitude))) && (is.null(site))) {
        stop(call. = FALSE,
             "Provide valid latitude and longitude\n",
             "coordinates or a valid station code.")
      }

      # get nearby stations from science api
      ret <- jsonlite::fromJSON(
        url(
          paste0("https://api.dpird.wa.gov.au/v2/science/stations/nearby?",
                                           "latitude=", latitude,
                                           "&longitude=", longitude,
                                           "&radius=", distance_km,
                                           "&api_key=", api_key,
                                           "&offset=0",
                                           "&limit=100",
                                           "&group=yellowspot")))

      distance_out <- ret$collection
      distance_out$links <- NULL
    }
  }

  if (isTRUE(dpird_only)) {
    distance_out <- subset(distance_out, owner == "DPIRD")

  } else {
    distance_out <- dplyr::filter(distance_out, !is.na(latitude) & !is.na(longitude))
    distance_out <- dplyr::mutate(distance_out,
                                  stationName = tolower(stationName),
                                  latitude = dplyr::case_when(latitude > 0 ~ (latitude * -1),
                                                              TRUE ~ latitude))
  }
  return(distance_out)
}
