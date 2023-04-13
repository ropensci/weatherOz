# file: /R/get_nearby_stations.R
#
# This file is part of the R-package weatherOz
#
# Copyright (C) 2022 DPIRD
#	<https://www.dpird.wa.gov.au>

#' Get a list of nearby weather stations from coordinates or station ID code
#'
#' @param latitude A `numeric` value for latitude expressed as decimal degrees
#'   (DD) (WGS84).
#' @param longitude A `numeric` value for longitude expressed as decimal degrees
#'  (DD) (WGS84).
#' @param station_id A `string` with the station ID code for the station of
#' interest. Optional and defaults to `NULL`.
#' @param distance_km A `numeric` value for distance to limit the search from
#'  the station or location of interest.  Defaults to 100 km.
#' @param api_key A `string` value that is the user's \acronym{API} key from
#'  \acronym{DPIRD} (see <https://www.agric.wa.gov.au/web-apis>).
#' @param silo_stations `Boolean`, return SILO stations only. Defaults
#' to `TRUE`. If `FALSE`, return only \acronym{DPIRD} owned stations.
#'
#' @return a `data.table` with 'station_code', 'station_name', 'latitude',
#' 'longitude', 'elevation' and 'distance'. Data are sorted by increasing
#' distance from station or location of interest. SILO queries also return the
#' Australian State ('state') while DPIRD queries also returns 'owner', 'online'
#' and 'status'.
#'
#' @note You can request your own API key from DPIRD for free by filling out the
#' form found at <https://www.agric.wa.gov.au/web-apis>.
#'
#' @examplesIf interactive()
#' # You must have an DPIRD API key to proceed
#' my_key <- rstudioapi::askForSecret()
#'
#' # Query WA only stations and return both DPIRD's and BOM's stations for
#' # the Northam WA station, returning stations with 50 km of this station
#' wa_stn <- find_nearby_stations(
#'   station_id = "NO",
#'   distance_km = 50,
#'   api_key = "YOUR API KEY",
#'   silo_stations = FALSE
#' )
#'
#' # Query Wagga Wagga BOM station.
#' wagga_stn <- find_nearby_stations(
#'   latitude = -35.1583,
#'   longitude = 147.4575,
#'   distance_km = 200,
#'   api_key = "YOUR API KEY",
#'   silo_stations = FALSE
#' )
#'
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au}
#' @export

find_nearby_stations <- function(latitude = NULL,
                                 longitude = NULL,
                                 station_id = NULL,
                                 distance_km = 100,
                                 api_key = NULL,
                                 which_api = "silo") {
  # CRAN NOTE avoidance
  lat <- lon <- distance <- owner <- links <- NULL

  if (((is.null(latitude)) ||
       (is.null(longitude))) && (is.null(station_id))) {
    stop(
      call. = FALSE,
      "Provide valid latitude and longitude\n",
      "coordinates or a valid station code."
    )
  }

  if (which_api %in% c("silo", "dpird") & which_api != "all") {
    if (length(station_id) == 1 || length(latitude) == 1) {
      if (is.null(station_id)) {

        .check_lonlat(longitude = longitude, latitude = latitude)

        if (which_api == "silo") {
          # Get list of all stations in Australia
          # Calculate distance from lat/lon coordinates, sort and filter data to
          # distance threshold. Round distance in km to .0
          out <- find_nearby_silo_stations(distance_km = distance_km,
                                           longitude = longitude,
                                           latitude = latitude)
          out[ , distance := round(distance, 1)]

          # Warn user if there are no stations within the input radius and return data
          if (nrow(out) == 0L) message(
            paste0(
              "No SILO stations found around a radius of < ",
              distance_km, " km\n",
              " from coordinates ",
              latitude, " and ",
              longitude,
              " (lat/lon)"
            )
          )
          # Rename lat and lon for consistency and return data.table
          data.table::setnames(out, c(3, 4, 8),
                               c("latitude", "longitude", "distance"))

        }
        if (which_api == "dpird") {

          # Error if api key not provided
          if (is.null(api_key)) {
            stop(
              call. = FALSE,
              "Provide a valid DPIRD API key.\n",
              "Visit: https://www.agric.wa.gov.au/web-apis"
            )
          }

          # get nearby stations from weather api
          ret <- jsonlite::fromJSON(url(
            paste0(
              "https://api.dpird.wa.gov.au/v2/weather/stations/nearby?",
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

          # Warn user if there are no stations within the input radius and return data
          if (is.null(nrow(ret$collection))) {
            message(
              paste0(
                "No DPIRD stations found around a radius of < ",
                distance_km, " km\n",
                " from coordinates ",
                latitude, " and ",
                longitude,
                " (lat/lon)"
              )
            )
          } else {
            out <- .parse_dpird_stations(ret)
          }
        }
      }
      if (!is.null(station_id)) {
        if (which_api == "silo") {
          out <- find_nearby_silo_stations(distance_km = distance_km,
                                           station_id = station_id)

          if (nrow(out) == 0L) message(
            paste0(
              "No SILO stations found around a radius of < ",
              distance_km, " km\n",
              " from coordinates ",
              latitude, " and ",
              longitude,
              " (lat/lon)"
            )
          )

          out[ , distance_km := round(distance_km, 1)]
          out[distance %in%
                out[(distance <= distance_km)]$distance]

          # Rename lat and lon for consistency and return data.table
          # Rename lat and lon for consistency and return data.table
          data.table::setnames(out, c(3, 4, 8),
                               c("latitude", "longitude", "distance"))
          data.table::setorderv(out, "distance")
        }

        if (which_api == "dpird") {
          # Error if api key not provided
          if (is.null(api_key)) {
            stop(
              call. = FALSE,
              "Provide a valid DPIRD API key.\n",
              "Visit: https://www.agric.wa.gov.au/web-apis"
            )
          }

          res <- jsonlite::fromJSON(url(
            paste0(
              "https://api.dpird.wa.gov.au/v2/weather/stations?",
              "stationCode=",
              station_id,
              "&api_key=",
              api_key,
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
              distance_km,
              "&api_key=",
              api_key,
              "&offset=0",
              "&limit=1000",
              "&group=rtd"
            )
          ))
          if (is.null(nrow(ret$collection))) {
            message(
              paste0(
                "No DPIRD stations found around a radius of < ",
                distance_km, " km\n",
                " from coordinates ",
                latitude, " and ",
                longitude,
                " (lat/lon)"
              )
            )
          } else {
            out <- .parse_dpird_stations(ret)
          }
        }
      }
    }
    return(out[])
  }

  if (which_api == "all" & which_api %notin% c("silo", "dpird")) {
    if (!is.null(station_id)) {
      if (isTRUE(grepl("^\\d+$", station_id))) {

        out_silo <- find_nearby_silo_stations(distance_km = distance_km,
                                              station_id = station_id)

        if (nrow(out) == 0L) message(
          paste0(
            "No SILO stations found around a radius of < ",
            distance_km, " km\n",
            " from coordinates ",
            latitude, " and ",
            longitude,
            " (lat/lon)"
          )
        )

        out_silo[ , distance_km := round(distance_km, 1)]
        out_silo[distance %in%
                   out_silo[(distance <= distance_km)]$distance]

        # Rename lat and lon for consistency and return data.table
        # Rename lat and lon for consistency and return data.table
        data.table::setnames(out_silo, c(3, 4, 8),
                             c("latitude", "longitude", "distance"))
        data.table::setorderv(out_silo, "distance")

        # Error if api key not provided
        if (is.null(api_key)) {
          stop(
            call. = FALSE,
            "Provide a valid DPIRD API key.\n",
            "Visit: https://www.agric.wa.gov.au/web-apis"
          )
        }

        this_coords <- out_silo[1, .(latitude, longitude)]

        # get nearby stations from weather api
        ret <- jsonlite::fromJSON(url(
          paste0(
            "https://api.dpird.wa.gov.au/v2/weather/stations/nearby?",
            "latitude=",
            this_coords[1, latitude],
            "&longitude=",
            this_coords[1, longitude],
            "&radius=",
            distance_km,
            "&api_key=",
            api_key,
            "&offset=0",
            "&limit=1000",
            "&group=rtd"
          )
        ))
        if (is.null(nrow(ret$collection))) {
          message(
            paste0(
              "No DPIRD stations found around a radius of < ",
              distance_km, " km\n",
              " from coordinates ",
              latitude, " and ",
              longitude,
              " (lat/lon)"
            )
          )
        } else {
          out_dpird <- .parse_dpird_stations(ret)
        }
      }

      if (isFALSE(grepl("^\\d+$", station_id))) {
        res <- jsonlite::fromJSON(url(
          paste0(
            "https://api.dpird.wa.gov.au/v2/weather/stations?",
            "stationCode=",
            station_id,
            "&api_key=",
            api_key,
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
            distance_km,
            "&api_key=",
            api_key,
            "&offset=0",
            "&limit=1000",
            "&group=rtd"
          )
        ))
        if (is.null(nrow(ret$collection))) {
          message(
            paste0(
              "No DPIRD stations found around a radius of < ",
              distance_km, " km\n",
              " from coordinates ",
              latitude, " and ",
              longitude,
              " (lat/lon)"
            )
          )
        } else {
          out_dpird <- .parse_dpird_stations(ret)
        }

        # Now query silo
        out_silo <- find_nearby_silo_stations(distance_km = distance_km,
                                              longitude = .latlon[[2]],
                                              latitude = .latlon[[1]])

        if (nrow(out_silo) == 0L) message(
          paste0(
            "No SILO stations found around a radius of < ",
            distance_km, " km\n",
            " from coordinates ",
            latitude, " and ",
            longitude,
            " (lat/lon)"
          )
        )

        out_silo[ , distance := round(distance, 1)]
        data.table::setnames(out_silo, c(3, 4, 8),
                             c("latitude", "longitude", "distance"))
        data.table::setorderv(out_silo, "distance")
      }
      return(merge(out_dpird, out_silo))
    }

    if (is.null(station_id)) {
      ret <- jsonlite::fromJSON(url(
        paste0(
          "https://api.dpird.wa.gov.au/v2/weather/stations/nearby?",
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
      if (is.null(nrow(ret$collection))) {
        message(
          paste0(
            "No DPIRD stations found around a radius of < ",
            distance_km, " km\n",
            " from coordinates ",
            latitude, " and ",
            longitude,
            " (lat/lon)"
          )
        )
      } else {
        out_dpird <- .parse_dpird_stations(ret)
      }

      # Now query silo
      out_silo <- find_nearby_silo_stations(distance_km = distance_km,
                                            longitude = longitude,
                                            latitude = latitude)

      if (nrow(out_silo) == 0L) message(
        paste0(
          "No SILO stations found around a radius of < ",
          distance_km, " km\n",
          " from coordinates ",
          latitude, " and ",
          longitude,
          " (lat/lon)"
        )
      )

      out_silo[ , distance := round(distance, 1)]
      data.table::setnames(out_silo, c(3, 4, 8),
                           c("latitude", "longitude", "distance"))
      data.table::setorderv(out_silo, "distance")
    }
    return(merge(out_dpird, out_silo))
  }
}

#' SILO API query parser for stations list
#' Takes results from an API query to the SILO database and formats it to a
#' flat `data.table`. The function also converts character columns to date and
#' numerical classes, according to the data represented in the column.
#' @param .station_id A numeric value, the station code to be queried.
#' Defaults to Alice Springs, NT (15540).
#' @param .distance_km A numeric value, limiting the distance from the
#' coordinates/station location. User defined by the query details.
#' @param return_km A boolean, allows to the removal of the distance column
#' from the SILO API response. Defaults to `FALSE`.
#' @return A `data.table` with date class column(s) and numeric class columns
#' for the weather variables.
#' @keywords internal
#' @noRd

.parse_silo_stations <- function(.station_id = 15540,
                                 .distance_km = 10000,
                                 return_km = FALSE) {

  # Source all SILO stations in Australia
  base_url <- "https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?"
  query_params <- paste0("format=near&station=",
                         .station_id,
                         "&radius=",
                         .distance_km)
  query_url <- paste0(base_url, query_params)
  res <- readLines(query_url)

  # Split the character vector into lines
  x <- unlist(strsplit(res, "\n"))

  # Split each line at the "|" character and clean up the split data
  df <- lapply(x, function(row) {
    temp <- trimws(unlist(strsplit(row, "\\|")), which = "both")
    return(temp)
  })

  # Convert the split data into a dataframe
  out <- do.call(rbind, df)

  # Set the column names
  colnames(out) <- c("station_code", "station_name",
                     "lat", "lon",
                     "state", "elevation", "distance")

  # Set date columns to date class
  out <- data.table::data.table(out[-1, ])
  out[, distance := as.numeric(distance)]
  out[, lat := as.numeric(lat)]
  out[, lon := as.numeric(lon)]
  out[, elevation := as.numeric(elevation)]
  out[, station_name := .cap_names(s = station_name)]
  out[, owner := "BOM"]

  data.table::setcolorder(out, c(1:5, 7, 6))

  if (isFALSE(return_km)) {
    out[, distance := NULL]
  }

  return(out)
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

  # Manipulate date for consistency
  links <- NULL #nocov
  out <- data.table::data.table(query_result$collection)
  out <- .rename_cols(out, which_api = 'dpird')

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

  out <- out[owner_code %in% "DPIRD"]
  out[, state := "WA"]
  data.table::setcolorder(out, c(1:4, 8, 5, 6, 7))
  data.table::setnames(out, c(6, 7), c("elevation", "owner"))

  return(out)
}

#' Internal function to create a data.table
#'
#' @param ret a JSON object returned from the DPIRD API with station information
#' @noRd

.create_distance_out <- function(ret) {
  links <- NULL #nocov
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
