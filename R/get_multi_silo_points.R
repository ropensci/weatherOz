
# file: /R/get_multi_silo_points.R
#
# This file is part of the R-package wrapique
#
# Copyright (C) 2023 DPIRD
#	<https://www.dpird.wa.gov.au>

#' Retrieve data from SILO (Scientific Information for Land Owners) API for multiple sites
#'
#' @description This function is a wrapper around `get_silo_points()` to handle
#' queries with multiple sites, either station codes or latitude and longitude
#' coordinates. See help for `get_silo_points()` for details.
#'
#' @inheritParams get_silo_points
#'
#' @return A `list` object of `data.frames` with the retrieved data. Each list
#'  item is a `data.frame` with data for a given station code or geographic
#'  coordinates. New columns are added in both cases, `station_id` or `latitude`
#'  and `longitude`, respectively.
#'
#' @family SILO
#'
#' @examplesIf interactive()
#' # start multisession
#' future::plan("multisession")
#' # Query multiple stations (BoM, by code)
#' my_stations <- c(65030, 89033, 39083, 8061)
#' wd <- get_multi_silo_points(station_id = my_stations,
#'                             first = "20220401",
#'                             last = "20221001",
#'                             data_format = "monthly",
#'                             email = "YOUR_EMAIL_ADDRESS")
#'
#' # Query multiple stations (using latitude and longitude coordinates)
#' mylat <- c(-33.512, -30.665, -27.309, -24.237)
#' mylon <- c(115.821, 117.487, 119.649, 121.584)
#' wd <- get_multi_silo_points(latitude = mylat,
#'                             longitude = mylon,
#'                             first = "202101201",
#'                             last = "20220228",
#'                             data_format = "alldata",
#'                             email = "YOUR_EMAIL_ADDRESS")
#' @export

get_multi_silo_points <- function(station_id = NULL,
                                  latitude = NULL,
                                  longitude = NULL,
                                  first = NULL,
                                  last = NULL,
                                  data_format = "standard",
                                  email = NULL) {

  .check_lonlat(longitude = longitude, latitude = latitude)

  if (is.null(latitude) & is.null(longitude) && !is.null(station_id)) {

    weather_raw <- furrr::future_map(
      .x = station_id,
      .f = purrr::possibly(
        ~ get_silo_points(
          station_id = .x,
          latitude = latitude,
          longitude = longitude,
          first = first,
          last = last,
          data_format = data_format,
          email = email),
        otherwise = "Error in acquiring weather data for this station.\n"
      ),
      .options = furrr::furrr_options(seed = NULL)
    )

    # add the location to the weather data
    names(weather_raw) <- station_id
    out <- Map(cbind,
               station_id = names(weather_raw),
               weather_raw)
  }

  if (is.null(station_id) && !is.null(latitude) & !is.null(longitude)) {

    weather_raw <- furrr::future_map2(
      .x = latitude,
      .y = longitude,
      .f = purrr::possibly(
        ~ get_silo_points(
          latitude = .x,
          longitude = .y,
          first = first,
          last = last,
          data_format = data_format,
          email = email),
        otherwise = "Error in acquiring weather data for this station.\n"
      ),
      .options = furrr::furrr_options(seed = NULL)
    )

    # provide unique names
    names(weather_raw) <- paste0(unique(latitude), "_", unique(longitude))

    # add the location to the weather data
    weather_raw <- Map(cbind,
                       latlon = names(weather_raw),
                       weather_raw)

    out <-
      lapply(weather_raw, function(i) {
        latlon_sep <- strsplit(i$latlon, "_")[[1]]
        i$latitude <- latlon_sep[1]
        i$longitude <- latlon_sep[2]
        i$latlon <- NULL
        return(i)
      })
  }
  return(out)
}
