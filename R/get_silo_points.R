# file: /R/get_silo_points.R
#
# This file is part of the R-package wrapique
#
# Copyright (C) 2023 DPIRD
#	<https://www.dpird.wa.gov.au>

#' @title Retrieve data from SILO (Scientifc Information for Land Owners) API
#' @description Download weather data from the SILO API from both station
#' observations (DataDrill) and gridded data (PatchedPointData). There are
#' three formats available: 'alldata' and 'apsim' with daily frequency and
#' 'monthly' with, that's right, monthly frequency.
#' @param station_id An integer, station number available at the SILO
#' the network.
#' @param latitude A vector, represeting the latitude of a point-of-interest
#' @param longitude A vector, represeting the longitude of a point-of-interest
#' @param first A string representing the start date of the query in the
#' format 'yyyymmdd'
#' @param last A string representing the end date of the query in the
#' format 'yyyymmdd'
#' @param data_format A string specifying the type of data to retrieve.
#' Limited to 'alldata', 'monthly' or 'apsim'. Note 'apsim' and 'alldata'
#' retrieve daily data.
#' @param email A string specifying the email address to use for the request.
#' The query will return an error if a valid email address is not provided.
#' @return A dataframe containing the retrieved data from the SILO API.
#' @export
#' @examples
#' # Source observation data for station Wongan Hills station, WA (8137)
#' wd <- get_silo_points(station_id = 8137,
#'                       first = "20210601",
#'                       last = "20210701",
#'                       data_type = "alldata",
#'                       email = "your@@email")
#'
#' # Source data from latitude and longitude coordinates (gridded data)
#' # Southwood, WLD in the 'apsim' format.
#' wd <-
#' get_silo_points(latitude = -27.85,
#'                 longitude = 150.05,
#'                 first = "20221001",
#'                 last = "20221201",
#'                 data_format = "apsim",
#'                 email = "your@@email")

get_silo_points <- function(station_id = NULL,
                            latitude = NULL,
                            longitude = NULL,
                            first = NULL,
                            last = NULL,
                            data_format = "alldata",
                            email = NULL)
{

  if (missing(first)) stop("Provide a start date", call. = FALSE)
  if (missing(last)) stop("Provide an end date", call. = FALSE)
  if (is.null(email)) stop("Provide a valid email address", call. = FALSE)

  # Retrieve data for queries with lat lon coordinates
  if (is.null(station_id) && !is.null(latitude) & !is.null(longitude)) {

    # Build query
    query_params <- list(lat = latitude, lon = longitude,
                         start = first, finish = last,
                         format = data_format,
                         username = email,
                         password = "api_request")

    # Query API and store output
    result <- httr::GET(
      url =  "https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php",
      query = query_params
    )
  }

  # Retrieve data for queries with station code
  if (is.null(latitude) & is.null(longitude) && !is.null(station_id)) {

    # Build query
    query_params <- list(station = station_id,
                         start = first, finish = last,
                         format = data_format,
                         username = email, password = "api_request")

    # Create query and store output
    result <- httr::GET(
      url =  "https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php",
      query = query_params
    )
  }

  # Extract content and parse data according to the format and frequency
  r <- httr::content(result, "text")
  out <- wrapique::parse_silo(r, data_format, first)
  return(out)
}
