#
# file: /R/get_weather_as_list.R
#
# This file is part of the R-package wrapique
#
# Copyright (C) 2021 DPIRD
#	<https://www.dpird.wa.gov.au>

#' Fetch a list of weather station data from DPIRD Science API
#'
#' A wrapper function for [weather_data] to handle fetching data for several
#' stations specified in a `list` object from the \acronym{DPIRD} Science
#' \acronym{API} in parallel, see the **Stations** section for further detail.
#' Weather data returned will always start on Jan. 1 of the current year and
#' carry to the current day or yesterday dependent on data availability in the
#' database.
#'
#' @param sites A character vector or list of station ID codes from the
#'  \acronym{DPIRD} Science \acronym{API} for which data should be fetched.
#' @param first The date on which the weather data should start.
#' @param last The last date on which the weather data should end.
#' @param data.type ...
#' @param api.key ...
#' @param verbose ... Defaults to `FALSE`.
#'
#' @examples
#' # set up multisession environment
#' future::plan("multisession")
#'
#' w <- get_weather_list()
#'
#' # revert to sequential processing
#' future::plan("sequential")
#'
#' @return A `list` object of data frames containing the following columns:
#'  * date,
#'  * rainfall (rain),
#'  * maximum temperature (max_temp),
#'  * minimum temperature (min_temp) and
#'  * \acronym{DPIRD}'s Science \acronym{API} 2.0 station code.
#'
#' @author Adam Sparks, \email{adam.sparks@@dpird.wa.gov.au}
#' @export

get_weather_list <- function(sites,
                             first,
                             last = Sys.Date(),
                             data.type,
                             api.key,
                             verbose = FALSE) {
  weather_raw <- furrr::future_map(
    .x = sites,
    .f = purrr::possibly(
      ~ FoliarDisease::weather.data(
        site = .x,
        first = first,
        last = last,
        data.type = data.type, # for BoM sites, this means use mirror
        api.key = api.key,
        verbose = verbose
      ),
      otherwise = "Error in acquiring weather data for this station.\n"
    ),
    .options = furrr::furrr_options(seed = NULL)
  )

  names(weather_raw) <- sites

  # add the location to the weather data
  weather_raw <- Map(cbind, site = names(weather_raw), weather_raw)

  return(weather_raw)
}
