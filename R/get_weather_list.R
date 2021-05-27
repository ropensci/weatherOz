#
# file: /R/get_weather_as_list.R
#
# This file is part of the R-package blackleg.sporacle
#
# Copyright (C) 2021 DPIRD
#	<https://www.dpird.wa.gov.au>
#
# NOT TO BE USED WITHOUT PRIOR PERMISSION

#' Fetch a list of weather station data from DPIRD Science API
#'
#' A wrapper function for [FoliarDisease::weather.data] to handle fetching
#' data for several stations specified in a `list` object from the
#' \acronym{DPIRD} Science \acronym{API}, see **Stations** section for further
#' detail.  Weather data returned will always start on Jan. 1 of the current
#' year and carry to the current day or yesterday dependent on data availability
#' in the database.
#'
#' @inheritParams weather.data
#'
#' @section Stations:
#' \pkg{blackleg.sporacle} contains a list of possible weather stations in the
#' \acronym{DPIRD} \acronym{API} that can be used, [blackleg_stations].  The
#' [select_default_stations()] will select only stations that are designated
#' with a "Y" in the `use_this` field. See the `data-raw` folder for more
#' details on the provenance of this data object.
#'
#' @examples
#' w <- get_weather_list()
#'
#' @return A `list` object of data frames containing the following columns:
#'  * location (site),
#'  * date,
#'  * rainfall (rain),
#'  * maximum temperature (max_temp) and
#'  * minimum temperature (min_temp).
#'
#' @author Adam Sparks, adam.sparks@@dpird.wa.gov.au
#' @export

get_weather_list <- function(stations,
                             first,
                             last = Sys.Date(),
                             data.type,
                             api.key,
                             verbose = FALSE) {
  weather_raw <- furrr::future_map(
    .x = stations,
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

  names(weather_raw) <- stations$station_name
  weather_raw <- Map(cbind, location = names(weather_raw), weather_raw)
  colnames <- c("location", "date", "rain", "max_temp", "min_temp")
  weather_raw <- lapply(weather_raw, data.table::setnames, colnames)

  return(weather_raw)
}
