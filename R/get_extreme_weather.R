# file: /R/get_extreme_weather.R
#
# This file is part of the R-package weatherOz
#
# Copyright (C) 2022 DPIRD
#	<https://www.dpird.wa.gov.au>

#' Get extreme weather event summaries for a single DPIRD station
#'
#' @param station_id A string with the station ID code for the station of interest.
#' @param type A string with the type of extreme weather to return. Defaults to
#' "all"; and can be combination of "frost", "erosion", "heat", or "all".
#' @param api_key User's \acronym{API} key from \acronym{DPIRD}
#'  (<https://www.agric.wa.gov.au/web-apis>)
#'
#' @return a `data.table` of one row with 'station_code', 'station_name',
#' 'latitude', 'longitude', 'dateTime' of the query and the extreme weather
#' information according to the type(s) selected.
#'
#' @family DPIRD
#'
#' @examplesIf interactive()
#' # You must have an DPIRD API key to proceed
#' my_key <- rstudioapi::askForSecret()
#'
#' # Query Bonnie Rock station for wind erosion and heat extreme events.
#' my_station <- "BR"
#' my_events <- c("erosion", "heat")
#'
#' output <- get_extreme_weather(
#'   station_id = my_station,
#'   type = my_events,
#'   api_key = my_key
#' )
#'
#' # Query multiple stations for all extreme events
#' # Provide a list of station (as strings)
#' these_stations <- list("MN", "ES", "KARI", "NO", "KA002", "CO001", "MA002")
#'
#' # Row bind output lists (one for each station)
#' # together with `data.table::rbindlist`
#' outputs <- lapply(these_stations,
#'                   get_extreme_weather,
#'                   type = "all",
#'                   api_key = my_key)
#'
#' df <- data.table::rbindlist(outputs)
#'
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au}
#' @export

get_extreme_weather <- function(station_id,
                                type = "all",
                                api_key = NULL)
{
  if (missing(station_id)) {
    stop(
      call. = FALSE,
      "Provide a station ID via the `site` argument. It should take a string
         e.g., `AN001` for Allanooka station."
    )
  }

  type <- try(match.arg(
    type,
    choices = c("frost", "erosion", "heat", "all"),
    several.ok = TRUE
  ),
  silent = TRUE)

  if (length(station_id) != 1) {
    stop(call. = FALSE,
         "Wrong number of sites.\n",
         "This function only handles one site per query.")
  }

  httr::set_config(httr::config(ssl_verifypeer = 0L))
  api <-
    paste0(
      "https://api.dpird.wa.gov.au/v2/weather/stations/extreme-conditions?",
      "stationCode=",
      station_id,
      "&offset=0",
      "&limit=1",
      "&group=all",
      "&includeClosed=true",
      "&api_key=",
      api_key
    )

  api_data <- jsonlite::fromJSON(url(api))
  out_data <- api_data$collection[, c("stationCode",
                                      "stationName",
                                      "latitude",
                                      "longitude",
                                      "dateTime")]
  nrec <- nrow(out_data)

  if (any(c("erosion", "all") %in% type)) {
    erosion <- api_data$collection$erosionCondition
    out_erosion <- data.frame(
      erosion$since12AM$minutes,
      erosion$since12AM$startTime,
      erosion$last7Days$minutes,
      erosion$last7Days$days,
      erosion$last14Days$minutes,
      erosion$last14Days$days,
      erosion$monthToDate$minutes,
      erosion$monthToDate$startTime,
      erosion$monthToDate$days,
      erosion$yearToDate$minutes,
      erosion$yearToDate$startTime,
      erosion$yearToDate$days
    )

    names(out_erosion) <- .strcap(x = names(out_erosion))

  } else {
    out_erosion <- data.table::data.table()[1:nrec,]
  }

  if (any(c("frost", "all") %in% type)) {
    frost <- api_data$collection$frostCondition
    out_frost <- data.frame(
      frost$since9AM$minutes,
      frost$since9AM$startTime,
      frost$to9AM$minutes,
      frost$to9AM$startTime,
      frost$last7Days$minutes,
      frost$last7Days$days,
      frost$last14Days$minutes,
      frost$last14Days$days,
      frost$monthToDate$minutes,
      frost$monthToDate$startTime,
      frost$monthToDate$days,
      frost$yearToDate$minutes,
      frost$yearToDate$startTime,
      frost$yearToDate$days
    )

    names(out_frost) <- .strcap(x = names(out_frost))

  } else {
    out_frost <- data.table::data.table()[1:nrec,]
  }

  if (any(c("heat", "all") %in% type)) {
    heat <- api_data$collection$heatCondition
    out_heat <- data.frame(
      heat$since12AM$minutes,
      heat$since12AM$startTime,
      heat$last7Days$minutes,
      heat$last7Days$days,
      heat$last14Days$minutes,
      heat$last14Days$days,
      heat$monthToDate$minutes,
      heat$monthToDate$startTime,
      heat$monthToDate$days,
      heat$yearToDate$minutes,
      heat$yearToDate$startTime,
      heat$yearToDate$days
    )

    names(out_heat) <- .strcap(x = names(out_heat))

  } else {
    out_heat <- data.table::data.table()[1:nrec,]
  }

  # return final data
  out <- data.table::data.table(out_data, out_erosion, out_frost, out_heat)
  out <- .rename_cols(out)
  names(out) <- gsub("[.]", "_", names(out))

  return(data.table::setDT(out))
}
