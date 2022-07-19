#
# file: /R/get_station_list.R
#
# This file is part of the R-package wrapique
#
# Copyright (C) 2022 DPIRD
#	<https://www.dpird.wa.gov.au>

#' Fetch a list with all available weather stations for either `Science` and/or
#'`Weather` \acronym{DPIRD} \acronym{APIs}.
#'
#' Providers include: \acronym{DPIRD}, \acronym{DBCA}, \acronym{DFES},
#' Water Corporation, Harvey Water and Pardoo Beef Corporation.
#' @param which_api name of the \acronym{API} to be queried. Needs to match to
#' one of the existing options (currently 'science' or 'weather')
#' @param api_key User's \acronym{API} key from \acronym{DPIRD}
#'  (\url{https://www.agric.wa.gov.au/web-apis})
#' @param station_group
#' @return a `data.frame` with station code, name, longitude, longitude, owner
#' and state (for the Science API).
#' Science \acronym{API} queries return additional details by default
#' *i.e.*, station model, battery voltage, height of sensors, status and others.
#'
#' @examples
#' # You must have an DPIRD API key to proceed
#' mykey <- rstudioapi::askForSecret()
#'
#' stations_weather <- get_station_list(which_api = "weather",
#' station_ group = "rtd",
#' state = "all",
#' api_key = mykey)
#'
#' stations_science <- get_station_list(which_api = "science",
#' station_ group = "yellowspot",
#' state = "nsw",
#' api_key = mykey)
#'
#' @author Rodrigo Pires, rodrigo.pires@@dpird.wa.gov.au
#' @export

get_station_list <- function(api = c("science", "weather"),
                             station_group = c("all", 'api' ,'rtd', 'web', "yshistory", "yellowspot"),
                             state = c("all", "wa", "sa", "nsw", "vic", "qld", "tas", "nt"),
                             api_key = NULL) {

  # Error if api key not provided
  if (is.null(api_key)) {
    stop("If you to provide a valid DPIRD API key. Visit: https://www.agric.wa.gov.au/web-apis")
  }

  # Set API
  which_api <- try(
    match.arg(tolower(api),
              c("science", "weather"),
              several.ok = FALSE),
    silent = TRUE)

  # Match request limits
  n_limit <- switch(which_api, science = "100000", weather = "0")

  # Set stations group
  # science only accepts 'rtd'
  # weather accepts all but all returned outputs are identical)
  api_group <- try(
    match.arg(station_group,
              c("all", "api" ,"rtd", "web", "yshistory", "yellowspot"),
              several.ok = FALSE),
    silent = TRUE)

  # Match state query
  this_state <- try(
    match.arg(state,
              c("all", "wa", "sa", "nsw", "vic", "qld", "tas", "nt"),
              several.ok = FALSE),
    silent = TRUE)

  # Query API
  if (which_api == 'science') {

    # Stop if station group is not on the list
     if (!api_group %in% c("all", "rtd", "yellowspot", "yshistory")) {
       stop(call. = FALSE,
         "Science API only accepts 'all', 'rtd', 'yellowspot'\n",
            "and 'yshistory' as station groups")
     }

    if (this_state == "nt") {
    stop(call. = FALSE,
         "The Science API has no data for NT stations.\n",
         "Visit: https://www.agric.wa.gov.au/web-apis")
    }

    g <- url(paste0("https://api.dpird.wa.gov.au/v2/",
                    which_api,
                    "/stations.json?api_key=",
                    api_key,
                    "&limit=",
                    n_limit,
                    "&group=",
                    api_group,
                    "&state=",
                    this_state))
  } else {
    if (length(this_state != 0)) {
      message("Weather API only returns queries for WA. Disregarding state selection.")
    }
    g <- url(paste0("https://api.dpird.wa.gov.au/v2/",
                    which_api,
                    "/stations?api_key=",
                    api_key,
                    "&limit=",
                    n_limit,
                    "&group=",
                    api_group))

  }

  res <- jsonlite::fromJSON(g)

  stations <- res$collection

  stations$latitude <- as.numeric(stations$latitude)
  stations$longitude <- as.numeric(stations$longitude)

  stations$stationName <- tolower(stations$stationName)
  stations$links <- NULL
  names(stations) <- tolower(names(stations))

  return(stations)
}
