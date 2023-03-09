# file: /R/get_station_list.R
#
# This file is part of the R-package wrapique
#
# Copyright (C) 2022 DPIRD
#	<https://www.dpird.wa.gov.au>

#' Fetch a list with all available weather stations for the DPIRD Weather API
#' or stations present in the SILO API.
#'
#' Queries \acronym{DPIRD} and \acronym{SILO} \acronym{API}s. If querying the
#' DPIRD Weather API, the function return a data frame with metadata for the
#' weather stations in the \acronym{DPIRD} network. Data providers include:
#' \acronym{DPIRD}, \acronym{DBCA}, \acronym{DFES}, Water Corporation, Harvey
#' Water and Pardoo Beef Corporation. If querying the \acronym{SILO}
#' \acronym{API}, the function also return a data frame with metadata for the
#' \acronym{SILO} weather stations network.
#'
#' @param api name of the \acronym{API} to be queried. Needs to match to
#' one of the existing options (currently 'weather' or 'silo'). Defaults to the
#' "weather" \acronym{API}.
#' @param api_key User's \acronym{API} key from \acronym{DPIRD}
#'  (<https://www.agric.wa.gov.au/web-apis>)
#' @param limit a numeric value, limiting the amount of rows to return. Defaults
#' to 1000.
#' @param state a string limiting the query to one or several states, defaults
#' to "wa". Accepts "all", "wa", "sa", "nsw", "vic", "qld", "tas", "nt".
#' @return a `data.table` with station code, name, latitude, longitude, owner
#' and state plus metadata details if any available.
#'
#' @examplesIf interactive()
#' # Query DPIRD API
#' # You must have an api key to query the DPIRD API
#' mykey <- "YOUR_API_KEY"
#'
#' dpird_stations <- get_station_list(api = "weather",
#'                                    state = "wa",
#'                                    api_key = mykey)
#'
#' # Query the SILO API
#' silo_stations <- get_station_list(api = "silo",
#'                                    state = "nsw",
#'                                    api_key = mykey)
#'
#' # Query the SILO API for multiple states
#' silo_stations_multi_state <- get_station_list(api = "silo",
#'                                               state = c("nsw", "nt"),
#'                                               api_key = mykey)
#'
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au}
#' @export

get_station_list <- function(api = "weather",
                             state = "wa",
                             limit = 1000,
                             api_key = NULL) {

  # CRAN NOTE avoidance:
  stations_site_list <-
    api_group <- latitude <- longitude <- links <- NULL #nocov

  # Set API
  which_api <- try(
    match.arg(tolower(api),
              c("weather", "silo"),
              several.ok = FALSE),
    silent = TRUE)

  # Query API
  if (which_api == "weather") {

    # Match request limits
    n_limit <- limit

    # Set stations group
    # Only 'rtd' for the DPIRD API
    api_group <- 'rtd'

    # Error if api key not provided
    if (is.null(api_key)) {
      stop("If you to provide a valid DPIRD API key.\n",
           "Visit: https://www.agric.wa.gov.au/web-apis",
           call. = FALSE)
    }

    # Match state query
    this_state <- try(
      match.arg(state,
                c("all", "wa", "sa", "nsw", "vic", "qld", "tas", "nt"),
                several.ok = FALSE),
      silent = TRUE)

    # Stop if station group is not on the list
    if (api_group != "rtd") {
      stop(call. = FALSE,
           "Science API only accepts 'rtd' as a station group")
    }

    if (this_state != "wa") {
      stop(call. = FALSE,
           "The Weather API has no data for stations outside WA.\n",
           "Visit: https://www.agric.wa.gov.au/web-apis")
    }

    ret <- url(paste0("https://api.dpird.wa.gov.au/v2/",
                      which_api,
                      "/stations.json?api_key=",
                      api_key,
                      "&limit=",
                      n_limit,
                      "&group=",
                      api_group,
                      "&state=",
                      this_state))

    out <- data.table::data.table(jsonlite::fromJSON(ret)$collection)

    out[, latitude := as.numeric(latitude)]
    out[, longitude := as.numeric(longitude)]
    out[, links := NULL]

    out <- .rename_cols(out, which_api = "dpird")
  }

  if (which_api == "silo") {

    # Match state query
    this_state <- try(
      match.arg(state,
                c("all", "wa", "sa", "nsw", "vic", "qld", "tas", "nt"),
                several.ok = TRUE),
      silent = TRUE)

    if (is.null(this_state)) {
      stop(call. = FALSE,
           "You must provide at least string to the `state` argument when\n
           querying the SILO API. One or more of `wa`, `sa`, `nsw`,\n
           `vic`, `qld`, `tas`, `nt` and `all`.")
    }

    # get file with bom station metadata
    load(
      system.file(
        "extdata",
        "stations_site_list.rda",
        package = "wrapique",
        mustWork = TRUE
      )
    )

    out_bom <- data.table::copy(stations_site_list)
    out_bom[, state := tolower(out_bom$state)]

    if (!is.null(this_state) & length(this_state) > 1) {
      out <- subset(out_bom, state %in% this_state)
    } else if (!is.null(this_state) &
               length(this_state) == 1 & this_state == "all") {
      out <- out_bom
    } else if (!is.null(this_state) &
               length(this_state) == 1 & this_state != "all") {
      out <- subset(out_bom, state == this_state)
    }
    out <- .rename_cols(out, which_api = "silo")
  }

  return(out[])
}
