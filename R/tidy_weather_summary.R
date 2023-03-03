#
# file: /R/tidy_weather_summary.R
#
# This file is part of the R-package wrapique
#
# Copyright (C) 2023 DPIRD
#	<https://www.dpird.wa.gov.au>

#' Individual station summaries nicely formatted.
#' @param site A string of the station ID code for the station of interest.
#' Passed through from `get_summaries()`
#' @param first A string representing the start date of the query in the
#' format 'yyyy-mm-dd'.
#' @param last A string representing the start date of the query in the
#' format 'yyyy-mm-dd'.
#' @param api_key Api key from DPIRD (https://www.agric.wa.gov.au/web-apis).
#' Defaults to NULL.
#' @param interval Time interval to summarise over.
#' Default is 'daily'; others are '15min', '30min', 'hourly',
#' 'monthly', 'yearly'.For intervals shorter than 1 day, time period covered
#' will be midnight to midnight, with the last time interval being before
#' midnight - hour/minute values are for the end of the time period.
#' Data for shorter intervals ('15min', '30min') are available from
#' January of the previous year.
#' @param api_name Defaults to "weather", Only works with DPIRD's Weather API.
#' @param api_version Defaults to 2, and gives a error if not 2.
#' @param which_vars Match weather summary selected. Defaults to "all".
#' Can be one of "all", "rain", "wind", "temp" and "erosion."
#'
#' @return a `data frame` with site and date interval queried together with
#'
#' @family DPIRD
#'
#' @examplesIf interactive()
#' # You must have an DPIRD API key to proceed
#' mykey <- 'dpird_api_key'
#'
#' # Set date interval for yearly request
#' # Get rainfall summary
#' start_date <- "2017-10-28"
#'
#' # Use default for end data (current system date)
#' output <- tidy_weather_summary(
#'             site = "CL001",
#'             first = start_date,
#'             api_key = mykey,
#'             interval = "yearly",
#'             which_vars = "rain")
#'
#' # Only for wind and erosion conditions for daily time interval
#' # define start and end date
#' start_date <- "2022-05-01"
#' end_date <- "2022-05-02"
#'
#' output <- tidy_weather_summary(
#'             site = "BI",
#'             first = start_date,
#'             last = end_date,
#'             api_key = mykey,
#'             interval = "daily",
#'             which_vars = c("wind", "erosion"))
#'
#' @export

tidy_weather_summary <- function(
    site = NULL,
    first,
    last = Sys.Date(),
    api_key = NULL,
    interval = c("daily", "15min", "30min", "hourly", "monthly", "yearly"),
    api_name = "weather",
    api_version = 2,
    which_vars = c("all", "rain", "wind", "temp", "erosion")) {

  # Error if not the Weather API or the latest version
  stopifnot(api_name == "weather", api_version == 2)

  # Function can only hand one station at time at this stage
  if (length(site) > 1)
    stop("Multiple stations not currently supported")

  # Get summaries based on user query
  df <-
    get_summaries(
      site = site,
      first = first,
      last = last,
      api_key = api_key,
      interval = interval
    )

  # Get query time interval
  out_period <- df$summaries$period

  # Remove empty columns (eg minute for hourly summaries) and grab number of
  # records in the data collection
  out_period <- out_period[, !apply(is.na(out_period), 2, all)]
  nrec <- nrow(out_period)

  # Airtemp
  if (any(c("all", "temp") %in% which_vars)) {
    out_temp <- df$summaries$airTemperature
    names(out_temp) <- paste0("airtemp.",
                              names(out_temp))

  } else {
    out_temp <- data.frame()[1:nrec, ]
  }

  # Rainfall
  if (any(c("all", "rain") %in% which_vars)) {
    out_rain <- df$summaries$rainfall

  } else {
    out_rain <- data.frame()[1:nrec, ]
  }

  # Wind
  if (any(c("all", "wind") %in% which_vars)) {
    temp <- df$summaries$wind
    temp <- lapply(temp, data.table::as.data.table)

    out_wind <- data.table::rbindlist(temp)
    names(out_wind) <- paste0("airtemp.",
                              names(out_wind))

  } else {
    out_wind <- data.frame()[1:nrec, ]
  }

  # Wind erosion
  if (any(c("all", "erosion") %in% which_vars)) {
    out_erosion <- df$summaries$erosionCondition
    names(out_erosion) <- paste0("wind.erosion.",
                                 names(out_erosion))

  } else {
    out_erosion <- data.frame()[1:nrec, ]
  }

  # Put together
  ret <- data.frame(site = df$stationCode,
                    out_period,
                    out_temp,
                    rain = out_rain,
                    out_wind,
                    out_erosion)

  names(ret) <- tolower(names(ret))
  return(ret)
}
