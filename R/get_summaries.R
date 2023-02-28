#
# file: /R/get_summaries.R
#
# This file is part of the R-package wrapique
#
# Copyright (C) 2021 DPIRD
#	<https://www.dpird.wa.gov.au>

#' Fetch weather summary from DPIRD weather API for an individual station
#'
#' @param site A string with the station ID code for the station of interest.
#' @param first The date on which the weather data summary will be sourced.
#' \pkg{wrapique} does its best to determine the date given any format but may
#' fail if given an unconventional date format.
#' @param last The last date for which the data will be sourced. For intervals
#' less than one day, to get one day of data, last should be the same as first,
#' but must be explicitly coded, as otherwise it will default to the current
#' date.
#' @param api_key \acronym{API} key from \acronym{DPIRD}
#'  \url{https://www.agric.wa.gov.au/web-apis}.  Defaults to `NULL`.
#' @param interval Time interval to summarise over.  The default is 'daily',
#' others are '15min', '30min', 'hourly', 'monthly', 'yearly'. For intervals
#' shorter than 1 day, time period covered will be midnight to midnight, with
#' the last time interval being before midnight–hour/minute values are for the
#' end of the time period. Data for shorter intervals ('15min', '30min') should
#' be available from January of last year.
#' @return a `list` with 3 elements: the station code, station name and a nested
#' `data frame` with the all summary output as per Weather \acronym{API}
#' documentation.
#'
#' @family DPIRD
#'
#' @examples
#' # You must have an DPIRD API key to proceed
#' mykey <- 'dpird_api_key'
#'
#' # set date interval for yearly request
#' start.date <- "2015-02-01"
#'
#' # Use default for end data (current system date)
#' output <- get_summaries(
#'            site = "AN001",
#'            first = start.date,
#'            api_key = mykey,
#'            interval = "yearly")
#'
#' # 15 min interval query, define start and end date
#' start.date <- "2022-05-01"
#' end.date <- "2022-05-02"
#'
#' output <- get_summaries(
#'            site = "BI",
#'            first = start.date,
#'            last = end.date,
#'            api_key = mykey,
#'            interval = "15min")
#'
#' @export

get_summaries <- function(site,
                          first,
                          last = Sys.Date(),
                          api_key = api_key,
                          interval = "daily") {
  if (missing(site))
    stop(call. = FALSE,
         "Station ID required.")

  if (missing(first))
    stop(call. = FALSE,
         "Please supply a start date.")

  # validate user provided date
  .check_date(first)
  .check_date(last)

  # Match time interval query to user requests
  m_int <- try(match.arg(interval,
                         c("daily",
                           "15min",
                           "30min",
                           "hourly",
                           "monthly",
                           "yearly"),
                         several.ok = FALSE),
               silent = TRUE)

  # Stop if query is for 15 and 30 min intervals and date is more than one
  # year in the past.
  if (m_int %in% c("15min", "30min") &&
      ((as.numeric(format(as.Date(first), "%Y"))) <
       (as.numeric(format(as.Date(last), "%Y")) - 1))) {
    stop(
      call. = FALSE,
      "Start date is too early. Data in 15 and 30 min intervals are only ",
      "available from the the 1st day of the previous year"
    )
  }

  # Stop if query is for monthly and interval is wrong
  if (m_int %in% c("monthly") &&
      (lubridate::interval(first, last, tz = "Australia/Perth")) < 0) {
    stop(call. = FALSE,
         "For monthly intervals date difference should be at least one month.")
  }

  # Stop if query is for daily and interval is wrong
  if (m_int %in% c("daily") &&
      (lubridate::interval(first, last, tz = "Australia/Perth")) < 0) {
    stop(call. = FALSE,
         "For daily intervals date difference should be at least one day.")
  }

  # Error if summary interval is not available. API only allows for daily,
  # 15 min, 30 min, hourly, monthly, yearly
  if (methods::is(m_int, "try-error"))
    stop(call. = FALSE,
         "\"", interval, "\" is not a supported time interval")

  # Helper message, tells which variable, date and stations are being queried
  message(
    "Requesting ",
    m_int,
    " data from",
    format(as.Date(first), "%e %B %Y"),
    " to ",
    format(as.Date(last), "%e %B %Y"),
    " for location code ",
    site,
    "\n"
  )

  # Create base query URL for weather summaries
  api <- paste0("https://api.dpird.wa.gov.au/v2/weather/stations/",
                site,
                "/summaries/",
                m_int)

  # Select correct time interval input
  uri <- switch(
    m_int,
    `15min` = paste0(
      api,
      "?startDateTime=",
      format(as.Date(first), "%Y-%m-%d"),
      "T00%3A15%3A00",
      "&endDateTime=",
      format(as.Date(last) + 1, "%Y-%m-%d"),
      "T00%3A00%3A00",
      "&limit=1000",
      "&api_key=",
      api_key
    ),
    `30min` = paste0(
      api,
      "?startDateTime=",
      format(as.Date(first), "%Y-%m-%d"),
      "T00%3A30%3A00",
      "&endDateTime=",
      format(as.Date(last) + 1, "%Y-%m-%d"),
      "T00%3A00%3A00",
      "&limit=1000",
      "&api_key=",
      api_key
    ),
    hourly = paste0(
      api,
      "?startDateTime=",
      format(as.Date(first), "%Y-%m-%d"),
      "T01%3A00%3A00",
      "&endDateTime=",
      format(as.Date(last) + 1, "%Y-%m-%d"),
      "T00%3A00%3A00",
      "&limit=1000",
      "&api_key=",
      api_key
    ),
    daily = paste0(
      api,
      "?startDate=",
      format(as.Date(first), "%Y-%m-%d"),
      "&endDate=",
      format(as.Date(last), "%Y-%m-%d"),
      "&api_key=",
      api_key,
      "&limit=",
      as.Date(last) - as.Date(first) + 1
    ),
    monthly = paste0(
      api,
      "?startMonth=",
      format(as.Date(first), "%Y-%m"),
      "&endMonth=",
      format(as.Date(last), "%Y-%m"),
      "&limit=",
      ceiling(as.double(
        difftime(
          format(as.Date(last), "%Y-%m-%d"),
          format(as.Date(first), "%Y-%m-%d"),
          units = "days"
        ) / 365
      ) * 12),
      "&api_key=",
      api_key
    ),
    yearly = paste0(
      api,
      "?startYear=",
      format(as.Date(first), "%Y"),
      "&endYear=",
      format(as.Date(last), "%Y"),
      "&offset=0",
      "&limit=100",
      "&api_key=",
      api_key
    )
  )

  # return only data collection; disregard metadata
  ret <- jsonlite::fromJSON(url(uri))$data
  return(ret)
}
