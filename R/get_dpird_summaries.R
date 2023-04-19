#
# file: /R/get_dpird_summaries.R
#
# This file is part of the R-package weatherOz
#
# Copyright (C) 2023 DPIRD
#	<https://www.dpird.wa.gov.au>

#' Get weather data from DPIRD weather API summarised by time interval
#'
#' Nicely formatted individual station weather summaries from the DPIRD
#' weather station network.
#'
#' @param station_id A string of the station ID code for the station of
#' interest. Defaults to `NULL`.
#' @param first A string representing the start date of the query in the
#' format 'yyyymmdd'. Defaults to `NULL`.
#' @param last A string representing the start date of the query in the
#' format 'yyyymmdd'. Defaults to the current system date.
#' @param api_key Api key from DPIRD (https://www.agric.wa.gov.au/web-apis).
#' Defaults to `NULL`.
#' @param interval Time interval to summarise over.
#' Default is 'daily'; others are '15min', '30min', 'hourly',
#' 'monthly', 'yearly'.  For intervals shorter than 1 day, time period covered
#' will be midnight to midnight, with the last time interval being before
#' midnight - hour/minute values are for the end of the time period.
#' Data for shorter intervals ('15min', '30min') are available from
#' January of the previous year.
#' @param which_vars Match weather summary selected. Defaults to "all".
#' Can be one of "all", "rain", "wind", "temp" and "erosion."
#'
#' @return a `data table` with station_id and date interval queried together
#' with the requested weather variables.
#'
#' @note Please note this function converts date-time columns from Coordinated
#' Universal Time 'UTC' to Australian Western Standard Time 'AWST'.
#'
#' @family DPIRD
#'
#' @examplesIf interactive()
#' # You must have an DPIRD API key to proceed
#' mykey <- 'dpird_api_key'
#'
#' # Set date interval for yearly request
#' # Get rainfall summary
#' start_date <- "20171028"
#'
#' # Use default for end data (current system date)
#' output <- get_dpird_summaries(
#'             station_id = "CL001",
#'             first = start_date,
#'             api_key = mykey,
#'             interval = "yearly",
#'             which_vars = "rain")
#'
#' # Only for wind and erosion conditions for daily time interval
#' # define start and end date
#' start_date <- "20220501"
#' end_date <- "20220502"
#'
#' output <- get_dpird_summaries(
#'             station_id = "BI",
#'             first = start_date,
#'             last = end_date,
#'             api_key = mykey,
#'             interval = "daily",
#'             which_vars = c("wind", "erosion"))
#'
#' @export get_dpird_summaries

get_dpird_summaries <- function(
    station_id = NULL,
    first = NULL,
    last = Sys.Date(),
    api_key = NULL,
    interval = "daily",
    which_vars = "all") {

  if (length(station_id) == 1) {
    return(.query_dpird_summaries(
      station_id = station_id,
      first = first,
      last = last,
      api_key = api_key,
      interval = interval,
      which_vars = which_vars)
    )
  } else {
    # query multiple stations and return the values ----
    return(
      lapply(station_id,
             .query_dpird_summaries,
             station_id = station_id,
             first = first,
             last = last,
             api_key = api_key,
             interval = interval,
             which_vars = which_vars)
    )
  }
}

#' Fetch weather summary from DPIRD weather API for an individual station
#'
#' @param station_id A string with the station ID code for the station of
#' interest.
#' @param first `Integer`. A string representing the start date of the query in
#' the format 'yyyymmdd' (ISO-8601). \pkg{weatherOz} does its best to determine
#' the date given any format but may fail if given an unconventional date format.
#' @param last A string representing the start date of the query in the format '
#' yyyymmdd' (ISO-8601). For intervals less than one day, to get one day of
#' data, last should be the same as first, but must be explicitly coded,
#' as otherwise it will default to the current date.
#' @param api_key \acronym{API} key from \acronym{DPIRD}
#'  <https://www.agric.wa.gov.au/web-apis>.  Defaults to NULL.
#' @param interval Time interval to summarise over. The default is 'daily',
#' others are '15min', '30min', 'hourly', 'monthly', 'yearly'. For intervals
#' shorter than 1 day, time period covered will be midnight to midnight, with
#' the last time interval being before midnight–hour/minute values are for the
#' end of the time period. Data for shorter intervals ('15min', '30min') should
#' be available from January of last year.
#' @return a `list` with 3 elements: the station code, station name and a nested
#' `data frame` with the all summary output as per Weather \acronym{API}
#' documentation.
#'
#' @note You can request your own API key from DPIRD for free by filling out the
#' form found at <https://www.agric.wa.gov.au/web-apis>. Also note that
#' \acronym{DPIRD} API has usage limits (2000 observations), which are evident
#' if you are querying relative long time periods using the '15min', '30min'
#' and 'hourly' summarisation intervals The API will not warn you that the query
#' might have ended before date used in the `last` argument. Check the DPIRD
#' Weather API documentation for further details
#' <https://www.agric.wa.gov.au/web-apis>.
#'
#' @family DPIRD
#'
#' @examples
#' # You must have an DPIRD API key to proceed
#' mykey <- 'dpird_api_key'
#'
#' # set date interval for yearly request
#' start_date <- "20150201"
#'
#' # Use default for end data (current system date)
#' output <- .query_dpird_summaries(
#'            station_id = "AN001",
#'            first = start_date,
#'            api_key = mykey,
#'            interval = "yearly")
#'
#' # 15 min interval query, define start and end date
#' start_date <- "20220501"
#' end_date <- "20220502"
#'
#' output <- .query_dpird_summaries(
#'            station_id = "BI",
#'            first = start_date,
#'            last = end_date,
#'            api_key = mykey,
#'            interval = "15min")
#'
#' @noRd
#' @keywords Internal

.query_dpird_summaries <- function(station_id = NULL,
                                   first = NULL,
                                   last = Sys.Date(),
                                   api_key = NULL,
                                   interval = "daily",
                                   which_vars = "all") {
  if (is.null(station_id))
    stop(call. = FALSE,
         "Station ID required.")

  if (is.null(first))
    stop(call. = FALSE,
         "Please supply a start date.")

  # Error if api key not provided
  if (is.null(api_key)) {
    stop("If you to provide a valid DPIRD API key.\n",
         "Visit: https://www.agric.wa.gov.au/web-apis",
         call. = FALSE)
  }

  # validate user provided date
  first <- .check_date(first)
  last <- .check_date(last)

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

  # Create base query URL for weather summaries
  api <- paste0("https://api.dpird.wa.gov.au/v2/weather/stations/",
                station_id,
                "/summaries/",
                m_int)

  # Select correct time interval input
  uri <- switch(
    m_int,
    `15min` = paste0(
      api,
      "?startDateTime=",
      format(first, "%Y-%m-%d"),
      "T00%3A15%3A00",
      "&endDateTime=",
      format(last + lubridate::days(1), "%Y-%m-%d"),
      "T00%3A00%3A00",
      "&limit=1000",
      "&api_key=",
      api_key
    ),
    `30min` = paste0(
      api,
      "?startDateTime=",
      format(first, "%Y-%m-%d"),
      "T00%3A30%3A00",
      "&endDateTime=",
      format(last + lubridate::days(1), "%Y-%m-%d"),
      "T00%3A00%3A00",
      "&limit=1000",
      "&api_key=",
      api_key
    ),
    hourly = paste0(
      api,
      "?startDateTime=",
      format(first, "%Y-%m-%d"),
      "T01%3A00%3A00",
      "&endDateTime=",
      format(last + lubridate::days(1), "%Y-%m-%d"),
      "T00%3A00%3A00",
      "&limit=1000",
      "&api_key=",
      api_key
    ),
    daily = paste0(
      api,
      "?startDate=",
      format(first, "%Y-%m-%d"),
      "&endDate=",
      format(last, "%Y-%m-%d"),
      "&api_key=",
      api_key,
      "&limit=1000"
    ),
    monthly = paste0(
      api,
      "?startMonth=",
      format(first, "%Y-%m"),
      "&endMonth=",
      format(last, "%Y-%m"),
      "&limit=",
      ceiling(as.double(
        difftime(
          format(last, "%Y-%m-%d"),
          format(first, "%Y-%m-%d"),
          units = "days"
        ) / 365
      ) * 12),
      "&api_key=",
      api_key
    ),
    yearly = paste0(
      api,
      "?startYear=",
      format(first, "%Y"),
      "&endYear=",
      format(last, "%Y"),
      "&offset=0",
      "&limit=100",
      "&api_key=",
      api_key
    )
  )

  # return only data collection; disregard metadata
  out <- .parse_summary(jsonlite::fromJSON(url(uri))$data,
                        which_vars)
  return(out[])
}

#' .parse_summary
#'
#' Parses and tidy up data as returned by `.query_dpird_summaries()`
#'
#' @param .ret_list a list with the DPIRD weather API response
#' @param .which_vars a character vector with the variables to query. See the
#' `.query_dpird_summaries()` for further details.
#'
#' @return a tidy `data table` with station id and request weather summaries
#'
#' @noRd
#' @keywords Internal
#'
.parse_summary <- function(.ret_list = NULL,
                           .which_vars = NULL) {
  # Get query time interval
  out_period <- .ret_list$summaries$period

  # Remove empty columns (eg minute for hourly summaries) and grab number of
  # records in the data collection
  out_period <- out_period[, !apply(is.na(out_period), 2, all)]
  nrec <- nrow(out_period)

  # Airtemp
  if (any(c("all", "temp") %in% .which_vars)) {
    out_temp <- .ret_list$summaries$airTemperature
    names(out_temp) <- paste0("airtemp.",
                              names(out_temp))

  } else {
    out_temp <- data.frame()[1:nrec, ]
  }

  # Rainfall
  if (any(c("all", "rain") %in% .which_vars)) {
    out_rain <- .ret_list$summaries$rainfall

  } else {
    out_rain <- data.frame()[1:nrec, ]
  }

  # Wind
  if (any(c("all", "wind") %in% .which_vars)) {
    temp <- .ret_list$summaries$wind
    temp <- lapply(temp, data.table::as.data.table)

    out_wind <- data.table::rbindlist(temp)
    names(out_wind) <- paste0("wind.",
                              names(out_wind))

  } else {
    out_wind <- data.frame()[1:nrec, ]
  }

  # Wind erosion
  if (any(c("all", "erosion") %in% .which_vars)) {
    out_erosion <- .ret_list$summaries$erosionCondition
    names(out_erosion) <- paste0("wind.erosion.",
                                 names(out_erosion))

  } else {
    out_erosion <- data.frame()[1:nrec, ]
  }

  # Soil temperature
  if (any(c("all", "soil") %in% .which_vars)) {
    out_soil <- .ret_list$summaries$soilTemperature
    names(out_soil) <- paste0("soil.",
                                 names(out_soil))

  } else {
    out_soil <- data.frame()[1:nrec, ]
  }

  # Put together
  out <- data.frame(station_id = .ret_list$stationCode,
                    out_period,
                    out_temp,
                    rain = out_rain,
                    out_wind,
                    out_erosion,
                    out_soil,
                    row.names = NULL)

  names(out) <- tolower(names(out))
  names(out) <- gsub("[.]", "_", names(out))
  out <- data.table::setDT(out)

  out[, to := format(
    lubridate::as_datetime(
      lubridate::ymd_hms(to),
      tz = "Australia/Perth"),
    "%Y-%m-%d %H:%M:%S %Z")]

  out[, from := format(
    lubridate::as_datetime(
      lubridate::ymd_hms(from),
      tz = "Australia/Perth"),
    "%Y-%m-%d %H:%M:%S %Z")]

  if ("airtemp_mintime" %in% colnames(out)) {
    out[, airtemp_mintime := format(
      lubridate::as_datetime(
        lubridate::ymd_hms(airtemp_mintime),
        tz = "Australia/Perth"),
      "%Y-%m-%d %H:%M:%S %Z")]
  }

  if ("airtemp_maxtime" %in% colnames(out)) {
    out[, airtemp_maxtime := format(
      lubridate::as_datetime(
        lubridate::ymd_hms(airtemp_maxtime),
        tz = "Australia/Perth"),
      "%Y-%m-%d %H:%M:%S %Z")]
  }

  if ("wind_max_time" %in% colnames(out)) {
    out[, wind_max_time := format(
      lubridate::as_datetime(
        lubridate::ymd_hms(wind_max_time),
        tz = "Australia/Perth"),
      "%Y-%m-%d %H:%M:%S %Z")]
  }

  if ("wind_erosion_starttime" %in% colnames(out)) {
    out[, wind_erosion_starttime := format(
      lubridate::as_datetime(
        lubridate::ymd_hms(wind_erosion_starttime),
        tz = "Australia/Perth"),
      "%Y-%m-%d %H:%M:%S %Z")]
  }

  if ("soil_mintime" %in% colnames(out)) {
    out[, soil_mintime := format(
      lubridate::as_datetime(
        lubridate::ymd_hms(soil_mintime),
        tz = "Australia/Perth"),
      "%Y-%m-%d %H:%M:%S %Z")]
  }

  if ("soil_maxtime" %in% colnames(out)) {
    out[, soil_maxtime := format(
      lubridate::as_datetime(
        lubridate::ymd_hms(soil_maxtime),
        tz = "Australia/Perth"),
      "%Y-%m-%d %H:%M:%S %Z")]
  }
  return(out[])
}
