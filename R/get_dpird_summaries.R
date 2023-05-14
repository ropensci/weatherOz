#
# file: /R/get_dpird_summaries.R
#
# This file is part of the R-package weatherOz
#
# Copyright (C) 2023 DPIRD
#	<https://www.dpird.wa.gov.au>

#' Get weather data from DPIRD Weather 2.0 API summarised by time interval
#'
#' Nicely formatted individual station weather summaries from the
#'  \acronym{DPIRD} weather station network.
#'
#' @param station_code A `character` string or `vector` of the \acronym{DPIRD}
#'  station code(s) for the station(s) of interest.
#' @param start_date A `character` string representing the beginning of the
#'  range to query in the format 'yyyy-mm-dd' (ISO8601).  Will return data
#'  inclusive of this range.
#' @param end_date A `character` string representing the end of the range query
#'  in the format 'yyyy-mm-dd' (ISO8601).  Will return data inclusive of this
#'  range.  Defaults to the current system date.
#' @param interval A `character` string that indicates the time interval to
#'  summarise over.  Default is 'daily'; others are '15min', '30min', 'hourly',
#'  'monthly' or 'yearly'.  For intervals shorter than 1 day, the time period
#'  covered will be midnight to midnight, with the end_date time interval being
#'  before midnight - hour/minute values are for the end of the time period.
#'  Data for shorter intervals ('15min', '30min') are available from January of
#'  the previous year.
#' @param which_values A `character` string with the type of summarised weather
#'  to return.  See **Available Values** for a full list of valid values.
#'  Optional, as you may specify a `value_group` and query several related
#'  values or all types at once.  Defaults to 'all' with all values being
#'  returned.
#' @param group Filter the stations to a predefined group. These need to be
#'  supported on the back end; 'all' returns all stations, 'api' returns the
#'  default stations in use with the \acronym{API}, 'web' returns the list in
#'  use by the <https:://weather.agric.wa.gov.au> and 'rtd' returns stations
#'  with scientifically complete data sets. Defaults to 'rtd'.
#' @param include_closed A `Boolean` value that defaults to `FALSE`. If set to
#'  `TRUE` the query returns closed and open stations. Closed stations are those
#'  that have been turned off and no longer report data. They may be useful for
#'  historical purposes.
#' @param api_key A `character` string containing your \acronym{API} key from
#'  \acronym{DPIRD}, <https://www.agric.wa.gov.au/web-apis>, for the
#'  \acronym{DPIRD} Weather 2.0 \acronym{API}.
#'
#' ## Available Values for `which_values`
#' * all (which will return all of the following values),
#' * airTemperature,
#' * airTemperatureAvg,
#' * airTemperatureMax,
#' * airTemperatureMaxTime,
#' * airTemperatureMin,
#' * airTemperatureMinTime,
#' * apparentAirTemperature,
#' * apparentAirTemperatureAvg,
#' * apparentAirTemperatureMax,
#' * apparentAirTemperatureMaxTime,
#' * apparentAirTemperatureMin,
#' * apparentAirTemperatureMinTime,
#' * barometricPressure,
#' * barometricPressureAvg,
#' * barometricPressureMax,
#' * barometricPressureMaxTime,
#' * barometricPressureMin,
#' * barometricPressureMinTime,
#' * battery,
#' * batteryMinVoltage,
#' * batteryMinVoltageDateTime,
#' * chillHours,
#' * deltaT,
#' * deltaTAvg,
#' * deltaTMax,
#' * deltaTMaxTime,
#' * deltaTMin,
#' * deltaTMinTime,
#' * dewPoint,
#' * dewPointAvg,
#' * dewPointMax,
#' * dewPointMaxTime,
#' * dewPointMin,
#' * dewPointMinTime,
#' * erosionCondition,
#' * erosionConditionMinutes,
#' * erosionConditionStartTime,
#' * errors,
#' * etoShortCrop,
#' * etoTallCrop,
#' * evapotranspiration,
#' * frostCondition,
#' * frostConditionMinutes,
#' * frostConditionStartTime,
#' * heatCondition,
#' * heatConditionMinutes,
#' * heatConditionStartTime,
#' * observations,
#' * observationsCount,
#' * observationsPercentage,
#' * panEvaporation,
#' * period,
#' * periodDay,
#' * periodFrom,
#' * periodHour,
#' * periodMinute,
#' * periodMonth,
#' * periodTo,
#' * periodYear,
#' * rainfall,
#' * relativeHumidity,
#' * relativeHumidityAvg,
#' * relativeHumidityMax,
#' * relativeHumidityMaxTime,
#' * relativeHumidityMin,
#' * relativeHumidityMinTime,
#' * richardsonUnits,
#' * soilTemperature,
#' * soilTemperatureAvg,
#' * soilTemperatureMax,
#' * soilTemperatureMaxTime,
#' * soilTemperatureMin,
#' * soilTemperatureMinTime,
#' * solarExposure,
#' * wetBulb,
#' * wetBulbAvg,
#' * wetBulbMax,
#' * wetBulbMaxTime,
#' * wetBulbMin,
#' * wetBulbMinTime,
#' * wind,
#' * windAvgSpeed, and
#' * windMaxSpeed
#'
#' @return a [data.table::data.table]  with 'station_code' and date interval
#'  queried together with the requested weather variables.
#'
#' @note Please note this function converts date-time columns from Coordinated
#'  Universal Time 'UTC' to Australian Western Standard Time 'AWST'.
#'
#' @family DPIRD
#'
#' @examples
#' \dontrun{
#' # You must have an DPIRD API key to proceed
#' # Set date interval for yearly request
#' # Get rainfall summary
#' start_date <- "20171028"
#'
#' # Use default for end data (current system date)
#' output <- get_dpird_summaries(
#'             station_code = "CL001",
#'             start_date = start_date,
#'             api_key = "YOUR API KEY",
#'             interval = "yearly",
#'             which_values = "rain")
#'
#' # Only for wind and erosion conditions for daily time interval
#' # define start and end date
#' start_date <- "20220501"
#' end_date <- "20220502"
#'
#' output <- get_dpird_summaries(
#'             station_code = "BI",
#'             start_date = start_date,
#'             end_date = end_date,
#'             api_key = "YOUR API KEY",
#'             interval = "daily",
#'             which_values = c("wind", "erosion"))
#' }
#' @export get_dpird_summaries

get_dpird_summaries <- function(station_code,
                                start_date,
                                end_date = Sys.Date(),
                                interval = "daily",
                                which_values = "all",
                                group = "rtd",
                                include_closed = FALSE,
                                api_key) {
  if (missing(station_code)) {
    stop(call. = FALSE,
         "Please supply a valid `station_code`.")
  }

  if (missing(start_date))
    stop(call. = FALSE,
         "Please supply a valid start date as `start_date`.")

  # Error if api key not provided
  if (missing(api_key)) {
    stop(
      "A valid DPIRD API key must be provided, please visit\n",
      "<https://www.agric.wa.gov.au/web-apis> to request one.\n",
      call. = FALSE
    )
  }

  if (which_values != "all" & which_values %notin% dpird_summary_values) {
    stop(call. = FALSE,
         "You have specified invalid weather values.")
  }

  if (which_values == "all") {
    which_values <- dpird_summary_values
  }

  # validate user provided date
  start_date <- .check_date(start_date)
  end_date <- .check_date(end_date)
  .check_date_order(start_date, end_date)

  # Match time interval query to user requests
  m_int <- try(match.arg(interval,
                         c("15min",
                           "30min",
                           "hourly",
                           "daily",
                           "monthly",
                           "yearly"),
                         several.ok = FALSE),
               silent = TRUE)

  # Stop if query is for monthly and interval is wrong
  if (m_int %in% c("monthly") &&
      (lubridate::interval(start_date,
                           end_date,
                           tzone = "Australia/Perth")) < 0) {
    stop(call. = FALSE,
         "For monthly intervals the interval should be at least one month.")
  }

  # Stop if query is for daily and interval is wrong
  if (m_int %in% c("daily") &&
      (lubridate::interval(start_date,
                           end_date,
                           tzone = "Australia/Perth")) < 0) {
    stop(call. = FALSE,
         "For daily intervals the interval should be at least one day.")
  }

  # Error if summary interval is not available. API only allows for daily,
  # 15 min, 30 min, hourly, monthly, yearly
  if (methods::is(m_int, "try-error")) {
    stop(call. = FALSE,
         "\"", interval, "\" is not a supported time interval")
  }

  # Stop if query is for 15 and 30 min intervals and date is more than one
  # year in the past
  if (m_int %in% c("15min", "30min") & lubridate::year(start_date) <
      lubridate::year(lubridate::today()) - 1 |
      m_int %in% c("15min", "30min") & lubridate::year(end_date) <
      lubridate::year(lubridate::today()) - 1) {
    stop(
      call. = FALSE,
      "Start date is too early. Data in 15 and 30 min intervals are only ",
      "available from the the 1st day of ",
      lubridate::year(lubridate::today()) - 1,
      "."
    )
  }

  query_list <- .build_query(
    station_code = station_code,
    start_date_time = start_date,
    end_date_time = end_date,
    interval = interval,
    which_values = which_values,
    group = group,
    include_closed = include_closed,
    api_key = api_key
  )

  # Define the query URL by OS due to issues with WindowsOS
  if (Sys.info()[["sysname"]] == "Windows") {
    base_url <- "https://api.agric.wa.gov.au/v2/weather/stations/summaries/"
  } else {
    base_url <- "https://api.agric.wa.gov.au/v2/weather/stations/summaries/"
  }

  # set base URL according to interval
  if (interval == "15min") {
    base_url <- paste0(base_url, "15min")
  } else if (interval == "30min") {
    base_url <- paste0(base_url, "30min")
  } else if (interval == "hourly") {
    base_url <- paste0(base_url, "hourly")
  } else if (interval == "daily") {
    base_url <- paste0(base_url, "daily")
  } else if (interval == "monthly") {
    base_url <- paste0(base_url, "monthly")
  } else if (interval == "yearly") {
    base_url <- paste0(base_url, "yearly")
  }

  out <- .query_dpird_api(.base_url = base_url,
                          .query_list = query_list,
                          .limit = 1000)
  .set_snake_case_names(out)
  out[, date_time := hour_sequence]
  out[, station_code := station_code]
  data.table::setkey(x = out, cols = station_code)
  data.table::setcolorder(out, c("station_code", "date_time"))
  return(out)

}

