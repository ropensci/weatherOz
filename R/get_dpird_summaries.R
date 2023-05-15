
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
#' @param station_code A `character` string of the \acronym{DPIRD} station code
#'  for the station of interest.
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
#'  Defaults to 'all' with all available values being returned.
#' @param api_group Filter the stations to a predefined group. These need to be
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
#'  queried together with the requested weather variables. Note that the name
#'  of the date column will vary according to the interval that has been
#'  requested, *e.g.*, 'year' interval will return a 'year' column, 'monthly' or
#'  'daily' will return a 'date' column, 'hourly', '30min' or '15min' will
#'  return a 'date_time' column.  The first three columns will be
#'  'station_code', 'station_name' and the year/date/time column. Value columns
#'  will be returned in alphabetical order.
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
#'             which_values = "rainfall")
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
                                api_group = "rtd",
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

  # if "all" is found in `which_values`, disregard everything and just return
  # all values else
  if (any(which_values == "all")) {
    which_values <- dpird_summary_values
  } else {
    if (any(which_values %notin% dpird_summary_values)) {
      stop(call. = FALSE,
           "You have specified invalid weather values.")
    }
    which_values <- c("stationCode", "stationName", which_values)
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
    api_group = api_group,
    include_closed = include_closed,
    api_key = api_key
  )

  # Define the query URL by OS due to issues with WindowsOS
  if (Sys.info()[["sysname"]] == "Windows") {
    base_url <-
      "https://api.agric.wa.gov.au/v2/weather/stations/summaries/"
  } else {
    base_url <-
      "https://api.dpird.wa.gov.au/v2/weather/stations/summaries/"
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

  out <-
    .parse_summary(
      .ret_list = .query_dpird_api(
        .base_url = base_url,
        .query_list = query_list,
        .limit = 1000
      ),
      .which_vars = which_vars
    )

  .set_snake_case_names(out)
  data.table::setcolorder(out, order(names(out)))

  if (interval == "yearly") {
    year <- format(seq(
      from = as.Date(start_date),
      to = as.Date(end_date),
      by = "year"
    ), "%Y")

    # because the DPIRD data is inclusive of start and end dates, we have
    # to append the end year
    if (format(end_date, "%Y") == format(Sys.Date(), "%Y") &
        year[length(year)] != format(end_date, "%Y")) {
      year <- c(year, format(Sys.Date(), "%Y"))
    }
    out[, year := year]
    data.table::setcolorder(out, c("station_code", "station_name", "year"))
  } else if (interval == "monthly") {
    out[, date := format(seq(
      from = as.Date(start_date),
      to = as.Date(end_date),
      by = "month"
    ), "%Y-%m")]
    data.table::setcolorder(out, c("station_code", "station_name", "date"))
  } else if (interval == "daily") {
    out[, date := format(seq(
      from = as.Date(start_date),
      to = as.Date(end_date),
      by = "day"
    ), "%Y-%m-%d")]
    data.table::setcolorder(out, c("station_code", "station_name", "date"))
  } else if (interval == "hourly") {
    out[, date_time := seq(
      from = lubridate::ymd_hm(sprintf("%s 00:00", start_date)),
      to = lubridate::ymd_hm(sprintf("%s 00:00", end_date)),
      by = "hour"
    )]
    data.table::setcolorder(out, c("station_code", "station_name", "date_time"))
  } else if (interval == "30min") {
    out[, date_time := seq(
      from = lubridate::ymd_hm(sprintf("%s 00:00", start_date)),
      to = lubridate::ymd_hm(sprintf("%s 00:00", end_date)),
      by = "30 mins"
    )]
    data.table::setcolorder(out, c("station_code", "station_name", "date_time"))
  } else if (interval == "15min") {
    out[, date_time := seq(
      from = lubridate::ymd_hm(sprintf("%s 00:00", start_date)),
      to = lubridate::ymd_hm(sprintf("%s 00:00", end_date)),
      by = "15 mins"
    )]
    data.table::setcolorder(out, c("station_code", "station_name", "date_time"))
  }
  data.table::setkey(x = out, cols = station_code)

  return(out)
}


#' .parse_summary
#'
#' Internal function that parses and tidy up data as returned by
#'  `.query_dpird_summaries()`
#'
#' @param .ret_list a list with the DPIRD weather API response
#' @param .which_vars a character vector with the variables to query. See the
#' `.query_dpird_summaries()` for further details.
#'
#' @return a tidy `data.table` with station id and request weather summaries
#'
#' @noRd
#' @keywords Internal
#'
.parse_summary <- function(.ret_list,
                           .which_vars) {

  # pull data out into `data.table`
  parsed <- vector(mode = "list", length = length(.ret_list))

  for (i in seq_len(length(.ret_list))) {
    x <- jsonlite::fromJSON(.ret_list[[i]]$parse("UTF8"))
    if ("summaries" %in% names(x$collection)) {
      dpird_stations <- data.table::as.data.table(x$collection$summaries)
      dpird_stations[, station_code := x$collection$stationCode]
      dpird_stations[, station_name := x$collection$stationName]
    }
  }

  col_classes <- vapply(parsed, class, FUN.VALUE = character(1))

  # get the nested list columns
  col_lists <- which(col_classes == "list")

  new_df <- vector(mode = "list", length = length(col_lists))
  names(new_df) <- names(col_lists)
  for (i in col_lists) {
    j <- 1
    new_df[[j]] <-
      data.table::rbindlist(lapply(X = .ret_list[[i]],
                                   FUN = data.table::as.data.table))

    # assign the prefix to the column names, e.g., 'wind.height'
    names(new_df[[j]]) <-
      paste(names(new_df[j]), names(new_df[[j]]), sep = ".")

    # drop the list column from the org data.table
    .ret_list[, names(new_df[j]) := NULL]

    j <- j + 1
  }

  new_df <- do.call(what = cbind, args = new_df)

  out <- cbind(.ret_list, new_df)

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
    out_temp <- data.frame()[1:nrec,]
  }

  # Rainfall
  if (any(c("all", "rain") %in% .which_vars)) {
    out_rain <- .ret_list$summaries$rainfall

  } else {
    out_rain <- data.frame()[1:nrec,]
  }

  # Wind
  if (any(c("all", "wind") %in% .which_vars)) {
    temp <- .ret_list$summaries$wind
    temp <- lapply(temp, data.table::data.table)

    out_wind <- data.table::rbindlist(temp)
    names(out_wind) <- paste0("wind.",
                              names(out_wind))

  } else {
    out_wind <- data.frame()[1:nrec,]
  }

  # Wind erosion
  if (any(c("all", "erosion") %in% .which_vars)) {
    out_erosion <- .ret_list$summaries$erosionCondition
    names(out_erosion) <- paste0("wind.erosion.",
                                 names(out_erosion))

  } else {
    out_erosion <- data.frame()[1:nrec,]
  }

  # Soil temperature
  if (any(c("all", "soil") %in% .which_vars)) {
    out_soil <- .ret_list$summaries$soilTemperature
    names(out_soil) <- paste0("soil.",
                              names(out_soil))

  } else {
    out_soil <- data.table::data.table()[1:nrec,]
  }

  # Put together
  out <- data.table::data.table(
    station_id = .ret_list$stationCode,
    out_period,
    out_temp,
    rain = out_rain,
    out_wind,
    out_erosion,
    out_soil,
    row.names = NULL
  )

  names(out) <- tolower(names(out))
  names(out) <- gsub("[.]", "_", names(out))

  out[, to := format(
    lubridate::as_datetime(lubridate::ymd_hms(to),
                           tz = "Australia/Perth"),
    "%Y-%m-%d %H:%M:%S %Z"
  )]

  out[, from := format(
    lubridate::as_datetime(lubridate::ymd_hms(from),
                           tz = "Australia/Perth"),
    "%Y-%m-%d %H:%M:%S %Z"
  )]

  if ("airtemp_mintime" %in% colnames(out)) {
    out[, airtemp_mintime := format(
      lubridate::as_datetime(lubridate::ymd_hms(airtemp_mintime),
                             tz = "Australia/Perth"),
      "%Y-%m-%d %H:%M:%S %Z"
    )]
  }

  if ("airtemp_maxtime" %in% colnames(out)) {
    out[, airtemp_maxtime := format(
      lubridate::as_datetime(lubridate::ymd_hms(airtemp_maxtime),
                             tz = "Australia/Perth"),
      "%Y-%m-%d %H:%M:%S %Z"
    )]
  }

  if ("wind_max_time" %in% colnames(out)) {
    out[, wind_max_time := format(
      lubridate::as_datetime(lubridate::ymd_hms(wind_max_time),
                             tz = "Australia/Perth"),
      "%Y-%m-%d %H:%M:%S %Z"
    )]
  }

  if ("wind_erosion_starttime" %in% colnames(out)) {
    out[, wind_erosion_starttime := format(
      lubridate::as_datetime(lubridate::ymd_hms(wind_erosion_starttime),
                             tz = "Australia/Perth"),
      "%Y-%m-%d %H:%M:%S %Z"
    )]
  }

  if ("soil_mintime" %in% colnames(out)) {
    out[, soil_mintime := format(
      lubridate::as_datetime(lubridate::ymd_hms(soil_mintime),
                             tz = "Australia/Perth"),
      "%Y-%m-%d %H:%M:%S %Z"
    )]
  }

  if ("soil_maxtime" %in% colnames(out)) {
    out[, soil_maxtime := format(
      lubridate::as_datetime(lubridate::ymd_hms(soil_maxtime),
                             tz = "Australia/Perth"),
      "%Y-%m-%d %H:%M:%S %Z"
    )]
  }
  return(out)
}
