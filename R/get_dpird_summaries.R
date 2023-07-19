
#' Get DPIRD Weather Data in Summarised Formats
#'
#' Fetch nicely formatted individual station weather summaries from the
#'   \acronym{DPIRD} Weather 2.0 \acronym{API}.
#'
#' @param station_code A `character` string of the \acronym{DPIRD} station code
#'   for the station of interest.
#' @param start_date A `character` string or `Date` object representing the
#'   beginning of the range to query in the format \dQuote{yyyy-mm-dd}
#'   (ISO8601).  Data returned is inclusive of this range.
#' @param end_date A `character` string or `Date` object representing the end of
#'   the range query in the format  \dQuote{yyyy-mm-dd} (ISO8601).  Data
#'   returned is inclusive of this range.  Defaults to the current system date.
#' @param interval A `character` string that indicates the time interval to
#`   summarise over.  Default is `daily`; others are `15min`, `30min`, `hourly`,
#'   `monthly` or `yearly`.  For intervals shorter than 1 day, the time period
#'   covered will be midnight to midnight, with the end_date time interval being
#'   before midnight - hour/minute values are for the end of the time period.
#'   Data for shorter intervals (`15min`, `30min`) are available from January of
#'   the previous year.
#' @param values A `character` string with the type of summarised weather
#'   to return.  See **Available Values** for a full list of valid values.
#'   Defaults to `all` with all available values being returned.
#' @param include_closed A `Boolean` value that defaults to `FALSE`.  If set to
#'   `TRUE` the query returns closed and open stations.  Closed stations are
#'   those that have been turned off and no longer report data.  They may be
#'   useful for historical purposes.  Only set to `TRUE` to fetch data from
#'   closed stations.
#' @param api_key A `character` string containing your \acronym{API} key from
#'   \acronym{DPIRD}, <https://www.agric.wa.gov.au/web-apis>, for the
#'   \acronym{DPIRD} Weather 2.0 \acronym{API}.
#'
#' @section Available Values:
#'
#'   * all (which will return all of the following values),
#'   * airTemperature,
#'   * airTemperatureAvg,
#'   * airTemperatureMax,
#'   * airTemperatureMaxTime,
#'   * airTemperatureMin,
#'   * airTemperatureMinTime,
#'   * apparentAirTemperature,
#'   * apparentAirTemperatureAvg,
#'   * apparentAirTemperatureMax,
#'   * apparentAirTemperatureMaxTime,
#'   * apparentAirTemperatureMin,
#'   * apparentAirTemperatureMinTime,
#'   * barometricPressure,
#'   * barometricPressureAvg,
#'   * barometricPressureMax,
#'   * barometricPressureMaxTime,
#'   * barometricPressureMin,
#'   * barometricPressureMinTime,
#'   * battery,
#'   * batteryMinVoltage,
#'   * batteryMinVoltageDateTime,
#'   * chillHours,
#'   * deltaT,
#'   * deltaTAvg,
#'   * deltaTMax,
#'   * deltaTMaxTime,
#'   * deltaTMin,
#'   * deltaTMinTime,
#'   * dewPoint,
#'   * dewPointAvg,
#'   * dewPointMax,
#'   * dewPointMaxTime,
#'   * dewPointMin,
#'   * dewPointMinTime,
#'   * erosionCondition,
#'   * erosionConditionMinutes,
#'   * erosionConditionStartTime,
#'   * errors,
#'   * etoShortCrop,
#'   * etoTallCrop,
#'   * evapotranspiration,
#'   * frostCondition,
#'   * frostConditionMinutes,
#'   * frostConditionStartTime,
#'   * heatCondition,
#'   * heatConditionMinutes,
#'   * heatConditionStartTime,
#'   * observations,
#'   * observationsCount,
#'   * observationsPercentage,
#'   * panEvaporation,
#'   * rainfall,
#'   * relativeHumidity,
#'   * relativeHumidityAvg,
#'   * relativeHumidityMax,
#'   * relativeHumidityMaxTime,
#'   * relativeHumidityMin,
#'   * relativeHumidityMinTime,
#'   * richardsonUnits,
#'   * soilTemperature,
#'   * soilTemperatureAvg,
#'   * soilTemperatureMax,
#'   * soilTemperatureMaxTime,
#'   * soilTemperatureMin,
#'   * soilTemperatureMinTime,
#'   * solarExposure,
#'   * wetBulb,
#'   * wetBulbAvg,
#'   * wetBulbMax,
#'   * wetBulbMaxTime,
#'   * wetBulbMin,
#'   * wetBulbMinTime,
#'   * wind,
#'   * windAvgSpeed, and
#'   * windMaxSpeed
#'
#' @return a [data.table::data.table] with `station_code` and the date interval
#'   queried together with the requested weather variables in alphabetical
#'   order.  The first ten columns will always be:
#'
#'   * `station_code`,
#'   * `station_name`,
#'   * `latitude`,
#'   * `longitude`,
#'   * `year`,
#'   * `month`,
#'   * `day`,
#'   * `hour`,
#'   * `minute`, and if `month` or finer is present,
#'   * `date` (a combination of year, month, day, hour, minute as appropriate).
#'
#' @note Please note this function converts date-time columns from Coordinated
#'   Universal Time \sQuote{UTC} to Australian Western Standard Time
#'   \sQuote{AWST}.
#'
#' @family DPIRD
#' @family data fetching
#'
#' @author Adam H. Sparks \email{adam.sparks@@dpird.wa.gov.au} and Rodrigo
#'   Pires, \email{rodrigo.pires@@dpird.wa.gov.au}
#'
#' @examples
#' \dontrun{
#' # Note that you need to supply your own API key
#' # Use default for end date (current system date) to get rainfall
#'
#' wd <- get_dpird_summaries(
#'    station_code = "CL001",
#'    start_date = "20171028",
#'    api_key = "your_api_key",
#'    interval = "yearly",
#'    values = "rainfall"
#' )
#'
#' # Only for wind and erosion conditions for daily time interval
#'
#' wd <- get_dpird_summaries(
#'   station_code = "BI",
#'   start_date = "20220501",
#'   end_date = "20220502",
#'   api_key = "your_api_key",
#'   interval = "daily",
#'   values = c(
#'     "wind",
#'     "erosionCondition",
#'     "erosionConditionMinutes",
#'     "erosionConditionStartTime"
#'     )
#' )
#' }
#' @export get_dpird_summaries

get_dpird_summaries <- function(station_code,
                                start_date,
                                end_date = Sys.Date(),
                                interval = "daily",
                                values = "all",
                                include_closed = FALSE,
                                api_key) {
  if (missing(station_code)) {
    stop(call. = FALSE,
         "Please supply a valid `station_code`.")
  }

  if (missing(start_date))
    stop(call. = FALSE,
         "Please supply a valid start date as `start_date`.")

  # Error if api_key is not provided
  if (missing(api_key)) {
    stop(
      "A valid DPIRD API key must be provided, please visit\n",
      "<https://www.agric.wa.gov.au/web-apis> to request one.\n",
      call. = FALSE
    )
  }

  .check_not_example_api_key(api_key)

  if (any(values == "all")) {
    values <- dpird_summary_values
  } else {
    if (any(values %notin% dpird_summary_values)) {
      stop(call. = FALSE,
           "You have specified invalid weather values.")
    }
    values <-
      c("stationCode", "stationName", "period", values)
  }

  # validate user provided dates
  start_date <- .check_date(start_date)
  end_date <- .check_date(end_date)
  .check_date_order(start_date, end_date)

  # Use `agrep()` to fuzzy match the user-requested time interval
  approved_intervals <- c("15min",
                          "30min",
                          "hourly",
                          "daily",
                          "monthly",
                          "yearly")

  likely_interval <- agrep(pattern = interval,
                           x = approved_intervals)

  # Match time interval query to user requests
  checked_interval <-
    try(match.arg(approved_intervals[likely_interval],
                  approved_intervals,
                  several.ok = FALSE),
        silent = TRUE)

  # Error if summary interval is not available. API only allows for daily,
  # 15 min, 30 min, hourly, monthly or yearly
  if (methods::is(checked_interval, "try-error")) {
    stop(call. = FALSE,
         "\"", interval, "\" is not a supported time interval")
  }

  request_interval <- lubridate::interval(start_date,
                                          end_date,
                                          tzone = "Australia/Perth")

  # Stop if query is for 15 and 30 min intervals and date is more than one
  # year in the past
  this_year <- lubridate::year(lubridate::today())

  if (checked_interval %in% c("15min", "30min") &
      lubridate::year(start_date) <
      this_year - 1 |
      checked_interval %in% c("15min", "30min") &
      lubridate::year(end_date) <
      this_year - 1) {
    stop(
      call. = FALSE,
      "Start date is too early. Data in 15 and 30 min intervals are only ",
      "available from the the 1st day of ",
      this_year - 1,
      "."
    )
  }

  # determine how many records are being requested. Default here is 'daily' as
  # with default user arguments
  total_records_req <- data.table::fcase(
    interval == "yearly",
    floor(lubridate::time_length(request_interval, unit = "year") + 1),
    interval == "monthly",
    floor(lubridate::time_length(request_interval, unit = "month") + 1),
    interval == "hourly",
    floor(lubridate::time_length(request_interval, unit = "hour") + 1),
    interval == "30min",
    floor((lubridate::time_length(request_interval, unit = "hour")) + 1) * 2,
    interval == "15min",
    floor((lubridate::time_length(request_interval, unit = "hour")) + 1) * 4,
    default = floor(lubridate::time_length(request_interval, unit = "day") + 1)
  )

  if (total_records_req < 1) {
    stop(
      call. = FALSE,
      "You have submitted a query with 0 total records.\n",
      "Please extend the dates requested."
    )
  }

  query_list <- .build_query(
    station_code = station_code,
    start_date_time = start_date,
    end_date_time = end_date,
    interval = checked_interval,
    values = values,
    include_closed = include_closed,
    api_key = api_key,
    limit = total_records_req
  )

  # set base URL according to interval
  end_point <- data.table::fcase(
    checked_interval == "15min",
    "summaries/15min",
    checked_interval == "30min",
    "summaries/30min",
    checked_interval == "hourly",
    "summaries/hourly",
    checked_interval == "daily",
    "summaries/daily",
    checked_interval == "monthly",
    "summaries/monthly",
    default = "summaries/yearly"
  )

  out <-
    .parse_summary(
      .ret_list = .query_dpird_api(
        .end_point = end_point,
        .query_list = query_list,
        .limit = total_records_req
      ),
      .values = values
    )

  out[, period.from := NULL]
  out[, period.to := NULL]

  .set_snake_case_names(out)

  data.table::setcolorder(out, order(names(out)))

  if (interval == "monthly") {
    out[, date := lubridate::ym(sprintf("%s-%s",
                                        out$period_year,
                                        out$period_month))]

    data.table::setcolorder(out,
                            c(
                              "station_code",
                              "station_name",
                              "period_year",
                              "period_month",
                              "date"
                            ))

    out[, period_day := NULL]
    out[, period_hour := NULL]
    out[, period_minute := NULL]
  } else if (interval == "daily") {
    out[, date := lubridate::ymd(sprintf(
      "%s-%s-%s",
      out$period_year,
      out$period_month,
      out$period_day
    ))]

    data.table::setcolorder(
      out,
      c(
        "station_code",
        "station_name",
        "period_year",
        "period_month",
        "period_day",
        "date"
      )
    )

    out[, period_minute := NULL]
    out[, period_hour := NULL]
  } else if (interval == "hourly") {
    out[, date := lubridate::ymd_h(
      sprintf(
        "%s-%s-%s-%s",
        out$period_year,
        out$period_month,
        out$period_day,
        out$period_hour
      ),
      tz = "Australia/West"
    )]

    data.table::setcolorder(
      out,
      c(
        "station_code",
        "station_name",
        "period_year",
        "period_month",
        "period_day",
        "period_hour",
        "date"
      )
    )

    out[, period_minute := NULL]

  } else if (interval == "30min" || interval == "15min") {
    out[, date := lubridate::ymd_hm(
      sprintf(
        "%s-%s-%s-%s-%s",
        out$period_year,
        out$period_month,
        out$period_day,
        out$period_hour,
        out$period_minute
      ),
      tz = "Australia/West"
    )]

    data.table::setcolorder(
      out,
      c(
        "station_code",
        "station_name",
        "period_year",
        "period_month",
        "period_day",
        "period_hour",
        "period_minute",
        "date"
      )
    )
  } else {
    data.table::setcolorder(out,
                            c("station_code",
                              "station_name",
                              "period_year"))
    out[, period_month := NULL]
    out[, period_day := NULL]
    out[, period_hour := NULL]
    out[, period_minute := NULL]
  }

  if (any(grep("time", colnames(out)))) {
    out[, grep("time", colnames(out)) := suppressMessages(lapply(
      .SD,
      lubridate::ymd_hms,
      truncated = 3,
      tz = "Australia/West"
    )),
    .SDcols = grep("time", colnames(out))]
  }

  data.table::setnames(out, gsub("period_", "", names(out)))

  out[, station_code := as.factor(station_code)]

  data.table::setkey(x = out, cols = station_code)

  return(out[])
}


#' Parse DPIRD API summary data
#'
#' Internal function that parses and tidy up data as returned by
#'  `.query_dpird_api()`
#'
#' @param .ret_list a list with the DPIRD weather API response
#' @param .values a character vector with the variables to query. See the
#' `get_dpird_summaries()` for further details.
#'
#' @return a tidy `data.table` with station id and requested weather summaries
#'
#' @noRd
#' @keywords Internal
#'
.parse_summary <- function(.ret_list,
                           .values) {
  for (i in seq_len(length(.ret_list))) {
    x <- jsonlite::fromJSON(.ret_list[[i]]$parse("UTF8"))
    if ("summaries" %in% names(x$collection)) {
      nested_list_objects <-
        data.table::as.data.table(x$collection$summaries)
      # insert `station_name` and `station_code` into the nested_list_objects df
      nested_list_objects[, station_code := x$collection$stationCode]
      nested_list_objects[, station_name := x$collection$stationName]
    }
  }

  # get the nested list columns and convert them to data.table objects
  col_classes <-
    vapply(nested_list_objects, class, FUN.VALUE = character(1))

  col_lists <- which(col_classes == "list")

  if (length(col_lists) > 0L) {
  new_df_list <- vector(mode = "list", length = length(col_lists))
  names(new_df_list) <- names(col_lists)
  j <- 1
  for (i in col_lists) {
    new_df_list[[j]] <-
      data.table::rbindlist(lapply(X = nested_list_objects[[i]],
                                   FUN = data.table::as.data.table))

    # drop the list column from the org data.table
    nested_list_objects[, names(new_df_list[j]) := NULL]

    j <- j + 1
  }

  x <-
    data.table::setorder(x = data.table::as.data.table(
      do.call(what = cbind, args = new_df_list)),
                         cols = "wind.height",
                         "wind.max.time")

  return(cbind(nested_list_objects, x))
  }

  return(nested_list_objects)

}
