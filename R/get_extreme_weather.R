# file: /R/get_extreme_weather.R
#
# This file is part of the R-package weatherOz
#
# Copyright (C) 2023 DPIRD
#	<https://www.dpird.wa.gov.au>

#' Get extreme weather event summaries for a single DPIRD station
#'
#' @param station_code A `character` string with the station code for the
#'   station of interest.
#' @param values A `character` string with the type of extreme weather to
#'   return.  See **Available Values** for a full list of valid values.
#'   Defaults to 'all', returning the full list of values unless otherwise
#'   specified.
#' @param group Filter the stations to a predefined group. These need to be
#'   supported on the back end; 'all' returns all stations, 'api' returns the
#'   default stations in use with the \acronym{API}, 'web' returns the list in
#'   use by the <https:://weather.agric.wa.gov.au> and 'rtd' returns stations
#'   with scientifically complete data sets. Defaults to 'rtd'.
#' @param include_closed A `Boolean` value that defaults to `FALSE`. If set to
#'   `TRUE` the query returns closed and open stations. Closed stations are
#'   those that have been turned off and no longer report data.  They may be
#'   useful for historical purposes.
#' @param api_key A `character` string containing your \acronym{API} key from
#'   \acronym{DPIRD}, <https://www.agric.wa.gov.au/web-apis>, for the
#'   \acronym{DPIRD} Weather 2.0 \acronym{API}.
#'
#' @section Available Values for `values`:
#' * all (returns all of the following values),
#' * erosionCondition,
#' * erosionConditionLast7Days,
#' * erosionConditionLast7DaysDays,
#' * erosionConditionLast7DaysMinutes,
#' * erosionConditionLast14Days,
#' * erosionConditionLast14DaysDays,
#' * erosionConditionLast14DaysMinutes,
#' * erosionConditionMonthToDate,
#' * erosionConditionMonthToDateDays,
#' * erosionConditionMonthToDateMinutes,
#' * erosionConditionMonthToDateStartTime,
#' * erosionConditionSince12AM,
#' * erosionConditionSince12AMMinutes,
#' * erosionConditionSince12AMStartTime,
#' * erosionConditionYearToDate,
#' * erosionConditionYearToDateDays,
#' * erosionConditionYearToDateMinutes,
#' * erosionConditionYearToDateStartTime,
#' * frostCondition,
#' * frostConditionLast7Days,
#' * frostConditionLast7DaysDays,
#' * frostConditionLast7DaysMinutes,
#' * frostConditionLast14Days,
#' * frostConditionLast14DaysDays,
#' * frostConditionLast14DaysMinutes,
#' * frostConditionMonthToDate,
#' * frostConditionMonthToDateDays,
#' * frostConditionMonthToDateMinutes,
#' * frostConditionMonthToDateStartTime,
#' * frostConditionSince9AM,
#' * frostConditionSince9AMMinutes,
#' * frostConditionSince9AMStartTime,
#' * frostConditionTo9AM,
#' * frostConditionTo9AMMinutes,
#' * frostConditionTo9AMStartTime,
#' * frostConditionYearToDate,
#' * frostConditionYearToDate,
#' * frostConditionYearToDateMinutes,
#' * frostConditionYearToDateStartTime,
#' * heatCondition,
#' * heatConditionLast7Days,
#' * heatConditionLast7DaysDays,
#' * heatConditionLast7DaysMinutes,
#' * heatConditionLast14Days,
#' * heatConditionLast14DaysDays,
#' * heatConditionLast14DaysMinutes,
#' * heatConditionMonthToDate,
#' * heatConditionMonthToDateDays,
#' * heatConditionMonthToDateMinutes,
#' * heatConditionMonthToDateStartTime,
#' * heatConditionSince12AM,
#' * heatConditionSince12AMMinutes,
#' * heatConditionSince12AMStartTime,
#' * heatConditionYearToDate,
#' * heatConditionYearToDateDays,
#' * heatConditionYearToDateMinutes, and
#' * heatConditionYearToDateStartTime
#'
#' @return a [data.table::data.table] of one row with 'station_code',
#'   'station_name', 'latitude', 'longitude', 'date_time' of the query and the
#'   extreme weather information according to the value(s) selected.
#'
#' @family DPIRD
#'
#' @examples
#' \dontrun{
#' # You must have an DPIRD API key to proceed
#' # Query Bonnie Rock station for wind erosion and heat extreme events.
#'
#' xtreme <- get_extreme_weather(
#'   station_code = "BR",
#'   type = c("erosionCondition",
#'            "heatCondition"),
#'   api_key = my_key
#' )
#' }
#'
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au} and Adam H.
#'  Sparks, \email{adam.sparks@@dpird.wa.gov.au}.
#'
#' @export

get_extreme_weather <- function(station_code,
                                values = "all",
                                group = "rtd",
                                include_closed = FALSE,
                                api_key) {
  if (missing(station_code)) {
    stop(
      call. = FALSE,
      "Please provide a station code via the `station_code` argument.\n",
      "It should take a string e.g., `AN001` for Allanooka station.\n"
    )
  }

  if (length(station_code) != 1L) {
    stop(call. = FALSE,
         "You have provided more than one `station_code`.\n",
         "This function only handles one `station_code` per query.\n")
  }

  # Error if api_key is not provided
  if (missing(api_key)) {
    stop(
      "A valid DPIRD API key must be provided, please visit\n",
      "<https://www.agric.wa.gov.au/web-apis> to request one.\n",
      call. = FALSE
    )
  }

  if (any(values == "all")) {
    .values <- dpird_extreme_weather_values
  } else {
    if (any(values %notin% dpird_extreme_weather_values)) {
      stop(call. = FALSE,
           "You have specified invalid extreme weather values.")
    }
    .values <-
      dpird_extreme_weather_values[names(
        dpird_extreme_weather_values) %in% values]
  }

  values <- c("stationCode", "dateTime", "latitude", "longitude", values)

  query_list <- list(
    stationCode = station_code,
    offset = 0L,
    select = paste(values, collapse = ","),
    group = group,
    includeClosed = include_closed,
    api_key = api_key
  )

  out <- .query_dpird_api(.end_point = "extreme-conditions",
                          .query_list = query_list,
                          .limit = 1L)

  out <- jsonlite::fromJSON(out[[1]]$parse("UTF8"))
  out <- data.table::data.table(out$collection)

  .set_snake_case_names(out)

  if (any(grep("time", colnames(out)))) {
    out[, grep("time", colnames(out)) := suppressMessages(lapply(
      .SD,
      lubridate::ymd_hms,
      truncated = 3,
      tz = "Australia/West"
    )),
    .SDcols = grep("time", colnames(out))]
  }

  out[, station_code := station_code]
  data.table::setkey(x = out, cols = station_code)
  data.table::setcolorder(out, c("station_code", "date_time"))
  return(out)
}
