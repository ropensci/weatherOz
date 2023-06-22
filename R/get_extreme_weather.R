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
#' @param which_values A `character` string with the type of extreme weather to
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
#' @section Available Values for `which_values`:
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
#'            "heatCondition),
#'   api_key = my_key
#' )
#' }
#'
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au} and Adam H.
#'  Sparks, \email{adam.sparks@@dpird.wa.gov.au}.
#'
#' @export

get_extreme_weather <- function(station_code,
                                which_values = "all",
                                group = "rtd",
                                include_closed = FALSE,
                                api_key) {
  if (missing(station_code)) {
    stop(
      call. = FALSE,
      "Provide a station code via the `site` argument. It should take a string
         e.g., `AN001` for Allanooka station."
    )
  }

  if (length(station_code) != 1L) {
    stop(call. = FALSE,
         "Wrong number of sites.\n",
         "This function only handles one site per query.")
  }

  # Error if api_key is not provided
  if (missing(api_key)) {
    stop(
      "A valid DPIRD API key must be provided, please visit\n",
      "<https://www.agric.wa.gov.au/web-apis> to request one.\n",
      call. = FALSE
    )
  }

  if (any(which_values == "all")) {
    .which_values <- dpird_extreme_weather_values
  } else {
    if (any(which_values %notin% dpird_extreme_weather_values)) {
      stop(call. = FALSE,
           "You have specified invalid extreme weather values.")
    }
    .which_values <-
      dpird_extreme_weather_values[names(
        dpird_extreme_weather_values) %in% which_values]
  }



  values <- c("stationCode", "dateTime", "latitude", "longitude", which_values)

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
  .set_snake_case_names(out)

  out[, station_code := station_code]
  data.table::setkey(x = out, cols = station_code)
  data.table::setcolorder(out, c("station_code", "date_time"))
  return(out)
}

#' Parse DPIRD API summary data
#'
#' Internal function that parses and tidy up data as returned by
#'  `.query_dpird_api()`
#'
#' @param .ret_list a list with the DPIRD weather API response
#' @param .which_values a character vector with the variables to query. See the
#' `get_dpird_summaries()` for further details.
#'
#' @return a tidy `data.table` with station id and requested weather summaries
#'
#' @noRd
#' @keywords Internal
#'
.parse_extreme <- function(.ret_list,
                           .which_values) {

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

  return(cbind(nested_list_objects, do.call(what = cbind, args = new_df_list)))
}

