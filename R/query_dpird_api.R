
#' Build a DPIRD Weather 2.0 API Query
#'
#' Construct a list of options to pass to the DPIRD API for summary and minute
#'   data.
#'
#' Note that `get_dpird_extremes()` uses it's own process to build queries and
#'   query the API in it's own function definition.  This only covers the
#'   standard weather data from stations.
#'
#' @param station_code A `character` string or `vector` of the \acronym{DPIRD}
#'   station code(s) for the station(s) of interest.
#' @param start_date_time A `character` string representing the start date and
#'   time of the query in the format 'yyyy-mm-dd-hh-mm'. Defaults to 24 hours
#'   before the current local system time, returning the most recent 24 hour
#'   observations rounded to the nearest minute. This function does its best to
#'   decipher many date and time formats but prefers ISO8601.
#' @param end_date_time A `character` string representing the start date of the
#'   query in the format 'yyyy-mm-dd-hh-mm'.  Defaults to the current system
#'   date rounded to the nearest minute.  This function does its best to
#'   decipher many date and time formats but prefers ISO8601.
#' @param interval Interval to use, one of 'minute', '15min', '30min', 'hourly',
#'  'daily', 'monthly' or 'yearly'.
#' @param limit The pagination limit parameter restricts the number of entries
#'   returned.
#' @param values Values to query from the API
#' @param group A `string` used to filter the stations to a predefined group.
#'   These need to be supported on the back end. 'all' returns all stations,
#'   'api' returns the default stations in use with the API, 'web' returns the
#'   list in use by the weather.agric.wa.gov.au and 'rtd' returns stations with
#'   scientifically complete datasets. Available values: 'api', 'all', 'web' and
#'   'rtd'.
#' @param api_key A `character` string containing your \acronym{API} key from
#'   \acronym{DPIRD}, <https://www.agric.wa.gov.au/web-apis>, for the
#'   \acronym{DPIRD} Weather 2.0 \acronym{API}.
#'
#' @return A `list` object of values to be passed to a [crul] object to query
#'   the \acronym{DPIRD} Weather 2.0 \acronym{API}.
#' @keywords internal
#' @noRd
.build_query <- function(station_code,
                         start_date_time,
                         end_date_time,
                         interval,
                         values,
                         api_group,
                         include_closed,
                         limit,
                         api_key) {

  # the API only accepts "true" or "false" in all lowercase
  include_closed <- tolower(as.character(include_closed))

  if (interval == "minute") {
    query_list <- list(
      startDateTime = start_date_time,
      endDateTime = end_date_time,
      api_key = api_key,
      select = paste(values, collapse = ",")
    )
  } else if (interval %in% c("15min", "30min", "hourly")) {
    query_list <- list(
      stationCode = station_code,
      startDateTime = format(start_date_time, "%Y-%m-%d"),
      endDateTime = format(end_date_time + lubridate::days(1), "%Y-%m-%d"),
      interval = interval,
      select = paste(values, collapse = ","),
      group = api_group,
      includeClosed = include_closed,
      api_key = api_key
    )
  } else if (interval == "daily") {
    query_list <- list(
      stationCode = station_code,
      startDate = format(start_date_time, "%Y-%m-%d"),
      endDate = format(end_date_time, "%Y-%m-%d"),
      select = paste(values, collapse = ","),
      group = api_group,
      includeClosed = include_closed,
      api_key = api_key
    )
  } else if (interval == "monthly") {
    query_list <- list(
      stationCode = station_code,
      startMonth = format(start_date_time, "%Y-%m"),
      endMonth = format(end_date_time, "%Y-%m"),
      limit = ceiling(as.double(
        difftime(
          format(end_date_time, "%Y-%m-%d"),
          format(start_date_time, "%Y-%m-%d"),
          units = "days"
        ) / 365
      ) * 12),
      select = paste(values, collapse = ","),
      group = api_group,
      includeClosed = include_closed,
      api_key = api_key
    )
  }  else {
    query_list <- list(
      stationCode = station_code,
      startYear = format(start_date_time, "%Y"),
      endYear = format(end_date_time, "%Y"),
      select = paste(values, collapse = ","),
      group = api_group,
      includeClosed = include_closed,
      api_key = api_key
    )
  }

  return(query_list)
}


#' Query the DPIRD API
#'
#' Use {crul} to query the DPIRD Weather 2.0 API.
#'
#' @param .end_point the DPIRD Weather 2.0 API end point
#' @param .query_list a list of values in the API to query
#' @param .limit (numeric/integer) the maximum records wanted
#'
#' @return A `data.table` of data for manipulating before returning to the user
#'
#' @noRd
#' @keywords internal

.query_dpird_api <- function(.end_point = NULL,
                             .query_list,
                             .limit) {

  if (!is.null(.end_point)) {
  .base_url <- sprintf("https://api.dpird.wa.gov.au/v2/weather/stations/%s",
                       .end_point)
  } else {
    .base_url <- "https://api.dpird.wa.gov.au/v2/weather/stations/"
  }

  connection <- crul::HttpClient$new(url = .base_url)

  client <- crul::Paginator$new(
    client = connection,
    limit = .limit,
    limit_param = "limit",
    offset_param = "offset",
    chunk = 1000
  )
  response_data <- client$get(query = .query_list)

  # check to see if request failed or succeeded
  # - a custom approach this time combining status code,
  #   explanation of the code, and message from the server
  # check response from start_date item in list, should be same across all
  if (response_data[[1]]$status_code > 201) {
    if (length(
      jsonlite::fromJSON(response_data[[1]]$parse("UTF8"))$error$errors) > 0) {
      x <-
        jsonlite::fromJSON(response_data[[1]]$parse("UTF8"))$error$errors
      stop(sprintf("HTTP (%s) - %s\n  %s",
                   x[, "code"],
                   x[, "message"],
                   x[, "description"]),
           call. = FALSE)
    } else if (length(
      jsonlite::fromJSON(response_data[[1]]$parse("UTF8"))) > 0) {
        stop(call. = FALSE,
             domain = NA,
             gettextf(jsonlite::fromJSON(response_data[[1]]$parse("UTF8"))))
    } else {
      stop(call. = FALSE,
           "An unidentified error has occurred with your query.")
    }
  }

  response_data[[1]]$raise_for_status()
  response_data[[1]]$raise_for_ct_json()
  return(response_data)
}
