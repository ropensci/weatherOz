
#' Construct a list of options to pass to the DPIRD API
#'
#' @param station_code A `character` string or `vector` of the \acronym{DPIRD}
#'  station \acronym{ID} code(s) for the station(s) of interest.
#' @param start_date_time A `character` string representing the start date and
#'  time of the query in the format 'yyyy-mm-dd-hh-mm'. Defaults to 24 hours
#'  before the current local system time, returning the most recent 24 hour
#'  observations rounded to the nearest minute. This function does its best to
#'  decipher many date and time formats but prefers ISO8601.
#' @param end_date_time A `character` string representing the start date of the
#'  query in the format 'yyyy-mm-dd-hh-mm'.  Defaults to the current system
#'  date rounded to the nearest minute.  This function does its best to
#'  decipher many date and time formats but prefers ISO8601.
#' @param api_key A `character` string containing your \acronym{API} key from
#'  \acronym{DPIRD}, <https://www.agric.wa.gov.au/web-apis>, for the
#'  \acronym{DPIRD} weather \acronym{API}.
#' @param interval Interval to use, one of "minute", "15min", "30min", "hourly",
#' "daily", "monthly" or "yearly".
#' @param limit The pagination limit parameter restricts the number of entries
#'  returned.
#' @param which_values Values to query from the API
#' @param group A `string` used to filter the stations to a predefined group.
#'  These need to be supported on the back end. 'all' returns all stations,
#'  'api' returns the default stations in use with the API, 'web' returns the
#'  list in use by the weather.agric.wa.gov.au and 'rtd' returns stations with
#'  scientifically complete datasets. Available values: 'api', 'all', 'web' and
#'  'rtd'.
#'
#' @return A `list` object of values to be passed to a [crul] object to query
#'  the \acronym{DPIRD} Weather 2.0 \acronym{API}.
#' @keywords internal
#' @noRd
.build_query <- function(station_code,
                         start_date_time,
                         end_date_time,
                         api_key,
                         interval,
                         limit,
                         which_values,
                         group) {

  if (interval == "minute") {
    query_list <- list(
      startDateTime = start_date_time,
      endDateTime = end_date_time,
      api_key = api_key,
      select = paste(which_values, collapse = ","),
      limit = 1000
    )
  } else if (interval %in% c("15min", "30min", "hourly")) {
    query_list <- list(
      stationCode = station_code,
      startDateTime = format(first, "%Y-%m-%d"),
      endDateTime = format(last + lubridate::days(1), "%Y-%m-%d"),
      interval = interval,
      select = which_values,
      limit = 1000,
      group = all,
      api_key = api_key
    )
  } else if (interval == "daily") {
    query_list <- list(
      stationCode = station_code,
      startDateTime = format(first, "%Y-%m-%d"),
      endDateTime = format(last, "%Y-%m-%d"),
      select = which_values,
      limit = 1000,
      group = all,
      api_key = api_key
    )
  } else if (interval == "monthly") {
    query_list <- list(
      stationCode = station_code,
      startDateTime = format(first, "%Y-%"),
      endDateTime = format(last, "%Y-%m"),
      limit = ceiling(as.double(
        difftime(
          format(last, "%Y-%m-%d"),
          format(first, "%Y-%m-%d"),
          units = "days"
        ) / 365
      ) * 12),
      select = which_values,
      group = all,
      api_key = api_key
    )
  }  else {
    query_list <- list(
      stationCode = station_code,
      startDateTime = format(first, "%Y"),
      endDateTime = format(last, "%Y"),
      select = which_values,
      limit = 1000,
      group = all,
      api_key = api_key
    )
  }

  return(query_list)
}

#' Query the DPIRD API using {crul}
#'
#' @param .base_url the base URL for the API query
#' @param .query_list a list of values in the API to query
#' @param .limit (numeric/integer) the maximum records wanted. Defaults to 1000
#'  as per the Weather 2.0 API
#'
#' @return A `data.table` of data for manipulating before returning to the user
#'
#' @noRd
#' @keywords internal

.query_dpird_api <- function(.base_url,
                             .query_list,
                             .limit) {
  connection <- crul::HttpClient$new(url = .base_url)

  client <- crul::Paginator$new(client = connection,
                                limit = .limit,
                                limit_param = "limit",
                                offset_param = "offset",
                                chunk = 1000)
  response <- client$get(query = .query_list)

  # check to see if request failed or succeeded
  # - a custom approach this time combining status code,
  #   explanation of the code, and message from the server

  if (length(response) == 1L) {
    if (response$status_code > 201) {
      mssg <- jsonlite::fromJSON(response$parse("UTF-8"))$message
      x <- response$status_http()
      stop(sprintf(
        "HTTP (%s) - %s\n  %s",
        x$status_code,
        x$message,
        x$explanation
      ),
      call. = FALSE)
    }

    response$raise_for_status()

    # pull data out into `data.table`
    x <- jsonlite::fromJSON(response$parse("UTF8"))
    dpird_stations <- data.table::data.table(x$collection)

  } else {
    # check response from first item in list, should be same across all
    if (response[[1]]$status_code > 201) {
      mssg <- jsonlite::fromJSON(response[[1]]$parse("UTF-8"))$message
      x <- response$status_http()
      stop(sprintf(
        "HTTP (%s) - %s\n  %s",
        x$status_code,
        x$message,
        x$explanation
      ),
      call. = FALSE)
    }

    response[[1]]$raise_for_status()

    # pull data out into `data.table`
    parsed <- vector(mode = "list", length = length(response))
    for (i in seq_len(length(response))) {
      x <- jsonlite::fromJSON(response[[i]]$parse("UTF8"))
      parsed[[i]] <- data.table::data.table(x$collection)
    }
    dpird_stations <- data.table::rbindlist(parsed)
  }
  return(dpird_stations)
}
