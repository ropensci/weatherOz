#' Get minute weather data from DPIRD Weather 2.0 API
#'
#' Nicely formatted minute weather station data from the \acronym{DPIRD} weather
#'  station network over a maximum 24 hour period.
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
#' @param which_values A `vector` of weather values to query from the
#'  \acronym{API}. See **Available Values** section for valid available codes.
#' @section Available Values:
#' * airTemperature,
#' * dateTime,
#' * dewPoint,
#' * rainfall,
#' * relativeHumidity,
#' * soilTemperature,
#' * solarIrradiance,
#' * wetBulb,
#' * wind,
#' * windAvgSpeed,
#' * windMaxSpeed, and
#' * windMinSpeed
#'
#' @note Please note this function converts date-time columns from Coordinated
#'  Universal Time 'UTC' to Australian Western Standard Time 'AWST'.
#'
#' @return a `data.table` with 'station_code' and date interval queried together
#'  with the requested weather variables.
#'
#' @examplesIf interactive()
#' library(lubridate)
#'
#' today <- now()
#' yesterday <- now() - hours(24)
#'
#' get_dpird_minute_data(station_code = "NO",
#'                       start_date_time = today,
#'                       end_date_time = yesterday,
#'                       api_key = "YOUR_API_KEY",
#'                       which_values = c("airTemperature",
#'                                        "solarIrradiance"))
#'
#' @family DPIRD

get_dpird_minute_data <- function(station_code,
                                  start_date_time = lubridate::round_date(
                                      lubridate::now() - lubridate::hours(24),
                                      unit = "minute"),
                                  end_date_time = lubridate::round_date(
                                      lubridate::now(),
                                      unit = "minute"),
                                  api_key,
                                  which_values) {

  if (any(
    which_values %notin% c(
      "airTemperature",
      "dateTime",
      "dewPoint",
      "rainfall",
      "relativeHumidity",
      "soilTemperature",
      "solarIrradiance",
      "wetBulb",
      "wind",
      "windAvgSpeed",
      "windMaxSpeed",
      "windMinSpeed"
    )
  )) {
    stop(call. = FALSE,
         "You have specified a 'value' in `which_values`\n",
         "that is not available in the 'API'.\n")
  }

  start_date_time <- .check_date_time(start_date_time)
  end_date_time <- .check_date_time(end_date_time)

  if (start_date_time > end_date_time) {
    stop(.Call = FALSE,
         "The `start_date_time` and `end_date_time` are reversed.")
  }

  query_list <- .build_query(
    station_code = NULL,
    start_date_time = start_date_time,
    end_date_time = end_date_time,
    api_key = api_key,
    interval = "minute",
    which_values = which_values,
    limit = 1440,
    group = NULL
  )

  minute_base_url = sprintf("%sweather/stations/%s/data",
                     base_dpird_url,
                     station_code,
                     "data")
  out <- .query_dpird_api(.base_url = minute_base_url,
                          .query_list = query_list)

  return(out)
}

#' Check user-provided start and end date-time objects
#' @param x user provide date-time object
#' @return A `character` string of the date time in ISO8601 format in UTC TZ
#' .check_date_time(lubridate::now())
#' @keywords internal
#' @noRd

.check_date_time <- function(x) {
  tryCatch(
    x <- lubridate::parse_date_time(
      x,
      c(
        "YmdHMS",
        "dmYHMS",
        "mdYHMS",
        "BdYHMS",
        "BdyHMS",
        "bdYHMS",
        "bdyHMS",
        "YmdIMSp",
        "dmYIMSp",
        "mdYIMSp",
        "BdYIMSp",
        "BdyIMSp",
        "bdYIMSp",
        "bdyIMSp"
      ),
      tz = Sys.timezone()
    ),
    warning = function(c) {
      stop(call. = FALSE,
           "\n",
           x,
           " is not in a valid date format. Please enter a valid date format.",
           "\n")
    }
  )
  return(lubridate::format_ISO8601(x, usetz = "Z"))
}
