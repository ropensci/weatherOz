
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
#'  \acronym{DPIRD}, see <https://www.agric.wa.gov.au/web-apis> for the
#'  \acronym{DPIRD} Weather 2.0 \acronym{API}.
#' @param which_values A `vector` of weather values to query from the
#'  \acronym{API}. See **Available Values** section for valid available codes.
#'  Defaults to all available values.
#'
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
#'  Universal Time 'UTC' returned by the \acronym{API} to Australian Western
#'  Standard Time 'AWST'.
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
#' get_dpird_minute(station_code = "NO",
#'                  start_date_time = yesterday,
#'                  end_date_time = today,
#'                  api_key = YOUR_API_KEY,
#'                  which_values = c("airTemperature", "solarIrradiance"))
#'
#' @family DPIRD
#' @export

get_dpird_minute <- function(station_code,
                             start_date_time = lubridate::now() -
                               lubridate::minutes(1439),
                             end_date_time = lubridate::now(),
                             api_key,
                             which_values) {
  if (missing(station_code)) {
    stop(call. = FALSE,
         "Please supply a valid `station_id`.")
  }

  if (missing(api_key)) {
    stop(
      "A valid DPIRD API key must be provided, please visit\n",
      "<https://www.agric.wa.gov.au/web-apis> to request one.\n",
      call. = FALSE
    )
  }

  all_values <- c(
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

  if (missing(which_values)) {
    which_values <- all_values
  }

  # TODO: add feedback for the user including which values are invalid
  if (any(which_values %notin% all_values)) {
    stop(call. = FALSE,
         "You have specified a 'value' in `which_values`\n",
         "that is not available in the 'API'.\n")
  }

  start_date_time <- .check_date_time(start_date_time)
  end_date_time <- .check_date_time(end_date_time)

  if (start_date_time > end_date_time) {
    stop(call. = FALSE,
         "The `start_date_time` and `end_date_time` are reversed.")
  }

  hour_sequence <- clock::date_seq(from = start_date_time,
                  to = end_date_time,
                  by = clock::duration_minutes(1))
  total_recs_req <- length(hour_sequence)
  if (total_recs_req > 1441) {
    stop(call. = FALSE,
         "The API only supports queries for a maximum 24hr interval.")
  }

  query_list <- .build_query(
    station_code = NULL,
    start_date_time = lubridate::format_ISO8601(start_date_time, usetz = "Z"),
    end_date_time = lubridate::format_ISO8601(end_date_time, usetz = "Z"),
    api_key = api_key,
    interval = "minute",
    which_values = which_values,
    limit = total_recs_req,
    group = NULL
  )

  # Check the operating system
  os <- Sys.info()[["sysname"]]

  # Define the query URLs
  if (os == "Windows") {
    base_dpird_url <- "https://api.agric.wa.gov.au/v2/"
  } else {
    base_dpird_url <- "https://api.dpird.wa.gov.au/v2/"
  }

  minute_base_url = sprintf("%sweather/stations/%s/data",
                     base_dpird_url,
                     station_code)
  out <- .query_dpird_api(.base_url = minute_base_url,
                          .query_list = query_list,
                          .limit = length(hour_sequence))
  .set_snake_case_names(out)
  out[, date_time := hour_sequence]
  out[, station_code := station_code]
  data.table::setkey(x = out, cols = station_code)
  data.table::setcolorder(out, c("station_code", "date_time"))
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
  return(x)
}
