
#' Get minute weather data from DPIRD Weather 2.0 API
#'
#' Nicely formatted minute weather station data from the \acronym{DPIRD} weather
#'  station network over a maximum 24 hour period.
#'
#' @param station_code A `character` string or `vector` of the \acronym{DPIRD}
#'  station code for the station of interest.
#' @param start_date_time A `character` string representing the start date and
#'  time of the query in the format 'yyyy-mm-dd-hh-mm'. Defaults to 24 hours
#'  before the current local system time, returning the most recent 24 hour
#'  observations rounded to the nearest minute. This function does its best to
#'  decipher many date and time formats but prefers ISO8601.
#' @param minutes An `integer` value that provides the number of observations to
#'  be returned. Defaults to 1440 minutes for 24 hours of observations.
#' @param which_values A `vector` of weather values to query from the
#'  \acronym{API}. See **Available Values** section for valid available codes.
#'  Defaults to all available values, "all".
#' @param api_key A `character` string containing your \acronym{API} key from
#'  \acronym{DPIRD}, <https://www.agric.wa.gov.au/web-apis>, for the
#'  \acronym{DPIRD} Weather 2.0 \acronym{API}.
#'
#' ## Available Values:
#' * all (includes all of the following),
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
#' @return a [data.table::data.table] with 'station_code' and date interval
#'  queried together with the requested weather variables.
#'
#' @examples
#' \dontrun{
#' library(lubridate)
#'
#' yesterday <- now() - hours(24)
#'
#' get_dpird_minute(station_code = "SP",
#'                  start_date_time = "2018-02-01 13:00:00",
#'                  minutes = 1440,
#'                  api_key = YOUR_API_KEY,
#'                  which_values = c("airTemperature",
#'                                   "solarIrradiance",
#'                                   "wind"))
#' }
#' @family DPIRD
#' @export

get_dpird_minute <- function(station_code,
                             start_date_time = lubridate::now() -
                               lubridate::hours(24L),
                             minutes = 1440L,
                             which_values = "all",
                             api_key) {
  if (missing(station_code)) {
    stop(call. = FALSE,
         "Please supply a valid `station_code`.")
  }

  if (missing(api_key)) {
    stop(
      "A valid DPIRD API key must be provided, please visit\n",
      "<https://www.agric.wa.gov.au/web-apis> to request one.\n",
      call. = FALSE
    )
  }

  if (any(which_values %notin% dpird_minute_values)) {
    if (which_values != "all") {
      stop(call. = FALSE,
           "You have specified a value not found in the 'API'.")
    }
  }

  if ("all" %in% which_values) {
    which_values <- dpird_minute_values
  } else {
    which_values <- c(which_values, "dateTime")
  }

  start_date_time <- .check_date_time(start_date_time)

  hour_sequence <- clock::date_seq(
    from = start_date_time,
    by = clock::duration_minutes(1),
    total_size = minutes
  )
  total_records_req <- length(hour_sequence)
  if (total_records_req > 1440) {
    stop(call. = FALSE,
         "The API only supports queries for a maximum 24hr interval.")
  }

  # the `station_code` is null here because we assemble it next due to issues
  # Windows accessing the API properly at the proper URL
  # TODO: Figure out how to fix this

  query_list <- .build_query(
    station_code = NULL,
    start_date_time = lubridate::format_ISO8601(start_date_time, usetz = "Z"),
    end_date_time = lubridate::format_ISO8601(
      hour_sequence[total_records_req], usetz = "Z"),
    api_key = api_key,
    api_group = NULL,
    interval = "minute",
    which_values = which_values,
    limit = total_records_req,
    include_closed = NULL
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
  out <- .query_dpird_api(
    .base_url = minute_base_url,
    .query_list = query_list,
    .limit = length(hour_sequence)
  )

  # TODO: extract nested data.frames using .parse_minute()

  .set_snake_case_names(out)

  out[, date_time := suppressMessages(
    lubridate::ymd_hms(out$date_time, tz = "Australia/Perth"))]
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

#' Parse minute data returned from DPIRD Weather 2.0 API
#'
#' Internal function that parses and tidies up data as returned by
#'  `.query_dpird_api()` for minute data
#'
#' @param .ret_list a list with the DPIRD weather API response
#' @param .which_values a character vector with the variables to query. See the
#' `get_dpird_minute()` for further details.
#'
#' @return a tidy `data.table` with station id and request weather summaries
#'
#' @noRd
#' @keywords Internal
#'
.parse_minute <- function(.ret_list,
                           .which_values) {

  parsed <- vector(mode = "list", length = length(.ret_list))

  for (i in seq_len(length(.ret_list))) {
    x <- jsonlite::fromJSON(.ret_list[[i]]$parse("UTF8"),
                              simplifyVector = TRUE)
    parsed[[i]] <- x$collection
  }

  if (nrow(parsed[[1]]) == 0) {
    return(message("There are no available minute data for this query."))
  }

  out <- data.table::rbindlist(parsed)

  # get the nested list columns and convert them to data.table objects
  col_classes <-
    vapply(out, class, FUN.VALUE = character(1))

  col_lists <- which(col_classes == "list")

  new_df_list <- vector(mode = "list", length = length(col_lists))
  names(new_df_list) <- names(col_lists)

  j <- 1
  for (i in col_lists) {
    # TODO: extract wind height or other as with station_code in get_summaries()
    for (j in seq_len(out[[i]])) {
      new_df_list[[j]] <-
       do.call(rbind,
                                  unlist(out[[i]][[j]],
                                         recursive = FALSE))
    }
  }


  # drop the column that's now in the new list to be added to `out`
  out[, names(new_df_list[j]) := NULL]
  return(cbind(out, do.call(what = cbind, args = new_df_list)))
}

