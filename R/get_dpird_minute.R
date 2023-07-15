
#' Get DPIRD Weather Data by the Minute
#'
#' Fetch nicely formatted minute weather station data from the \acronym{DPIRD}
#'   Weather 2.0 \acronym{API} for a maximum 24-hour period.
#'
#' @param station_code A `character` string or `vector` of the \acronym{DPIRD}
#'   station code for the station of interest.
#' @param start_date_time A `character` string representing the start date and
#'   time of the query in the format \dQuote{yyyy-mm-dd-hh-mm} (ISO8601).
#'   Defaults to 24 hours before the current local system time, returning the
#'   most recent 24 hour observations rounded to the nearest minute.  This
#'   function does its best to decipher many date and time formats but prefers
#'   ISO8601.
#' @param minutes An `integer` value that provides the number of observations to
#'   be returned.  Defaults to 1440 minutes for 24 hours of observations.
#' @param values A `vector` of weather values to query from the
#'   \acronym{API}.  See **Available Values** section for valid available codes.
#'   Defaults to all available values, `all`.
#' @param api_key A `character` string containing your \acronym{API} key from
#'   \acronym{DPIRD}, <https://www.agric.wa.gov.au/web-apis>, for the
#'   \acronym{DPIRD} Weather 2.0 \acronym{API}.
#'
#' @section Available Values:
#'
#'   * all (includes all of the following),
#'   * airTemperature,
#'   * dateTime,
#'   * dewPoint,
#'   * rainfall,
#'   * relativeHumidity,
#'   * soilTemperature,
#'   * solarIrradiance,
#'   * wetBulb,
#'   * wind,
#'   * windAvgSpeed,
#'   * windMaxSpeed, and
#'   * windMinSpeed
#'
#' @note Please note this function converts date-time columns from Coordinated
#'   Universal Time \sQuote{UTC} returned by the \acronym{API} to Australian
#'   Western Standard Time \sQuote{AWST}.
#'
#' @return a [data.table::data.table] with `station_code` and the date interval
#'   queried together with the requested weather variables.
#'
#' @examples
#' \dontrun{
#'
#' get_dpird_minute(
#'   station_code = "SP",
#'   start_date_time = "2018-02-01 13:00:00",
#'   minutes = 1440,
#'   api_key = "your_api_key",
#'   values = c("airTemperature",
#'              "solarIrradiance",
#'              "wind")
#' )
#' }
#'
#' @family DPIRD
#' @family data fetching
#'
#' @author Adam H. Sparks \email{adam.sparks@@dpird.wa.gov.au}
#' @export

get_dpird_minute <- function(station_code,
                             start_date_time = lubridate::now() -
                               lubridate::hours(24L),
                             minutes = 1440L,
                             values = "all",
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

  if (any(values %notin% dpird_minute_values)) {
    if (values != "all") {
      stop(call. = FALSE,
           "You have specified a value not found in the 'API'.")
    }
  }

  if ("all" %in% values) {
    values <- dpird_minute_values
  } else {
    values <- c(values, "dateTime")
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

  query_list <- .build_query(
    station_code = NULL,
    start_date_time = lubridate::format_ISO8601(start_date_time, usetz = "Z"),
    end_date_time = lubridate::format_ISO8601(
      hour_sequence[total_records_req], usetz = "Z"),
    api_key = api_key,
    api_group = NULL,
    interval = "minute",
    values = values,
    limit = total_records_req,
    include_closed = NULL
  )

  return_list <- .query_dpird_api(
    .end_point = sprintf("%s/data", station_code),
    .query_list = query_list,
    .limit = length(hour_sequence)
  )

  out <- .parse_minute(.ret_list = return_list)

  # autoconvert numeric cols from character to numeric formats
  col_classes <-
    vapply(out, class, FUN.VALUE = character(1))
  out[, (which(col_classes == "character")) := lapply(.SD, utils::type.convert,
                                                      as.is = TRUE),
      .SDcols = which(col_classes == "character")]

  .set_snake_case_names(out)

  # convert dates
  out[, date_time := suppressMessages(
    lubridate::ymd_hms(out$date_time, tz = "Australia/Perth"))]

  out[, station_code := as.factor(station_code)]
  data.table::setkey(x = out, cols = station_code)

  data.table::setcolorder(out, c("station_code", "date_time"))
  return(out[])
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
#'  `.query_dpird_api()` for minute data.
#'
#' @param .ret_list a list with the DPIRD weather API response
#'
#' @return a tidy (long) `data.table` with station id and requested weather
#'  data.
#'
#' @noRd
#' @keywords Internal
#'
.parse_minute <- function(.ret_list) {
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
    new_df_list[[j]] <-
      data.table::rbindlist(lapply(out[[i]],
                                   function(x)
                                     as.data.frame(t(unlist(
                                       x
                                     )))))
    # drop the column that's now in the new list to be added to `out`
    out[, names(new_df_list[j]) := NULL]
    j <- j + 1
  }

  out <- cbind(out, do.call(what = cbind, args = new_df_list))

  if ("wind.height1" %in% names(out)) {
    out <- data.table::as.data.table(
      stats::reshape(
        out,
        idvar = "dateTime",
        direction = "long",
        varying = list(
          c(
            which(names(out) %in% "wind.avg.speed1"),
            which(names(out) %in% "wind.avg.speed2")
          ),
          c(
            which(names(out) %in% "wind.avg.direction.compassPoint1"),
            which(names(out) %in% "wind.avg.direction.compassPoint2")
          ),
          c(
            which(names(out) %in% "wind.avg.direction.degrees1"),
            which(names(out) %in% "wind.avg.direction.degrees2")
          ),
          c(
            which(names(out) %in% "wind.min.speed1"),
            which(names(out) %in% "wind.min.speed2")
          ),
          c(
            which(names(out) %in% "wind.max.speed1"),
            which(names(out) %in% "wind.max.speed2")
          )
        ),
        timevar = "wind.height",
        times = c(out$wind.height1[[1]],
                  out$wind.height2[[1]]),
        v.names = c(
          "wind.avg.speed",
          "wind.avg.direction.compassPoint",
          "wind.avg.direction.degrees",
          "wind.min.speed",
          "wind.max.speed"
        )
      )
    )

    out[, wind.height1 := NULL]
    out[, wind.height2 := NULL]
  }
  return(out)
}
