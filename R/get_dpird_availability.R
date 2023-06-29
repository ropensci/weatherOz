
#' Get the Availability for DPIRD Weather Stations
#'
#' Fetch the availability metadata of weather stations in the \acronym{DPIRD}
#'   weather station network from the Weather 2.0 \acronym{API}.
#'
#' @param station_code A `character` string of the \acronym{DPIRD} station code
#'   for the station of interest. Defaults to `NULL`, returning metadata for
#'   all stations during the requested \var{start_date} and \var{end_date}
#'   interval.
#' @param start_date A `character` string representing the beginning of the
#'   range to query in the format \dQuote{yyyy-mm-dd} (ISO8601).  This function
#'   will return data inclusive of this range.  Defaults to `NULL`, returning
#'   data for the current year-to-date.  Must be sent along with an
#'   \var{end_date}.
#' @param end_date A `character` string representing the end of the range query
#'   in the format \dQuote{yyyy-mm-dd} (ISO8601).  This function will return
#'   data inclusive of this range.  Defaults to `NULL`, returning data for the
#'   current year-to-date.  Must be sent with a \var{start_date}.
#' @param values A `character` string with the type of availability metadata to
#'   return.  See **Available Values** for a full list of valid values.
#'   Defaults to `availability`, returning metadata for all stations.
#' @param api_key A `character` string containing your \acronym{API} key from
#'   \acronym{DPIRD}, <https://www.agric.wa.gov.au/web-apis>, for the
#'   \acronym{DPIRD} Weather 2.0 \acronym{API}.
#'
#' @section Available Values:
#'
#'   * availability (which will return all of the following values),
#'   * availabilityCurrentHour,
#'   * availabilityLast7DaysSince9AM,
#'   * availabilityLast7DaysSince12AM,
#'   * availabilityLast14DaysSince9AM,
#'   * availabilityLast14DaysSince12AM,
#'   * availabilityLast24Hours,
#'   * availabilityMonthToDateSince12AM,
#'   * availabilityMonthToDateTo9AM,
#'   * availabilitySince9AM,
#'   * availabilitySince12AM,
#'   * availabilityTo9AM,
#'   * availabilityYearToDateSince12AM, and
#'   * availabilityYearToDateTo9AM
#'
#' @return a [data.table::data.table]  with `station_code` and the requested
#'   metadata.
#'
#' @family DPIRD
#' @family metadata
#'
#' @author Adam H. Sparks \email{adam.sparks@@dpird.wa.gov.au}
#'
#' @examples
#' \dontrun{
#' # You must have a DPIRD API key to proceed
#' # Use default for end data (current system date)
#' output <- get_dpird_availability(
#'             station_code = "BI",
#'             start_date = "20170101",
#'             end_date = "20171231",
#'             api_key = "your_api_key")
#' }
#'
#' @export

get_dpird_availability <-
  function(station_code = NULL,
           start_date = NULL,
           end_date = NULL,
           values = "availability",
           api_key) {
    # Error if api_key is not provided
    if (missing(api_key)) {
      stop(
        "A valid DPIRD API key must be provided, please visit\n",
        "<https://www.agric.wa.gov.au/web-apis> to request one.\n",
        call. = FALSE
      )
    }

    # validate user provided dates
    if (!is.null(start_date)) {
      if (is.null(end_date)) {
        stop(call. = FALSE,
             "A custom 'end_date' must be supplied with the 'start_date'")
      }
      start_date <- .check_date(start_date)
    }
    if (!is.null(end_date)) {
      if (is.null(start_date)) {
        stop(call. = FALSE,
             "A custom 'start_date' must be supplied with the 'end_date'")
      }
      end_date <- .check_date(end_date)
      .check_date_order(start_date, end_date)
    }

    # set up "&select=values"
    ## if 'start_date' is not set, we append station_code and station_name
    values <- c(values, "stationCode", "stationName")

    ## if 'start_date' is specified, we only request the availability for the
    ## period and the station_code and station_name
    if (!is.null(start_date)) {
      values <-
        c("stationCode", "stationName", "availabilityPeriod")
    }

    if (!is.null(station_code)) {
      query_list <- list(
        select = paste(values, collapse = ","),
        stationCode = paste(station_code, collapse = ","),
        startDate = start_date,
        endDate = end_date,
        api_key = api_key
      )
    } else {
      query_list <- list(
        select = paste(values, collapse = ","),
        startDate = start_date,
        endDate = end_date,
        api_key = api_key
      )
    }


    return_list <- .query_dpird_api(.end_point = "availability",
                                    .query_list = query_list,
                                    .limit = 1000)

    out <- .parse_availability(.ret_list = return_list,
                               .start_date = start_date)

    .set_snake_case_names(out)

    if (!is.null(start_date)) {
      out[, start_date := start_date]
      out[, end_date := end_date]
      data.table::setnames(
        out,
        old = c("9_am", "12_am"),
        new = c("availability_since_9_am",
                "availability_since_12_am")
      )

      data.table::setcolorder(out,
                              c("station_code",
                                "station_name",
                                "start_date",
                                "end_date"))
    }

    data.table::setcolorder(out,
                            c("station_code",
                              "station_name"))

    data.table::setkey(x = out, cols = station_code)

    return(out)
  }

#' Parse DPIRD API availability data
#'
#' Internal function that parses and tidy up data as returned by
#'  `.query_dpird_api()`
#'
#' @param .ret_list a list with the DPIRD weather API response
#'
#' @return a tidy `data.table` with station id and requested availability
#'  metadata
#'
#' @noRd
#' @keywords Internal
#'
.parse_availability <- function(.ret_list, .start_date) {
  x <- jsonlite::fromJSON(.ret_list[[1]]$parse("UTF8"),
                          simplifyVector = TRUE)

  # start with no specific period requested and parse the resulting df, easy
  if (is.null(.start_date)) {
    y <- data.table::as.data.table(
      list(
        stationCode = x$collection$stationCode,
        stationName = x$collection$stationName
      )
    )

    out <- data.table::as.data.table(
      cbind(y, data.table::as.data.table(x$collection$availability)))

    out[, period := NULL]

  } else {
    y <- data.table::as.data.table(
      list(
        stationCode = x$collection$stationCode,
        stationName = x$collection$stationName
      )
    )

    out <- data.table::as.data.table(cbind(y, x$collection$availability$period))
  }
  return(out)
}
