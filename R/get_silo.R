#
# file: /R/get_silo.R
#
# This file is part of the R-package weatherOz
#
# Copyright (C) 2023 DPIRD
#	<https://www.dpird.wa.gov.au>

#' Get weather data from SILO (Scientific Information for Land Owners) API
#'
#' Download weather data from the \acronym{SILO} \acronym{API} from both
#'   station observations (DataDrill) and gridded data (PatchedPointData). There
#'   are three formats available: 'alldata' and 'apsim' with daily frequency and
#'   'monthly' with, that's right, monthly frequency. Queries with
#'   `station_code` return stations observations from the 'DataDrill' endpoint
#'   while queries with `latitude` and `longitude` coordinates return gridded
#'   data from the 'PatchedPointData'.
#'
#' @param station_code An `integer` representing the station number
#'   available from the \acronym{SILO} network. Defaults to `NULL` and when
#'   used, queries with latitude and longitude input are not permitted.
#' @param latitude A single `numeric` value representing the latitude of the
#'   point-of-interest. Defaults to `NULL` and when used, queries with
#'   `station_code` input are not permitted. Requires `longitude` to be
#'   provided.
#' @param longitude A single `numeric` value  representing the longitude of the
#'    point-of-interest. Defaults to `NULL` and when used, queries with
#'    `station_code` inputs are not permitted.  Requires `latitude` to be
#'    provided.
#' @param start_date A `character` string representing the start date of the
#'    query in the format 'yyyymmdd'.
#' @param end_date A `character` string representing the start date of the query
#'    in the format 'yyyymmdd'.  Defaults to the current system date.
#' @param data_format A `character` string specifying the type of data to
#'   retrieve.  Limited to 'alldata', 'monthly' or 'apsim'. Note 'apsim' and
#'   'alldata' retrieve daily data.
#' @param email A `character `string specifying a valid email address to use
#'   for the request. The query will return an error if a valid email address is
#'   not provided.
#'
#' @return A [data.table::data.table] containing the retrieved data from the
#'   \acronym{SILO} \acronym{API}.
#'
#' @family SILO
#'
#' @examplesIf interactive()
#' # Source observation data for station Wongan Hills station, WA (8137)
#' wd <- get_silo(station_code = 8137,
#'                start_dte = "20210601",
#'                end_date = "20210701",
#'                data_format = "alldata",
#'                email = "your@@email")
#'
#' # Source data from latitude and longitude coordinates (gridded data)
#' # Southwood, QLD in the 'apsim' format.
#' wd <- get_silo(latitude = -27.85,
#'                longitude = 150.05,
#'                start_date = "20221001",
#'                end_date = "20221201",
#'                data_format = "apsim",
#'                email = "your@@email")
#'
#' @export get_silo

get_silo <- function(station_code = NULL,
                     latitude = NULL,
                     longitude = NULL,
                     start_date,
                     end_date = Sys.Date(),
                     data_format = "alldata",
                     email) {
  if (missing(start_date))
    stop("Provide a start date", call. = FALSE)
  if (missing(email))
    stop("Provide a valid email address", call. = FALSE)
  stopifnot("Provide equal length lat and lon values" =
              length(latitude) == length(longitude))

  # validate user provided date
  start_date <- gsub("-", "", .check_date(start_date))
  end_date <-  gsub("-", "", .check_date(end_date))
  .check_date_order(start_date, end_date)

  # query a single point and return the values ----
  # if a single station or single lat/lon is requested, return values, else
  # check vectors for validity and then return all values in one data.table
  if (length(station_code) == 1 || length(latitude) == 1) {
    if (is.null(station_code)) {
      .check_lonlat(longitude = longitude, latitude = latitude)
      return(
        .query_silo(
          .station_code = station_code,
          .latitude = latitude,
          .longitude = longitude,
          .first = first,
          .last = last,
          .data_format = data_format,
          .email = email
        )
      )

    } else {
      return(
        .query_silo(
          .station_code = station_code,
          .latitude = latitude,
          .longitude = longitude,
          .first = first,
          .last = last,
          .data_format = data_format,
          .email = email
        )
      )
    }
  }
}

#' Construct and send SILO API queries
#'
#' Internal function to construct, send, receive and return the parsed API
#' response.
#'
#' @param station_code `Integer`, An integer or vector of integers representing
#'  station number(s) available from the \acronym{SILO} network.
#' @param latitude `Numeric`. A single value or a vector, representing the
#'  latitude(s) of the point(s)-of-interest.
#' @param longitude `Numeric`. A single value or vector, representing the
#'  longitude(s) of the point(s)-of-interest.
#' @param first `Integer`. A string representing the start date of the query in
#'  the format 'yyyymmdd' (ISO-8601).
#' @param last `Integer`. A string representing the end date of the query in the
#' format 'yyyymmdd' (ISO-8601).
#' @param data_format `Character`. A string specifying the type of data to
#'  retrieve.  Limited to 'alldata', 'monthly' or 'apsim'. Note 'apsim' and
#'  'alldata' retrieve daily data.
#' @param email `Character`. A string specifying a valid email address to use
#'  for the request. The query will return an error if a valid email address is
#'  not provided.
#'
#' @examples
#' .query_silo(station_code = 8137)
#'
#' @noRd
#' @keywords Internal
.query_silo <- function(.station_code = station_code,
                        .latitude = latitude,
                        .longitude = longitude,
                        .first = first,
                        .last = last,
                        .data_format = data_format,
                        .email = email) {
  base_url <- "https://www.longpaddock.qld.gov.au/cgi-bin/silo/"

  # Retrieve data for queries with lat lon coordinates
  if (is.null(.station_code) &&
      !is.null(.latitude) & !is.null(.longitude)) {
    # Build query
    query_params <- list(
      lat = .latitude,
      lon = .longitude,
      start = .first,
      finish = .last,
      format = .data_format,
      username = .email,
      password = "api_request"
    )

    # Query API and store output
    result <-
      httr::GET(url =  paste0(base_url, "DataDrillDataset.php"),
                query = query_params)
  }

  # Retrieve data for queries with station code
  if (is.null(.latitude) &
      is.null(.longitude) && !is.null(.station_code)) {
    # Build query
    query_params <- list(
      station = .station_code,
      start = .first,
      finish = .last,
      format = .data_format,
      username = .email,
      password = "api_request"
    )

    # Create query and store output
    result <-
      httr::GET(url =  paste0(base_url, "PatchedPointDataset.php"),
                query = query_params)
  }

  # Extract content and parse data according to the format and frequency
  r <- httr::content(result, "text")
  out <- .parse_silo(r, .data_format, .first, .station_code)
  return(out[])
}


#' SILO API query parser
#' Takes results from an API query to the SILO database and formats it to a
#' flat `data.table` or a list object of `data.tables` if querying multiple
#' sites. The function also converts character columns to date and numerical
#' classes, according to the data represented in the column.
#' @note For the 'monthly' `data_format` the function also renames the
#' columns from "Yr.Mth", "Avg TMax (oC)", "Avg TMin (oC)", "Tot Rain (mm)"
#' "Tot Evap (mm)", "Avg Rad (MJ/m2)", "Avg VP (hPa)" to "year_month", "
#' tmax_avg", "tmin_avg", "total_rainfall", "total_evap", "radiation_avg" and
#' "vapour_pressure_avg", respectively.
#' @param query_response data returned by the SILO API, usually a list of
#' character data.
#' @param this_format A string, user defined by the query details. One of
#' 'alldata', 'apsim' and 'monthly'. Internally inherited from
#' `get_silo()`.
#' @param this_date A string, user defined by the query details and represents
#' the start date of the query. Internally inherited from `get_silo()`.
#' @param station_code A string, user defined by the query details and represents
#' the station code. Internally inherited from `get_silo()`.
#' @return A `data.table` with date class column(s) and numeric class columns
#' for the weather variables.
#' @keywords internal
#' @noRd

.parse_silo <- function(query_response,
                        this_format,
                        this_date,
                        station_code) {
  Date <- Date2 <- NULL #nocov

  # apsim data
  if (toupper(this_format) == "APSIM") {
    # code snippet from {cropgrowdays}
    df <- strsplit(as.character(query_response), "\nyear")
    df <- as.character(df[[1]][2])
    df <- unlist(strsplit(df, "\n"))[-2]
    df <- gsub("\\s+", " ", df)
    out <- utils::read.delim(textConnection(df), sep = " ")
    names(out)[1] <- "year"
    out$date <-
      as.Date(out[, "day"] - 1, paste0(out[, "year"], "-01-01"))
    out <- data.table::setDT(out)
  }

  # monthly data
  if (this_format == "monthly") {
    df <- unlist(strsplit(query_response, "\n"))
    n_first <-
      grep(format(lubridate::as_date(this_date), "%Y%m"), df)
    this_names <- c(
      "year_month",
      "tmax_avg",
      "tmin_avg",
      "total_rainfall",
      "total_evap",
      "radiation_avg",
      "vapour_pressure_avg"
    )

    # Create df and give names
    df <- df[n_first:length(df)]
    out <- stats::setNames(data.frame(matrix(
      nrow = length(df),
      ncol = length(this_names)
    )),
    nm = this_names)
    # Add data
    for (j in 1:length(df)) {
      out[j,] <- unlist(strsplit(df[j], "\\s+"))
    }

    # All columns were parsed as char, fix it
    # Select columns that have only numbers as obs
    num_cols <- unlist(lapply(names(out),
                              function(x)
                                all(grepl(
                                  "^[-0-9.]+$", out[[x]]
                                ))))
    # grab the column with dates
    date_col <- unlist(lapply(out,
                              function(x)
                                any(
                                  class(out) %in% c("POSIXct",
                                                    "POSIXt",
                                                    "Date")
                                )))
    # Put them together
    numeric_columns <- num_cols & !date_col
    for (i in which(numeric_columns)) {
      out[[i]] <- as.numeric(as.character(out[[i]]))
    }
    out <- data.table::setDT(out)
  }

  # 'alldata' data (complete data with quality colums)
  if (this_format == "alldata") {
    df <- unlist(strsplit(query_response, "\n"))
    n_names <- grep("Date", df)
    n_first <- grep(this_date, df)
    this_names <- unlist(strsplit(df[n_names], "\\s+"))

    # Create df and provide names
    df <- df[n_first:length(df)]
    out <- stats::setNames(data.frame(matrix(
      nrow = length(df),
      ncol = length(this_names)
    )),
    nm = this_names)
    # Add data
    for (j in 1:length(df)) {
      out[j,] <- unlist(strsplit(df[j], "\\s+"))
    }

    # Set date columns to date class
    out <- data.table::data.table(out)
    out[, Date := as.Date(out$Date, format = "%Y%m%d")]
    out[, Date2 := as.Date(out$Date2, format = "%d-%m-%Y")]

    # All columns were parsed as char, fix it
    # Select columns that have only numbers as obs
    num_cols <- unlist(lapply(names(out),
                              function(x)
                                all(grepl(
                                  "^[-0-9.]+$", out[[x]]
                                ))))

    # grab the column with dates
    date_col <- unlist(lapply(out,
                              function(x)
                                any(
                                  class(x) %in% c("POSIXct",
                                                  "POSIXt",
                                                  "Date")
                                )))
    # Put them together
    numeric_columns <- num_cols & !date_col
    for (i in which(numeric_columns)) {
      out[[i]] <- as.numeric(as.character(out[[i]]))
    }
    out <- data.table::setDT(out)
  }

  # if querying station observation data, check data
  # codes for the presence of interpolated data
  if (!is.null(station_code)) {
    .check_silo_codes(out, this_format)
  }

  return(out)
}

#' Check SILO data codes
#'
#' Checks if any SILO data codes for interpolated data are present in the
#' requested station observation data. If any such codes are found, a message
#' will be reported with a suggestion to check the data source columns
#' and `get_silo()` documentation for further details on codes and references.
#'
#' @param dt A `data.table`, defaults to the SILO API query result object from
#' `.query_silo()`.
#' @param .this_format A string specifying the format of the input
#'   data. Valid values are 'alldata' and 'apsim'. Default is to 'this_format'
#'   variable passed to the `data_format` argument in `get_silo()`.
#'
#' @return An `invisible(NULL)`. This function returns no value, only a friendly
#' message. It is used for checking and reporting the presence of interpolated
#' data codes in the station observation data (for API queries performed using a
#' station_code/code).
#'
#' @noRd
.check_silo_codes <- function(dt,
                              .this_format = this_format) {
  if (.this_format == "alldata") {
    code_cols <- c("Smx",
                   "Smn",
                   "Srn",
                   "Sev",
                   "Ssl",
                   "Svp",
                   "Ssp",
                   "Ses",
                   "Sp")

    # Count the number of non-zero rows for each new column
    non_zero_counts <-
      dt[, lapply(.SD, function(col)
        sum(col != 0)),
        .SDcols = code_cols]

  } else {
    # Split the 'code' column into separate columns for each quality code
    code_cols <- c("Ssl", "Smx", "Smn", "Srn", "Sev", "Svp")
    x <- data.table::copy(dt)
    x[, (code_cols) := data.table::tstrsplit(as.character(code),
                                             "",
                                             fixed = TRUE,
                                             type.convert = TRUE)]

    # Count the number of non-zero rows for each new column
    non_zero_counts <- x[, lapply(.SD, function(col)
      sum(col != 0)),
      .SDcols = code_cols]
  }

  if (any(non_zero_counts > 0)) {
    # Report message
    message(
      "\nYou have requested station observation data but rows in this dataset \n",
      "have data codes of interpolated data. Check the data source columns and\n",
      "`get_silo()` documentation for further details on codes and references.\n"
    )
  }
  return(invisible(NULL))
}
