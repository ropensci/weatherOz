#
# file: /R/get_silo_points.R
#
# This file is part of the R-package weatherOz
#
# Copyright (C) 2023 DPIRD
#	<https://www.dpird.wa.gov.au>

#' Retrieve data from SILO (Scientific Information for Land Owners) API
#'
#' Download weather data from the \acronym{SILO} \acronym{API} from both
#'  station observations (DataDrill) and gridded data (PatchedPointData). There
#'  are three formats available: 'alldata' and 'apsim' with daily frequency and
#'  'monthly' with, that's right, monthly frequency.
#' @param station_id An integer, station number available at the \acronym{SILO}
#' the network.
#' @param latitude A vector, representing the latitude of a point-of-interest
#' @param longitude A vector, representing the longitude of a point-of-interest
#' @param first A string representing the start date of the query in the
#' format 'yyyymmdd' (ISO8601).
#' @param last A string representing the end date of the query in the
#' format 'yyyymmdd' (ISO8601).
#' @param data_format A string specifying the type of data to retrieve.
#' Limited to 'alldata', 'monthly' or 'apsim'. Note 'apsim' and 'alldata'
#' retrieve daily data.
#' @param email A string specifying a valid email address to use for the
#' request. The query will return an error if a valid email address is not
#' provided.
#'
#' @return A `data.table` containing the retrieved data from the \acronym{SILO}
#'  \acronym{API}.
#'
#' @family SILO
#'
#' @examplesIf interactive()
#' # Source observation data for station Wongan Hills station, WA (8137)
#' wd <- get_silo_points(station_id = 8137,
#'                       first = "20210601",
#'                       last = "20210701",
#'                       data_type = "alldata",
#'                       email = "your@@email")
#'
#' # Source data from latitude and longitude coordinates (gridded data)
#' # Southwood, WLD in the 'apsim' format.
#' wd <-
#' get_silo_points(latitude = -27.85,
#'                 longitude = 150.05,
#'                 first = "20221001",
#'                 last = "20221201",
#'                 data_format = "apsim",
#'                 email = "your@@email")
#' @export

get_silo_points <- function(station_id = NULL,
                            latitude = NULL,
                            longitude = NULL,
                            first = NULL,
                            last = NULL,
                            data_format = "alldata",
                            email = NULL) {

  if (missing(first)) stop("Provide a start date", call. = FALSE)
  if (missing(last)) stop("Provide an end date", call. = FALSE)
  if (is.null(email)) stop("Provide a valid email address", call. = FALSE)
  base_url <- "https://www.longpaddock.qld.gov.au/cgi-bin/silo/"

  .check_lonlat(longitude = longitude, latitude = latitude)

  # Retrieve data for queries with lat lon coordinates
  if (is.null(station_id) && !is.null(latitude) & !is.null(longitude)) {

    # Build query
    query_params <- list(lat = latitude, lon = longitude,
                         start = first, finish = last,
                         format = data_format,
                         username = email,
                         password = "api_request")

    # Query API and store output
    result <- httr::GET(
      url =  paste0(base_url, "DataDrillDataset.php"),
      query = query_params
    )
  }

  # Retrieve data for queries with station code
  if (is.null(latitude) & is.null(longitude) && !is.null(station_id)) {

    # Build query
    query_params <- list(station = station_id,
                         start = first, finish = last,
                         format = data_format,
                         username = email, password = "api_request")

    # Create query and store output
    result <- httr::GET(
      url =  paste0(base_url, "PatchedPointDataset.php"),
      query = query_params
    )
  }

  # Extract content and parse data according to the format and frequency
  r <- httr::content(result, "text")
  out <- .parse_silo(r, data_format, first)
  return(out[])
}


#' @title SILO API query parser
#' @description This function takes results from an API query to the SILO
#' database and formats it to a flat data.frame or a list of data.frames if
#' querying multiple sites. The function also converts character columns to date
#' and numerical classes, according to the data represented in the column.
#' Note that for the 'monthly' `data_format` the function also renames the
#' columns from "Yr.Mth", "Avg TMax (oC)", "Avg TMin (oC)", "Tot Rain (mm)"
#' "Tot Evap (mm)", "Avg Rad (MJ/m2)", "Avg VP (hPa)" to "year_month", "
#' tmax_avg", "tmin_avg", "total_rainfall", "total_evap", "radiation_avg" and
#' "vapour_pressure_avg", respectively.
#' @param query_response data returned by the SILO API, usually a list of
#' character data.
#' @param this_format A string, user defined by the query details. One of
#' 'alldata', 'apsim' and 'monthly'. Internally inherited from
#' `get_silo_points()`.
#' @param this_date A string, user defined by the query details and represents
#' the start date of the query. Internally inherited from `get_silo_points()`.
#' @return A data.frame with date class column(s) and numeric class columns for
#' the weather variables.
#' @keywords internal
#'
#' @noRd

.parse_silo <- function(query_response,
                        this_format,
                        this_date) {
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
  }

  # monthly data
  if (this_format == 'monthly') {
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
      out[j,] <- data.table::setDT(unlist(strsplit(df[j], "\\s+")))
    }

    # Set date columns to date class
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
  }
  return(data.table::setDT(out))
}

