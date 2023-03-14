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
#'
#' @param station_id `Integer`, An integer or vector of integers representing
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
#'                       data_format = "alldata",
#'                       email = "your@@email")
#'
#' # Source data from latitude and longitude coordinates (gridded data)
#' # Southwood, WLD in the 'apsim' format.
#' wd <- get_silo_points(latitude = -27.85,
#'                       longitude = 150.05,
#'                       first = "20221001",
#'                       last = "20221201",
#'                       data_format = "apsim",
#'                       email = "your@@email")
#'
#' # using multiple station IDs or locations
#'
#' # start multisession
#' future::plan("multisession")
#' # Query multiple stations (BoM, by code)
#' my_stations <- c(65030, 89033, 39083, 8061)
#' wd <- get_multi_silo_points(station_id = my_stations,
#'                             first = "20220401",
#'                             last = "20221001",
#'                             data_format = "monthly",
#'                             email = "YOUR_EMAIL_ADDRESS")
#'
#' # Query multiple stations (using latitude and longitude coordinates)
#' mylat <- c(-33.512, -30.665, -27.309, -24.237)
#' mylon <- c(115.821, 117.487, 119.649, 121.584)
#' wd <- get_multi_silo_points(latitude = mylat,
#'                             longitude = mylon,
#'                             first = "202101201",
#'                             last = "20220228",
#'                             data_format = "alldata",
#'                             email = "YOUR_EMAIL_ADDRESS")
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
  stopifnot("Provide equal length lat and lon values" =
              length(latitude) == length(longitude))

  # if a single station or single lat/lon is requested, return values, else
  # check vectors for validity and then return all values in one data.table
  if (length(station_id) == 1 || length(latitude) == 1) {
    .check_lonlat(longitude = longitude, latitude = latitude)
    return(.query_silo())
  } else {
    if (!is.null(latitude)) {
      .v_check_lonlat <-
        Vectorize(.check_lonlat, vectorize.args = c("longitude", "latitude"))
      invisible(.v_check_lonlat(longitude = longitude, latitude = latitude))
    }

    if (is.null(latitude) & is.null(longitude) && !is.null(station_id)) {

      weather_raw <- furrr::future_map(
        .x = station_id,
        .f = purrr::possibly(
          ~ get_silo_points(
            station_id = .x,
            latitude = latitude,
            longitude = longitude,
            first = first,
            last = last,
            data_format = data_format,
            email = email),
          otherwise = "Error in acquiring weather data for this station.\n"
        ),
        .options = furrr::furrr_options(seed = NULL)
      )

      # add the location to the weather data
      names(weather_raw) <- station_id
      out <- Map(cbind,
                 station_id = names(weather_raw),
                 weather_raw)
    }

    if (is.null(station_id) && !is.null(latitude) & !is.null(longitude)) {

      weather_raw <- furrr::future_map2(
        .x = latitude,
        .y = longitude,
        .f = purrr::possibly(
          ~ get_silo_points(
            latitude = .x,
            longitude = .y,
            first = first,
            last = last,
            data_format = data_format,
            email = email),
          otherwise = "Error in acquiring weather data for this station.\n"
        ),
        .options = furrr::furrr_options(seed = NULL)
      )

      # provide unique names
      names(weather_raw) <- paste0(unique(latitude), "_", unique(longitude))

      # add the location to the weather data
      weather_raw <- Map(cbind,
                         latlon = names(weather_raw),
                         weather_raw)

      out <-
        lapply(weather_raw, function(i) {
          latlon_sep <- strsplit(i$latlon, "_")[[1]]
          i$latitude <- latlon_sep[1]
          i$longitude <- latlon_sep[2]
          i$latlon <- NULL
          return(i)
        })
    }
    return(out)
  }
}


#' Construct and send SILO API queries
#'
#' Internal function to construct, send, receive and return the parsed API
#' response.
#'
#' @param station_id `Integer`, An integer or vector of integers representing
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
#' .query_silo(station_id = 8137)
#'
#' @noRd
#' @keywords Internal
.query_silo <- function(station_id = station_id,
                        latitude = latitude,
                        longitude = longitude,
                        first = first,
                        last = last,
                        data_format = data_format,
                        email = email) {
  base_url <- "https://www.longpaddock.qld.gov.au/cgi-bin/silo/"

  # Retrieve data for queries with lat lon coordinates
  if (is.null(station_id) &&
      !is.null(latitude) & !is.null(longitude)) {

    # Build query
    query_params <- list(
      lat = latitude,
      lon = longitude,
      start = first,
      finish = last,
      format = data_format,
      username = email,
      password = "api_request"
    )

    # Query API and store output
    result <- httr::GET(url =  paste0(base_url, "DataDrillDataset.php"),
                        query = query_params)
  }

  # Retrieve data for queries with station code
  if (is.null(latitude) &
      is.null(longitude) && !is.null(station_id)) {
    # Build query
    query_params <- list(
      station = station_id,
      start = first,
      finish = last,
      format = data_format,
      username = email,
      password = "api_request"
    )

    # Create query and store output
    result <- httr::GET(url =  paste0(base_url, "PatchedPointDataset.php"),
                        query = query_params)
  }

  # Extract content and parse data according to the format and frequency
  r <- httr::content(result, "text")
  out <- .parse_silo(r, data_format, first)
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
#' `get_silo_points()`.
#' @param this_date A string, user defined by the query details and represents
#' the start date of the query. Internally inherited from `get_silo_points()`.
#' @return A `data.table` with date class column(s) and numeric class columns
#' for the weather variables.
#' @keywords internal
#' @noRd

.parse_silo <- function(query_response,
                        this_format,
                        this_date) {

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
  }
  return(data.table::setDT(out))
}

