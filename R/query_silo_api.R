
#' Query the SILO API using {crul}
#' @param .station_code A `character` string of the \acronym{BOM} station code
#'   for the station of interest.
#' @param .start_date A `character` string representing the beginning of the
#'   range to query in the format 'yyyy-mm-dd' (ISO8601).  Will return data
#'   inclusive of this range.
#' @param .end_date A `character` string representing the end of the range query
#'   in the format 'yyyy-mm-dd' (ISO8601).  Will return data inclusive of this
#'   range.
#' @param .which_values A `character` string with the type of weather data to
#'   return.
#' @param .dataset A SILO dataset, either "PatchedPoint" or "DataDrill".
#' @param .api_key A valid e-mail address.
#'
#' @return A `data.table` of data for manipulating before returning to the user
#'
#' @noRd
#' @keywords internal

.query_silo_api <- function(.station_code = NULL,
                            .longitude = NULL,
                            .latitude = NULL,
                            .start_date,
                            .end_date,
                            .which_values,
                            .api_key,
                            .dataset) {
  base_url <- "https://www.longpaddock.qld.gov.au/cgi-bin/silo/"

  end_point <- data.table::fcase(
    .dataset == "PatchedPoint", "PatchedPointDataset.php",
    .dataset == "DataDrill", "DataDrillDataset.php"
  )

  if (.dataset == "PatchedPoint") {
    silo_query_list <- list(
      station = as.integer(.station_code),
      start = as.character(.start_date),
      finish = as.character(.end_date),
      format = "csv",
      comment = paste(.which_values, collapse = ""),
      username = .api_key,
      password = "api_request"
    )
  } else {
    silo_query_list <- list(
      longitude = as.integer(.longitude),
      latitude = as.integer(.latitude),
      start = as.character(.start_date),
      finish = as.character(.end_date),
      format = "csv",
      comment = paste(.which_values, collapse = ""),
      username = .api_key,
      password = "api_request"
    )
  }

  client <-
    crul::HttpClient$new(url = sprintf("%s%s", base_url, end_point))
  response <- client$get(query = silo_query_list)

  # check responses for errors
  # check to see if request failed or succeeded
  # - a custom approach this time combining status code,
  #   explanation of the code, and message from the server
  if (response$status_code > 201) {
    mssg <- response$parse("UTF-8")
    x <- response$status_http()
    stop(sprintf("HTTP (%s) - %s\n  %s", x$status_code, x$explanation, mssg),
         call. = FALSE)
  }
  response$raise_for_status()

  # the API won't return proper responses for malformed requests, so, we check
  # for the word "Sorry" and parse the response to the user if something slips
  # through our user checks.
  if (grepl("Sorry", response$parse("UTF8")) ||
      grepl("Request Rejected", response$parse("UTF8"))) {
    stop(call. = FALSE,
         gettextf(response$parse("UTF8")),
         domain = NA)
  }

  response_data <- data.table::fread(response$parse("UTF8"))

  response_data[, elev_m :=
                  trimws(gsub("elevation=", "",
                              response_data$metadata[
                                grep("elevation", response_data$metadata)]))]

  data.table::setnames(response_data, old = "YYYY-MM-DD", new = "date")
  response_data[, year := lubridate::year(date)]
  response_data[, month := lubridate::year(date)]
  response_data[, day := lubridate::year(date)]
  response_data[, extracted :=
                  lubridate::as_date(
                    trimws(gsub("extracted=", "",
                                response_data$metadata[
                                  grep("extracted", response_data$metadata)])))]

  if (.dataset == "PatchedPoint") {
    response_data[, station_code := sprintf("%06s", station)]
    response_data[, station := NULL]
    response_data[, station_name :=
                    trimws(gsub("name=", "",
                                response_data$metadata[
                                  grep("name", response_data$metadata)]))]
    response_data[, latitude :=
                    trimws(gsub("latitude=", "",
                                response_data$metadata[
                                  grep("latitude", response_data$metadata)]))]
    response_data[, longitude :=
                    trimws(gsub("longitude=", "",
                                response_data$metadata[
                                  grep("longitude", response_data$metadata)]))]
    .check_silo_codes(response_data)
  }

  # put columns in alphabetical order, then move others to front
  data.table::setcolorder(response_data, c(order(names(response_data))))

  data.table::setcolorder(response_data,
                          c("longitude",
                            "latitude",
                            "elev_m",
                            "date",
                            "year",
                            "month",
                            "day",
                            "extracted"))

  response_data[, metadata := NULL]
  return(response_data[])
}


#' Check SILO data codes
#'
#' Checks if any SILO data codes for interpolated data are present in the
#'   requested station observation data. If any such codes are found, a message
#'   will be reported with a suggestion to check the data source columns.  See
#'   `get_patched_point()` documentation for further details on codes and
#'   references.
#'
#' @param dt A `data.table`, defaults to the SILO API query result object from
#'   `.query_silo_api()`.
#'
#' @return An `invisible(NULL)`. This function returns no value, only a friendly
#'   message. It is used for checking and reporting the presence of interpolated
#'   data codes in the station observation data (for API queries performed using
#'   a station_code/code).
#'
#' @noRd
#' @keywords internal

.check_silo_codes <- function(dt) {

  # these are the only cols that we need to be concerned about being
  # interpolated
  primary_cols <- c(
    "daily_rain_source",
    "max_temp_source",
    "min_temp_source",
    "vp_source",
    "evap_pan_source"
  )

  dt <- dt[, .SD, .SDcols = primary_cols[primary_cols %in% names(dt)]]

  if (ncol(dt) > 0) {
    if (any(dt[, lapply(
      X = .SD,
      FUN = function(col)
        all(col == 0)
    )])) {
      # Report message
      message(
        "You have requested station observation data but some rows in this\n",
        "dataset have data codes for interpolated data.\n",
        "Check the 'data_source' columns and `get_patched_point()` or\n",
        "`get_data_drill()` documentation for further details on codes and\n",
        "references.\n"
      )
    }
  }
  return(invisible(NULL))
}
