
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

  if (.dataset == "PatchedPoint") {
    response_data[, station_name :=
                    trimws(gsub("name=", "",
                                response_data$metadata[
                                  grep("name", response_data$metadata)]))]
  }
  response_data[, latitude :=
                  trimws(gsub("latitude=", "",
                              response_data$metadata[
                                grep("latitude", response_data$metadata)]))]
  response_data[, longitude :=
                  trimws(gsub("longitude=", "",
                              response_data$metadata[
                                grep("longitude", response_data$metadata)]))]
  response_data[, elev_m :=
                  trimws(gsub("elevation=", "",
                              response_data$metadata[
                                grep("elevation", response_data$metadata)]))]
  response_data[, metadata := NULL]
  response_data[, station := sprintf("%06d", station)]
}
