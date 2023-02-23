#
# file: /R/tidy_weather_summary.R
#
# This file is part of the R-package wrapique
#
# Copyright (C) 2021 DPIRD
#	<https://www.dpird.wa.gov.au>

#' Individual station summaries nicely formatted.
#' @param site A string of the station ID code for the station of interest.
#' Passed through from `get_summaries` function.
#' @param first The date on which the weather data summary should start.
#' @param last The date on which the weather data summary should end.
#' @param api_key Api key from DPIRD (https://www.agric.wa.gov.au/web-apis).
#' Defaults to NULL.
#' @param interval Time interval to summarise over.
#' Default is 'daily'; others are '15min', '30min', 'hourly',
#' 'monthly', 'yearly'.For intervals shorter than 1 day, time period covered
#' will be midnight to midnight, with the last time interval being before
#' midnight - hour/minute values are for the end of the time period.
#' Data for shorter intervals ('15min', '30min') should be available from
#' January of last year
#' @param api_name Defaults to "weather", Only works with DPIRD's Weather API.
#' @param api_version Defaults to 2, and gives a error if not 2.
#' @param which_vars Match weather summary selected. Defaults to "all".
#' Can be one of "all", "rain", "wind", "temp" and "erosion."
#' @return a `data frame` with site and date interval queried together with
#' requested weather summary/summaries.
#' @examples
#' # You must have an DPIRD API key to proceed
#' mykey <- 'dpird_api_key'
#'
#' # Set date interval for yearly request
#' # Get rainfall summary
#' start.date <- "2017-10-28"
#'
#' # Use default for end data (current system date)
#' output <- tidy_weather_summary(
#'             site = "CL001",
#'             first = start.date,
#'             api_key = mykey,
#'             interval = "yearly",
#'             which_vars = "rain")
#'
#' # Only for wind and erosion conditions for daily time interval
#' # define start and end date
#' start.date <- "2022-05-01"
#' end.date <- "2022-05-02"
#'
#' output <- tidy_weather_summary(
#'             site = "BI",
#'             first = start.date,
#'             last = end.date,
#'             api_key = mykey,
#'             interval = "daily",
#'             which_vars = c("wind", "erosion"))
#'
#' @export

tidy_weather_summary <- function(
    site = NULL,
    first,
    last = Sys.Date(),
    api_key = NULL,
    interval = c("daily", "15min", "30min", "hourly", "monthly", "yearly"),
    api_name = "weather",
    api_version = 2,
    which_vars = c("all", "rain", "wind", "temp", "erosion")) {

  # Error if not the Weather API or the latest version
  stopifnot(api_name == "weather", api_version == 2)

  # Function can only hand one station at time at this stage
  if (length(site) > 1)
    stop("Multiple stations not currently supported (function still in development)")

  # Get summaries based on user query
  df <-
    get_summaries(
      site = site,
      first = first,
      last = last,
      api_key = api_key,
      interval = interval
    )

  # Get query time interval
  out_period <- df$summaries$period

  # Remove empty columns (eg minute for hourly summaries) and grab number of
  # records in the data collection
  out_period <- out_period[, !apply(is.na(out_period), 2, all)]
  nrec <- nrow(out_period)

  if (any(c("all", "temp") %in% which_vars)) {
    out_temp <- df$summaries$airTemperature
    names(out_temp) <- paste0("airtemp.", names(out_temp))

  } else {
    out_temp <- data.frame()[1:nrec, ]
  }

  if (any(c("all", "rain") %in% which_vars)) {
    out_rain <- df$summaries$rainfall

  } else {
    out_rain <- data.frame()[1:nrec, ]
  }

  if (any(c("all", "wind") %in% which_vars)) {
    temp <- df$summaries$wind

    # Data for 15 min, 30 min, and hourly are nested in `avg` and `max` lists
    if (any(c("15min", "30min", "hourly") %in% interval)) {

      # If a given station has both 3 and 10m
      if (nrow(temp[[1]][1]) == 2) {
        wind <- purrr::map_df(temp, cbind)
        wind <- suppressMessages({tidyr::unnest(wind, cols = c(avg, max), names_repair = "unique")})
        wind <- suppressMessages({tidyr::unnest(wind, cols = c(direction...3, direction...6), names_repair = "unique")})
        wind <- dplyr::rename(wind,
                              wind.height = 1,
                              wind.avg.speed = 2,
                              wind.avg.direction.degrees = 3,
                              wind.avg.direction.compass.point = 4,
                              wind.max.speed = 5,
                              wind.max.time = 6,
                              wind.max.direction.degrees = 7,
                              wind.max.direction.compass.point = 8)

        wind <- dplyr::group_split(wind, wind.height)

        name_list <- c(paste0("wind.", names(unlist(wind[[1]][1, ])), ".3m"),
                       paste0("wind.", names(unlist(wind[[1]][1, ])), ".10m"))

        out_wind <- cbind(wind[[1]], wind[[2]])
        names(out_wind) <- name_list

        # 10 m data, even if empty
      } else if (nrow(temp[[1]][1]) == 1) {

        # 3 m data
        name_list <- paste0("wind.", names(unlist(temp[[1]][1, ])), ".3m")
        wind_3m <- stats::setNames(
          as.data.frame(
            matrix(unlist(temp),
                   ncol = length(name_list),
                   byrow = TRUE),
            stringsAsFactors = FALSE),
          nm = name_list)


        name_list_10m <- paste0("wind.", names(unlist(temp[[1]][1, ])), ".10m")
        wind_3m[name_list_10m] <- NA_character_

        out_wind <- wind_3m
      }
    }

    # Data for daily, monthly, yearly
    else if (!any(c("15min", "30min, hourly") %in% interval)) {

      if (nrow(temp[[1]][1]) == 2) {
        wind <- purrr::map_df(temp, cbind)
        wind <- suppressMessages({tidyr::unnest(wind, cols = c(avg, max), names_repair = "unique")})
        wind <- suppressMessages({tidyr::unnest(wind, cols = c(direction), names_repair = "unique")})
        wind <- dplyr::rename(wind,
                              wind.height = 1,
                              wind.avg.speed = 2,
                              wind.max.speed = 3,
                              wind.max.time = 4,
                              wind.max.direction.degrees = 5,
                              wind.max.direction.compass.point = 6)

        wind <- dplyr::group_split(wind, wind.height)

        name_list <- c(paste0("wind.", names(unlist(wind[[1]][1, ])), ".3m"),
                       paste0("wind.", names(unlist(wind[[1]][1, ])), ".10m"))

        out_wind <- cbind(wind[[1]], wind[[2]])
        names(out_wind) <- name_list

        # 10 m data, even if empty
      } else if (nrow(temp[[1]][1]) == 1) {

        # 3 m data
        name_list <- paste0("wind.", names(unlist(temp[[1]][1, ])), ".3m")
        wind_3m <- stats::setNames(
          as.data.frame(
            matrix(unlist(temp),
                   ncol = length(name_list),
                   byrow = TRUE),
            stringsAsFactors = FALSE),
          nm = name_list)


        name_list_10m <- paste0("wind.", names(unlist(temp[[1]][1, ])), ".10m")
        wind_3m[name_list_10m] <- NA_character_

        out_wind <- wind_3m
      }
    }
      # if wind was not requested
    } else {
      out_wind <- data.frame()[1:nrec, ]
    }

  if (any(c("all", "erosion") %in% which_vars)) {
    out_erosion <- df$summaries$erosionCondition
    names(out_erosion) <- paste0("wind_erosion_", names(out_erosion))

  } else {
    out_erosion <- data.frame()[1:nrec, ]
  }

  ret <- data.frame(site = df$stationCode, out_period, out_temp,
                    rain = out_rain, out_wind, out_erosion)
  names(ret) <- tolower(names(ret))

  return(ret)
}
