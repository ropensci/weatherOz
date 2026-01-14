#' Get Weather Forecast Data from MET Weather API
#'
#' Retrieves weather forecast data from the Norwegian Meteorological Institute
#' locationforecast API and returns the parsed metadata together with a tidy hourly
#' `data.table`. The response headers `Expires` and `Last-Modified` are provided
#' both in their raw RFC 1123 form and parsed to `POSIXct` for downstream logic.
#'
#' @param latitude Numeric. Latitude in decimal degrees for the forecast location.
#'   Must be within Australian limits (-44 to -10).
#' @param longitude Numeric. Longitude in decimal degrees for the forecast location.
#'   Must be within Australian limits (112 to 154).
#' @param format Character. Either `"compact"` (default) or `"complete"` for the
#'   MET Weather API locationforecast endpoint variant.
#' @param api_key Character. Email address required for the User-Agent header
#'   in accordance with MET Weather API terms of service.
#' @param timeout Numeric. Request timeout in seconds (default: 30).
#' @param max_retries Integer. Maximum number of retry attempts on transient failures (default: 3).
#' @param retry_delay Numeric. Base delay between retries in seconds for exponential backoff (default: 1).
#'
#' @return A named list with elements:
#' \describe{
#'   \item{`data`}{Hourly forecast as a `data.table`.}
#'   \item{`raw`}{The full parsed GeoJSON response (list).}
#'   \item{`metadata`}{A list containing request parameters, status code, retrieval timestamp,
#'   and header information (`expires_raw`, `expires`, `last_modified_raw`, `last_modified`).}
#' }
#'
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au}
#'
#' @family METNO
#' @family data fetching
#'
#' @examples
#' \dontrun{
#' forecast <- get_metno_forecast(
#'   latitude = -31.95,
#'   longitude = 115.86,
#'   api_key = "your.email@@example.com"
#' )
#' forecast$metadata$expires
#' utils::head(forecast$data)
#' }
#'
#' @importFrom crul HttpClient
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate now
#' @keywords API
#' @autoglobal
#' @export
get_metno_forecast <- function(
    latitude,
    longitude,
    format = c("compact", "complete"),
    api_key = get_key(service = "METNO"),
    timeout = 30,
    max_retries = 3,
    retry_delay = 1
) {
  format <- match.arg(format)

  .check_not_example_api_key(api_key)
  .is_valid_email_metno_api_key(api_key)

  lonlat <- .check_lonlat(longitude, latitude)
  latitude <- as.numeric(lonlat["latitude"])
  longitude <- as.numeric(lonlat["longitude"])

  package_version <- tryCatch(
    as.character(utils::packageVersion("weatherOz")),
    error = function(e) "unknown"
  )
  user_agent <- sprintf(
    "weatherOz/%s (https://github.com/ropensci/weatherOz/) (%s)",
    package_version,
    api_key
  )

  base_url <- "https://api.met.no/weatherapi/locationforecast/2.0/"
  endpoint <- switch(
    format,
    "compact" = paste0(base_url, "compact"),
    "complete" = paste0(base_url, "complete")
  )

  params <- list(
    lat = round(latitude, 4),
    lon = round(longitude, 4)
  )

  client <- .metno_http_client(endpoint = endpoint,
                               user_agent = user_agent,
                               timeout = timeout)

  last_error <- NULL
  for (attempt in seq_len(max_retries)) {
    response <- tryCatch(
      client$get(query = params),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )

    if (!is.null(response)) {
      status_code <- response$status_code
      headers <- tolower(names(response$response_headers %||% list()))
      response_headers <- response$response_headers
      if (!is.null(response_headers)) {
        names(response_headers) <- headers
      } else {
        response_headers <- list()
      }

      if (status_code == 200) {
        raw_json <- jsonlite::fromJSON(response$parse("UTF-8"), simplifyVector = FALSE)
        timeseries <- raw_json$properties$timeseries %||% list()
        data_tbl <- metno_timeseries_to_data_table(timeseries)

        expires_raw <- response_headers[["expires"]]
        last_modified_raw <- response_headers[["last-modified"]]

        metadata <- list(
          request = list(
            latitude = latitude,
            longitude = longitude,
            format = format
          ),
          status_code = status_code,
          # set retrieval timestamp to AWST (Australia/Perth)
          retrieved_at = lubridate::now(tzone = "Australia/Perth"),
          expires_raw = expires_raw,
          expires = .parse_metno_http_date(expires_raw),
          last_modified_raw = last_modified_raw,
          last_modified = .parse_metno_http_date(last_modified_raw)
        )

        return(list(
          data = data_tbl,
          raw = raw_json,
          metadata = metadata
        ))
      }

      if (status_code == 203) {
        stop(
          "MET Weather API returned 203 (Deprecated Product).\nPlease review the API endpoint or parameters.",
          call. = FALSE
        )
      }

      if (status_code == 304) {
        stop("MET Weather API returned 304 Not Modified but no cached data is stored.", call. = FALSE)
      }

      if (status_code == 403) {
        stop(
          "MET Weather API returned 403 Forbidden. This usually means the User-Agent header is invalid or missing. ",
          "Current User-Agent: ", user_agent,
          call. = FALSE
        )
      }

      if (status_code == 429) {
        stop("MET Weather API rate limit exceeded. Please wait before making more requests.", call. = FALSE)
      }

      if (status_code >= 400) {
        stop(
          sprintf("MET Weather API HTTP Error %s: %s", status_code, response$parse("UTF-8")),
          call. = FALSE
        )
      }
    }

    if (attempt < max_retries) {
      delay <- retry_delay * (2 ^ (attempt - 1))
      warning(
        sprintf(
          "Transient error on attempt %d/%d%s. Retrying in %.2f seconds...",
          attempt,
          max_retries,
          if (!is.null(last_error)) paste0(": ", last_error$message) else "",
          delay
        ),
        call. = FALSE
      )
      Sys.sleep(delay)
    }
  }

  stop(
    sprintf(
      "MET Weather API request failed after %d attempts: %s",
      max_retries,
      if (!is.null(last_error)) last_error$message else "Unknown error"
    ),
    call. = FALSE
  )
}

#' Create a MET Norway HTTP Client
#' Internal helper for creating a `crul::HttpClient` configured for the
#' MET Norway API. Tests should mock this function rather than rebind into
#' the `crul` namespace.
#' @param endpoint Character string. The base URL endpoint for the API.
#' @param user_agent Character string. The User-Agent header to include in requests.
#' @param timeout Numeric. Timeout (in seconds) for the HTTP client. Defaults to 30.
#' @return A `crul::HttpClient` object configured with the given parameters.
#' @seealso [crul::HttpClient]
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au}
#' @family METNO
#' @family data fetching
#' @keywords internal
#' @noRd

.metno_http_client <- function(endpoint, user_agent, timeout = 30) {
  crul::HttpClient$new(
    url = endpoint,
    headers = list(`User-Agent` = user_agent),
    opts = list(timeout = timeout)
  )
}

#' Parse MET Norway HTTP Date
#' Internal parser for RFC 1123 HTTP date strings returned by the MET Norway API.
#' Converts strings to `POSIXct` in AWST (Australia/Perth) timezone.
#' @param value A character vector containing the date string (RFC 1123 format),
#'   or `NULL`.
#' @return A `POSIXct` object in the `"Australia/Perth"` timezone, or `NULL`
#'   if the input is invalid or missing.
#' @details
#' Attempts multiple parsing formats to handle common variations of HTTP
#' date formats (`GMT` or other timezone suffixes).
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au}
#' @family METNO
#' @family parse
#' @keywords internal
#' @noRd

.parse_metno_http_date <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NULL)
  }
  value <- value[[1]]
  if (is.na(value) || value == "") {
    return(NULL)
  }

  parsed <- as.POSIXct(value, format = "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT")
  if (is.na(parsed)) {
    parsed <- as.POSIXct(value, format = "%a, %d %b %Y %H:%M:%S %Z", tz = "GMT")
  }
  if (is.na(parsed)) {
    return(NULL)
  }
  # convert parsed GMT/UTC time to AWST (Australia/Perth)
  lubridate::with_tz(parsed, tzone = "Australia/Perth")
}

#' Format MET Norway HTTP Date
#' Internal helper that formats a `POSIXct` datetime into an RFC 1123-compliant
#' HTTP date string in UTC. Used when setting or comparing HTTP headers
#' for the MET Norway API.
#' @param value A `POSIXct` date-time object to format.
#' @return A character string formatted as an RFC 1123 HTTP date
#'   (e.g., `"Wed, 30 Oct 2024 10:45:00 GMT"`), or `NULL` if the input
#'   is `NULL`, not a `POSIXct`, or `NA`.
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au}
#' @family METNO
#' @family parse
#' @keywords internal
#' @noRd

.format_metno_http_date <- function(value) {
  if (is.null(value) || !inherits(value, "POSIXct") || is.na(value)) {
    return(NULL)
  }
  format(lubridate::with_tz(value, tzone = "UTC"), "%a, %d %b %Y %H:%M:%S GMT")
}

#' Convert MET Weather API timeseries to data.table
#'
#' @param timeseries List of timeseries data from MET Weather API
#' @return A data.table with hourly forecast data
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au}
#' @family METNO
#' @family parse
#' @keywords internal
#' @autoglobal
#' @export

metno_timeseries_to_data_table <- function(timeseries) {
  records <- data.table::data.table(
    time = as.POSIXct(character()),
    air_temperature = numeric(),
    relative_humidity = numeric(),
    wind_speed = numeric(),
    wind_from_direction = numeric(),
    cloud_area_fraction = numeric(),
    air_pressure_at_sea_level = numeric(),
    precipitation_amount = numeric(),
    symbol_code = character()
  )

  if (length(timeseries) == 0) {
    return(records)
  }

  for (entry in timeseries) {
    time_str <- entry$time
    if (is.null(time_str)) {
      next
    }

    instant_data <- entry$data$instant$details
    record <- list(
      # parse time as UTC then convert later to AWST
      time = lubridate::as_datetime(time_str, tz = "UTC"),
      air_temperature = NA_real_,
      relative_humidity = NA_real_,
      wind_speed = NA_real_,
      wind_from_direction = NA_real_,
      cloud_area_fraction = NA_real_,
      air_pressure_at_sea_level = NA_real_,
      precipitation_amount = NA_real_,
      symbol_code = NA_character_
    )

    for (name in names(instant_data)) {
      if (name %in% names(record)) {
        record[[name]] <- instant_data[[name]]
      }
    }

    period_names <- c("next_1_hours", "next_6_hours", "next_12_hours")
    for (period_name in period_names) {
      period_data <- entry$data[[period_name]]
      if (!is.null(period_data)) {
        details <- period_data$details
        if (!is.null(details$precipitation_amount)) {
          record$precipitation_amount <- details$precipitation_amount
        }
        summary <- period_data$summary
        if (!is.null(summary$symbol_code)) {
          record$symbol_code <- summary$symbol_code
        }
        break
      }
    }

    records <- data.table::rbindlist(list(records, record), fill = TRUE)
  }

  if ("time" %in% names(records)) {
    # ensure times are interpreted as UTC then converted to AWST
    records[, time := lubridate::force_tz(time, tzone = "UTC")]
    records[, time := lubridate::with_tz(time, tzone = "Australia/Perth")]
  }

  records[]
}

#' Get dominant weather symbol from a collection
#'
#' @param symbols Character vector of weather symbols
#' @return The dominant weather symbol
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au}
#' @family METNO
#' @family parse
#' @keywords internal
#' @autoglobal
#' @export

metno_get_dominant_symbol <- function(symbols) {
  if (length(symbols) == 0 || all(is.na(symbols))) {
    return(NA_character_)
  }

  symbols <- tolower(stats::na.omit(symbols))

  severity_keywords <- data.table::data.table(
    keyword = c("thunder", "lightning", "heavyrain", "rain", "sleet", "snow",
                "fog", "cloudy", "partlycloudy", "fair", "clearsky"),
    severity = c(100, 100, 90, 80, 75, 70, 60, 40, 30, 20, 10)
  )

  max_severity <- -1
  most_severe <- symbols[1]

  for (symbol in symbols) {
    current_severity <- 0
    for (i in seq_len(nrow(severity_keywords))) {
      if (grepl(severity_keywords$keyword[i], symbol)) {
        current_severity <- severity_keywords$severity[i]
        break
      }
    }
    if (current_severity > max_severity) {
      max_severity <- current_severity
      most_severe <- symbol
    }
  }
  most_severe
}

#' Resample data.table to different time frequencies
#'
#' @param dt_data data.table with time column
#' @param freq Character frequency: "hourly", "daily", "weekly", or "monthly"
#' @return A resampled data.table
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au}
#' @family METNO
#' @family parse
#' @keywords internal
#' @autoglobal
#' @export

metno_resample_data_table <- function(dt_data, freq) {
  if (!("time" %in% names(dt_data))) {
    stop("Input data.table must contain a 'time' column.", call. = FALSE)
  }

  # ensure times are in AWST (Australia/Perth)
  dt_data[, time := lubridate::force_tz(time, tzone = "UTC")]
  dt_data[, time := lubridate::with_tz(time, tzone = "Australia/Perth")]

  freq_map <- list(
    "hourly" = "hour",
    "daily" = "day",
    "weekly" = "week",
    "monthly" = "month",
    "D" = "day",
    "W" = "week",
    "M" = "month"
  )

  freq_lower <- tolower(freq)
  if (freq_lower %in% names(freq_map)) {
    freq_resolved <- freq_map[[freq_lower]]
  } else {
    stop("Unsupported frequency: ", freq, call. = FALSE)
  }

  dt_copy <- data.table::copy(dt_data)

  resampled <- switch(
    freq_resolved,
    "hour" = dt_copy[, .(
      date = time,
      air_temperature = air_temperature,
      relative_humidity = relative_humidity,
      wind_speed = wind_speed,
      wind_from_direction = wind_from_direction,
      cloud_area_fraction = cloud_area_fraction,
      air_pressure_at_sea_level = air_pressure_at_sea_level,
      precipitation_amount = precipitation_amount,
      symbol_code = symbol_code
    )],
    "day" = dt_copy[, .(
      min_temperature = min(air_temperature, na.rm = TRUE),
      max_temperature = max(air_temperature, na.rm = TRUE),
      total_precipitation = sum(precipitation_amount, na.rm = TRUE),
      avg_wind_speed = mean(wind_speed, na.rm = TRUE),
      max_wind_speed = max(wind_speed, na.rm = TRUE),
      avg_relative_humidity = mean(relative_humidity, na.rm = TRUE),
      avg_pressure = mean(air_pressure_at_sea_level, na.rm = TRUE),
      avg_cloud_fraction = mean(cloud_area_fraction, na.rm = TRUE),
      dominant_weather_symbol = metno_get_dominant_symbol(symbol_code)
    ), by = .(date = as.Date(time, tz = "Australia/Perth"))],
    "week" = {
  dt_copy[, week_start := as.Date(cut(time, "week"), tz = "Australia/Perth")]
      dt_copy[, .(
        min_temperature = min(air_temperature, na.rm = TRUE),
        max_temperature = max(air_temperature, na.rm = TRUE),
        total_precipitation = sum(precipitation_amount, na.rm = TRUE),
        avg_wind_speed = mean(wind_speed, na.rm = TRUE),
        max_wind_speed = max(wind_speed, na.rm = TRUE),
        avg_relative_humidity = mean(relative_humidity, na.rm = TRUE),
        avg_pressure = mean(air_pressure_at_sea_level, na.rm = TRUE),
        avg_cloud_fraction = mean(cloud_area_fraction, na.rm = TRUE),
        dominant_weather_symbol = metno_get_dominant_symbol(symbol_code)
      ), by = .(date = week_start)]
    },
    "month" = {
  dt_copy[, month_start := as.Date(cut(time, "month"), tz = "Australia/Perth")]
      dt_copy[, .(
        min_temperature = min(air_temperature, na.rm = TRUE),
        max_temperature = max(air_temperature, na.rm = TRUE),
        total_precipitation = sum(precipitation_amount, na.rm = TRUE),
        avg_wind_speed = mean(wind_speed, na.rm = TRUE),
        max_wind_speed = max(wind_speed, na.rm = TRUE),
        avg_relative_humidity = mean(relative_humidity, na.rm = TRUE),
        avg_pressure = mean(air_pressure_at_sea_level, na.rm = TRUE),
        avg_cloud_fraction = mean(cloud_area_fraction, na.rm = TRUE),
        dominant_weather_symbol = metno_get_dominant_symbol(symbol_code)
      ), by = .(date = month_start)]
    }
  )

  if (freq_resolved != "hour" && "date" %in% names(resampled)) {
    resampled[, date := as.Date(date)]
  }

  resampled[]
}
