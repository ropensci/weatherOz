#' Get Daily Weather Forecast Data from MET Weather API
#'
#' Retrieves daily aggregated weather forecast data from the Norwegian Meteorological Institute
#' (MET.NO) locationforecast API. This is a convenience function that wraps `get_metno_forecast`
#' and aggregates the hourly forecast data into daily summaries.
#'
#' @param latitude Numeric. The latitude in decimal degrees for the forecast location.
#'   Must be within Australian limits (-44 to -10).
#' @param longitude Numeric. The longitude in decimal degrees for the forecast location.
#'   Must be within Australian limits (112 to 154).
#' @param days Numeric. The number of forecast days to return (1-9, default: 9).
#' @param api_key Character. Your email address, required for the User-Agent header
#'   as per MET Weather API terms of service.
#' @param timeout Numeric. Request timeout in seconds (default: 30).
#' @param max_retries Numeric. Maximum number of retry attempts for failed requests (default: 3).
#' @param retry_delay Numeric. Base delay between retries in seconds (default: 1.0).
#'
#' @return A `data.table` with daily aggregated weather forecasts, including:
#'   `date`, `min_temperature`, `max_temperature`, `total_precipitation`,
#'   `avg_wind_speed`, `max_wind_speed`, `avg_relative_humidity`,
#'   `avg_pressure`, `avg_cloud_fraction`, and `dominant_weather_symbol`.
#'
#' @details
#' The `latitude` and `longitude` are truncated to 4 decimal places as per
#' MET Weather API Terms of Service. The daily aggregation includes minimum and
#' maximum temperatures, total precipitation, average and maximum wind speeds,
#' average relative humidity, average air pressure, average cloud fraction, and
#' a dominant weather symbol for the day.
#'
#'
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au}
#' @family METNO
#' @family data fetching
#'
#' @examples
#' \dontrun{
#' # Example of how to use the function (replace with your actual email)
#' # daily_forecast <- get_metno_daily_forecast(
#' #   latitude = -27.5,
#' #   longitude = 153.0,
#' #   days = 7,
#' #   api_key = "your.email@@example.com"
#' # )
#' # print(daily_forecast)
#' }
#'
#' @keywords API
#' @autoglobal
#' @export
get_metno_daily_forecast <- function(
    latitude,
    longitude,
    days = 9,
    api_key,
    timeout = 30,
    max_retries = 3,
    retry_delay = 1.0
) {
  # Validate days
  if (days < 1 || days > 9) {
    stop("'days' must be between 1 and 9.", call. = FALSE)
  }

  # Get raw forecast data
  raw_forecast <- get_metno_forecast(
    latitude = latitude,
    longitude = longitude,
    format = "compact",
    api_key = api_key,
    timeout = timeout,
    max_retries = max_retries,
    retry_delay = retry_delay
  )

  dt_hourly <- raw_forecast$data

  # Aggregate to daily
  dt_daily <- metno_resample_data_table(dt_hourly, freq = "daily")

  # Return only requested number of days
  return(utils::head(dt_daily, days))
}
