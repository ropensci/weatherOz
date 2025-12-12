
dummy_email <- "test@example.com"

load_metno_fixture <- function() {
  # locate canonical fixture under tests/fixtures
  fp <- testthat::test_path("../fixtures/metno_forecast_compact.json")
  cassette <- jsonlite::fromJSON(fp, simplifyVector = FALSE)
  interaction <- cassette$http_interactions[[1]]
  interaction$response$headers <- lapply(interaction$response$headers, identity)
  interaction
}

mock_metno_call <- function(latitude = -27.5, longitude = 153.0, format = "compact") {
  interaction <- load_metno_fixture()
  body_string <- interaction$response$body$string
  headers <- interaction$response$headers
  status_code <- interaction$response$status
  if (is.null(status_code)) status_code <- 200

  temp_response <- list(
    status_code = status_code,
    parse = function(encoding = "UTF-8") body_string,
    response_headers = headers
  )

  temp_client <- list(
    get = function(query) temp_response
  )

  # create a fake factory that returns our fake client
  temp_factory <- function(endpoint, user_agent, timeout = 30) {
    temp_client
  }

  # mock the package-internal factory so we don't bind into crul
  testthat::with_mocked_bindings(
    get_metno_forecast(latitude = latitude, longitude = longitude, format = format, api_key = dummy_email),
    `.metno_http_client` = temp_factory,
    .package = "weatherOz"
  )
}

test_that("get_metno_forecast returns structured forecast with metadata (AWST)", {
  forecast <- mock_metno_call()

  expect_type(forecast, "list")
  expect_s3_class(forecast$data, "data.table")
  expect_true("time" %in% names(forecast$data))
  expect_gt(nrow(forecast$data), 0)
  # times should be in Australia/Perth
  tz_attr <- attr(forecast$data$time, "tzone")
  expect_true(!is.null(tz_attr) && tz_attr == "Australia/Perth")
  expect_type(forecast$metadata, "list")
  expect_true(!is.null(forecast$metadata$retrieved_at))
})

test_that("get_metno_forecast validates inputs", {
  # latitude out of bounds
  expect_error(get_metno_forecast(latitude = -100, longitude = 153, api_key = dummy_email))
  # longitude out of bounds
  expect_error(get_metno_forecast(latitude = -27.5, longitude = 300, api_key = dummy_email))
  # invalid email
  expect_error(get_metno_forecast(latitude = -27.5, longitude = 153, api_key = "invalid"))
})

test_that("metno_timeseries_to_data_table converts timeseries to data.table and AWST", {
  interaction <- load_metno_fixture()
  body_string <- interaction$response$body$string
  parsed <- fromJSON(body_string, simplifyVector = FALSE)
  ts <- parsed$properties$timeseries
  dt_hourly <- metno_timeseries_to_data_table(ts)
  expect_s3_class(dt_hourly, "data.table")
  expect_true(nrow(dt_hourly) > 0)
  expect_true(all(attr(dt_hourly$time, "tzone") == "Australia/Perth"))
})

test_that("metno_timeseries_to_data_table handles empty input", {
  empty_dt <- metno_timeseries_to_data_table(list())
  expect_equal(nrow(empty_dt), 0)
})

test_that("metno_get_dominant_symbol identifies dominant code", {
  expect_equal(weatherOz:::metno_get_dominant_symbol(c()), NA_character_)
})

test_that("metno_resample_data_table aggregates to daily correctly", {
  forecast <- mock_metno_call()
  dt_hourly <- forecast$data
  dt_daily <- metno_resample_data_table(dt_hourly, "daily")
  expect_s3_class(dt_daily, "data.table")
  expect_true("date" %in% names(dt_daily))
  expect_true(inherits(dt_daily$date, "Date"))
})

test_that(".parse and .format metno http dates round trip", {
  # load headers from fixture
  interaction <- load_metno_fixture()
  headers <- interaction$response$headers
  # headers from VCR fixtures are lower-case; use 'expires' key
  expires_raw <- headers$expires[[1]]
  parsed <- weatherOz:::.parse_metno_http_date(expires_raw)
  expect_true(inherits(parsed, "POSIXct"))
  formatted <- weatherOz:::.format_metno_http_date(parsed)
  expect_true(is.character(formatted))
  expect_null(weatherOz:::.format_metno_http_date(NULL))
})

test_that("metno_timeseries_to_data_table converts timeseries to data.table", {
  forecast <- mock_metno_call()
  timeseries <- forecast$raw$properties$timeseries
  dt_hourly <- weatherOz:::metno_timeseries_to_data_table(timeseries)

  expect_s3_class(dt_hourly, "data.table")
  expect_true("time" %in% names(dt_hourly))
  expect_true(inherits(dt_hourly$time, "POSIXct"))
  expect_true("air_temperature" %in% names(dt_hourly))
  expect_true("precipitation_amount" %in% names(dt_hourly))
  expect_true("symbol_code" %in% names(dt_hourly))
  expect_true(all(lubridate::tz(dt_hourly$time) == "Australia/Perth")) # timezone aware (AWST)
})

test_that("metno_timeseries_to_data_table handles empty input", {
  empty_dt <- weatherOz:::metno_timeseries_to_data_table(list())
  expect_s3_class(empty_dt, "data.table")
  expect_equal(nrow(empty_dt), 0)
})

test_that("metno_get_dominant_symbol identifies dominant code", {
  expect_equal(weatherOz:::metno_get_dominant_symbol(c("clearsky_day", "fair_day", "partlycloudy_day")), "partlycloudy_day")
  expect_equal(weatherOz:::metno_get_dominant_symbol(c("rain", "clearsky_day", "thunder")), "thunder")
  expect_equal(weatherOz:::metno_get_dominant_symbol(c()), NA_character_)
})

test_that("metno_resample_data_table aggregates correctly", {
  forecast <- mock_metno_call()
  dt_hourly <- forecast$data
  dt_daily <- weatherOz:::metno_resample_data_table(dt_hourly, "daily")

  expect_s3_class(dt_daily, "data.table")
  expect_true("date" %in% names(dt_daily))
  expect_true("min_temperature" %in% names(dt_daily))
  expect_lt(nrow(dt_daily), nrow(dt_hourly))

  dt_hourly_again <- weatherOz:::metno_resample_data_table(dt_hourly, "hourly")
  expect_s3_class(dt_hourly_again, "data.table")
  expect_equal(dt_hourly_again$date, dt_hourly$time)
})

test_that("get_metno_daily_forecast aggregates daily data", {
  mock_result <- mock_metno_call()
  with_mocked_bindings(
    {
      daily <- get_metno_daily_forecast(
        latitude = -27.5,
        longitude = 153.0,
        days = 5,
        api_key = dummy_email
      )
      expect_s3_class(daily, "data.table")
      expect_equal(nrow(daily), 5)
      expect_true("date" %in% names(daily))
    },
    get_metno_forecast = function(...) mock_result,
    .package = "weatherOz"
  )
})

test_that(".parse and .format metno http dates round trip", {
  timestamp <- "Wed, 29 Oct 2025 01:43:37 GMT"
  parsed <- weatherOz:::.parse_metno_http_date(timestamp)
  expect_true(inherits(parsed, "POSIXct"))
  expect_equal(weatherOz:::.format_metno_http_date(parsed), timestamp)
  expect_null(weatherOz:::.parse_metno_http_date(NULL))
  expect_null(weatherOz:::.format_metno_http_date(NULL))
})
