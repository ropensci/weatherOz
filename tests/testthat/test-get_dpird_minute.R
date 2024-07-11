
test_that("user-input checks stop if invalid values are provided", {
  # missing station code
  expect_error(
    get_dpird_minute(
      start_date_time = "2018-02-01 13:00:00",
      minutes = 1440,
      values = "wind"
    )
  )

  # malformed date/time
  expect_error(
    get_dpird_minute(
      station_code = "BI",
      start_date_time = "2018-02-31 13:00:00",
      minutes = 1440,
      values = "wind"
    )
  )

  # no api_key
  expect_error(
    get_dpird_minute(
      station_code = "BI",
      start_date_time = "2018-02-01 13:00:00",
      minutes = 1440,
      values = "wind",
      api_key = ""
    )
  )

  # invalid 'values'
  expect_error(
    get_dpird_minute(
      station_code = "BI",
      start_date_time = "2018-02-01 13:00:00",
      minutes = 1440,
      values = "phytophthora"
    )
  )

  # 'minutes' too long
  expect_error(
    get_dpird_minute(
      station_code = "BI",
      start_date_time = "2018-02-01 13:00:00",
      minutes = 1450,
      values = "wind"
    )
  )
})

test_that("get_dpird_minute() returns minute values", {
  withr::local_timezone(tz = "Australia/Perth")
  vcr::use_cassette("dpird_minute_values", {
    skip_if_offline()
    x <- get_dpird_minute(
      station_code = "BI",
      start_date_time = "2018-02-01 13:00:00",
      minutes = 30,
      values = "wind"
    )
  }
  )
  expect_s3_class(x, "data.table")
  expect_equal(ncol(x), 8)
  expect_named(
    x,
    c(
      "station_code",
      "date_time",
      "wind_height",
      "wind_avg_speed",
      "wind_avg_direction_compass_point",
      "wind_avg_direction_degrees",
      "wind_min_speed",
      "wind_max_speed"
    )
  )
  expect_type(x$station_code, "integer")
  expect_s3_class(x$date_time, "POSIXct")
  expect_type(x$wind_height, "integer")
  expect_type(x$wind_avg_speed, "double")
  expect_type(x$wind_avg_direction_compass_point, "character")
  expect_type(x$wind_avg_direction_degrees, "double")
  expect_type(x$wind_min_speed, "double")
  expect_type(x$wind_max_speed, "double")
})
