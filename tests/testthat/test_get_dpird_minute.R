
test_that("get_dpird_minute() returns minute values", {
  vcr::use_cassette("dpird_minute_summaries", {
    skip_if_offline()
    x <- get_dpird_minute(
      station_code = "BI",
      start_date_time = "2018-02-01 13:00:00",
      minutes = 1440,
      api_key = Sys.getenv("DPIRD_API_KEY"),
      which_values = c("airTemperature",
                       "solarIrradiance",
                       "wind")
    )
  })
  expect_s3_class(x, "data.table")
  expect_equal(ncol(x), 10)
  expect_named(
    x,
    c(
      "station_code",
      "date_time",
      "air_temperature",
      "solar_irradiance",
      "wind_height",
      "wind_avg_speed",
      "wind_avg_direction_compass_point",
      "wind_avg_direction_degrees",
      "wind_min_speed",
      "wind_max_speed"
    )
  )
  expect_type(x$station_code, "character")
  expect_s3_class(x$date_time, "POSIXct")
  expect_type(x$air_temperature, "double")
  expect_type(x$solar_irradiance, "integer")
  expect_type(x$wind_height, "integer")
  expect_type(x$wind_avg_speed, "double")
  expect_type(x$wind_avg_direction_compass_point, "character")
  expect_type(x$wind_avg_direction_degrees, "double")
  expect_type(x$wind_min_speed, "double")
  expect_type(x$wind_max_speed, "double")
})
