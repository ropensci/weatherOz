test_that("user-input checks stop if invalid values are provided", {

  # missing station code -----
  expect_error(
    get_dpird_summaries(
      start_date = "20220501",
      end_date = "20220501",
      api_key = Sys.getenv("DPIRD_API_KEY"),
      interval = "daily",
      values = "wind",
      include_closed = FALSE
    )
  )

  # missing start_date -----
  expect_error(
    get_dpird_summaries(
      station_code = "BI",
      end_date = "20220501",
      api_key = Sys.getenv("DPIRD_API_KEY"),
      interval = "daily",
      values = "wind",
      include_closed = FALSE
    )
  )

  # reversed dates ----
  expect_error(
    get_dpird_summaries(
      station_code = "BI",
      start_date = "20220601",
      end_date = "20220501",
      api_key = Sys.getenv("DPIRD_API_KEY"),
      interval = "daily",
      values = "wind",
      include_closed = FALSE
    )
  )

  # missing api key -----
  expect_error(
    get_dpird_summaries(
      station_code = "BI",
      start_date = "20220501",
      end_date = "20220501",
      interval = "daily",
      values = "wind",
      include_closed = FALSE
    )
  )

  # invalid 'values' -----
  expect_error(
    get_dpird_summaries(
      station_code = "BI",
      start_date = "20220501",
      end_date = "20220501",
      api_key = Sys.getenv("DPIRD_API_KEY"),
      interval = "daily",
      values = "phytophthora",
      include_closed = FALSE
    )
  )

  # invalid 'interval' -----
  expect_error(
    get_dpird_summaries(
      station_code = "BI",
      start_date = "20220501",
      end_date = "20220501",
      api_key = Sys.getenv("DPIRD_API_KEY"),
      interval = "fortnightly",
      values = "wind",
      include_closed = FALSE
    )
  )

  # sub-hourly data are requested for data that is too old
  expect_error(
    get_dpird_summaries(
      station_code = "BI",
      start_date = "20180501",
      end_date = "20220501",
      api_key = Sys.getenv("DPIRD_API_KEY"),
      interval = "30min",
      values = "wind",
      include_closed = FALSE
    )
  )
})

## yearly ----
test_that("get_dpird_summaries() returns yearly values",
          {
            vcr::use_cassette("dpird_summaries_yearly", {
              skip_if_offline()
              x <- get_dpird_summaries(
                station_code = "BI",
                start_date = "20171028",
                end_date = "20221028",
                api_key = Sys.getenv("DPIRD_API_KEY"),
                interval = "yearly",
                values = "wind"
              )
            })
            expect_s3_class(x, "data.table")
            expect_length(x, 9)
            expect_named(
              x,
              c("station_code", "station_name", "year", "wind_avg_speed", "wind_height",
                "wind_max_direction_compass_point", "wind_max_direction_degrees",
                "wind_max_speed", "wind_max_time")
            )
            expect_type(x$station_code, "integer")
            expect_type(x$station_name, "character")
            expect_type(x$year, "integer")
            expect_s3_class(x$wind_max_time, "POSIXct")
          })

## monthly ----

test_that("get_dpird_summaries() returns monthly values",
          {
            vcr::use_cassette("dpird_summaries_monthly", {
              skip_if_offline()
              x <- get_dpird_summaries(
                station_code = "BI",
                start_date = "20171028",
                end_date = "20221028",
                api_key = Sys.getenv("DPIRD_API_KEY"),
                interval = "monthly",
                values = "wind"
              )
            })
            expect_s3_class(x, "data.table")
            expect_length(x, 11)
            expect_named(
              x,
              c("station_code", "station_name", "year", "month", "date", "wind_avg_speed",
                "wind_height", "wind_max_direction_compass_point", "wind_max_direction_degrees",
                "wind_max_speed", "wind_max_time")
            )
            expect_type(x$station_code, "integer")
            expect_type(x$station_name, "character")
            expect_type(x$year, "integer")
            expect_type(x$month, "integer")
            expect_s3_class(x$date, "Date")
            expect_s3_class(x$wind_max_time, "POSIXct")
          })

## daily ----

test_that("get_dpird_summaries() returns daily values",
          {
            vcr::use_cassette("dpird_summaries_daily", {
              skip_if_offline()
              x <- get_dpird_summaries(
                station_code = "BI",
                start_date = "20171028",
                end_date = "20181028",
                api_key = Sys.getenv("DPIRD_API_KEY"),
                interval = "daily",
                values = "wind"
              )
            })
            expect_s3_class(x, "data.table")
            expect_length(x, 12)
            expect_named(
              x,
              c("station_code", "station_name", "year", "month", "day", "date",
                "wind_avg_speed", "wind_height", "wind_max_direction_compass_point",
                "wind_max_direction_degrees", "wind_max_speed", "wind_max_time"
              )
            )
            expect_type(x$station_code, "integer")
            expect_type(x$station_name, "character")
            expect_type(x$year, "integer")
            expect_type(x$month, "integer")
            expect_type(x$day, "integer")
            expect_s3_class(x$date, "Date")
            expect_s3_class(x$wind_max_time, "POSIXct")
          })

## hourly ----

test_that("get_dpird_summaries() returns hourly values",
          {
            vcr::use_cassette("dpird_summaries_hourly", {
              skip_if_offline()
              x <- get_dpird_summaries(
                station_code = "BI",
                start_date = "20171028",
                end_date = "20171031",
                api_key = Sys.getenv("DPIRD_API_KEY"),
                interval = "hourly",
                values = "wind"
              )
            })
            expect_s3_class(x, "data.table")
            expect_length(x, 15)
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "year",
                "month",
                "day",
                "hour",
                "date",
                "wind_avg_direction_compass_point",
                "wind_avg_direction_degrees",
                "wind_avg_speed",
                "wind_height",
                "wind_max_direction_compass_point",
                "wind_max_direction_degrees",
                "wind_max_speed",
                "wind_max_time"
              )
            )
            expect_type(x$station_code, "integer")
            expect_type(x$station_name, "character")
            expect_type(x$year, "integer")
            expect_type(x$month, "integer")
            expect_type(x$day, "integer")
            expect_type(x$hour, "integer")
            expect_s3_class(x$date, "POSIXct")
            expect_s3_class(x$wind_max_time, "POSIXct")
          })

## 30min ----

test_that("get_dpird_summaries() returns 30min values",
          {
            vcr::use_cassette("dpird_summaries_30min", {
              skip_if_offline()
              x <- get_dpird_summaries(
                station_code = "BI",
                start_date = "20221028",
                end_date = "20221029",
                api_key = Sys.getenv("DPIRD_API_KEY"),
                interval = "30min",
                values = "wind"
              )
            })
            expect_s3_class(x, "data.table")
            expect_equal(ncol(x), 16)
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "year",
                "month",
                "day",
                "hour",
                "minute",
                "date",
                "wind_avg_direction_compass_point",
                "wind_avg_direction_degrees",
                "wind_avg_speed",
                "wind_height",
                "wind_max_direction_compass_point",
                "wind_max_direction_degrees",
                "wind_max_speed",
                "wind_max_time"
              )
            )
            expect_type(x$station_code, "integer")
            expect_type(x$station_name, "character")
            expect_type(x$year, "integer")
            expect_type(x$month, "integer")
            expect_type(x$day, "integer")
            expect_type(x$hour, "integer")
            expect_type(x$minute, "integer")
            expect_s3_class(x$date, "POSIXct")
            expect_s3_class(x$wind_max_time, "POSIXct")
          })

## 15min ----

test_that("get_dpird_summaries() returns 15min values",
          {
            vcr::use_cassette("dpird_summaries_15min", {
              skip_if_offline()
              x <- get_dpird_summaries(
                station_code = "BI",
                start_date = "20221028",
                end_date = "20221029",
                api_key = Sys.getenv("DPIRD_API_KEY"),
                interval = "15min",
                values = "wind"
              )
            })
            expect_s3_class(x, "data.table")
            expect_equal(ncol(x), 16)
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "year",
                "month",
                "day",
                "hour",
                "minute",
                "date",
                "wind_avg_direction_compass_point",
                "wind_avg_direction_degrees",
                "wind_avg_speed",
                "wind_height",
                "wind_max_direction_compass_point",
                "wind_max_direction_degrees",
                "wind_max_speed",
                "wind_max_time"
              )
            )
            expect_type(x$station_code, "integer")
            expect_type(x$station_name, "character")
            expect_type(x$year, "integer")
            expect_type(x$month, "integer")
            expect_type(x$day, "integer")
            expect_type(x$hour, "integer")
            expect_type(x$minute, "integer")
            expect_s3_class(x$date, "POSIXct")
            expect_s3_class(x$wind_max_time, "POSIXct")
          })
