test_that("user-input checks stop if invalid values are provided", {
  # missing station code -----
  expect_error(
    get_dpird_summaries(
      start_date = "20220501",
      end_date = "20220501",
      interval = "daily",
      values = "wind"
    )
  )

  # missing start_date -----
  expect_error(
    get_dpird_summaries(
      station_code = "BI",
      end_date = "20220501",
      interval = "daily",
      values = "wind"
    )
  )

  # start_date too early -----
  expect_error(
    get_dpird_summaries(
      station_code = "BI",
      start_date = "19800123",
      interval = "daily",
      values = "wind"
    )
  )

  # reversed dates ----
  expect_error(
    get_dpird_summaries(
      station_code = "BI",
      start_date = "20220601",
      end_date = "20220501",
      interval = "daily",
      values = "wind"
    )
  )

  # no api_key -----
  expect_error(
    get_dpird_summaries(
      station_code = "BI",
      start_date = "20220501",
      end_date = "20220501",
      interval = "daily",
      api_key = "",
      values = "wind"
    )
  )

  # n api_key, e.g., SILO API key (email) -----
  expect_error(
    get_dpird_summaries(
      station_code = "BI",
      start_date = "20220501",
      end_date = "20220501",
      interval = "daily",
      values = "wind",
      api_key = ""
    )
  )

  # invalid 'values' -----
  expect_error(
    get_dpird_summaries(
      station_code = "BI",
      start_date = "20220501",
      end_date = "20220501",
      interval = "daily",
      values = "phytophthora"
    )
  )

  # invalid 'interval' -----
  expect_error(
    get_dpird_summaries(
      station_code = "BI",
      start_date = "20220501",
      end_date = "20220501",
      interval = "fortnightly",
      values = "wind"
    )
  )

  # sub-hourly data are requested for data that is too old
  expect_error(
    get_dpird_summaries(
      station_code = "BI",
      start_date = "20180501",
      end_date = "20220501",
      interval = "30min",
      values = "wind"
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
                end_date = "20181028",
                interval = "yearly",
                values = "wind"
              )
            })
            expect_s3_class(x, "data.table")
            expect_identical(dim(x), c(4L, 11L))
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "longitude",
                "latitude",
                "year",
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
                end_date = "20181028",
                interval = "monthly",
                values = "wind"
              )
            })
            expect_s3_class(x, "data.table")
            expect_identical(dim(x), c(26L, 13L))
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "longitude",
                "latitude",
                "year",
                "month",
                "date",
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
                end_date = "20171029",
                api_key = Sys.getenv("DPIRD_API_KEY"),
                interval = "daily",
                values = "wind"
              )
            })
            expect_s3_class(x, "data.table")
            expect_identical(dim(x), c(4L, 14L))
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "longitude",
                "latitude",
                "year",
                "month",
                "day",
                "date",
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
                end_date = "20171029",
                api_key = Sys.getenv("DPIRD_API_KEY"),
                interval = "hourly",
                values = "wind"
              )
            })
            expect_s3_class(x, "data.table")
            expect_identical(dim(x), c(50L, 17L))
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "longitude",
                "latitude",
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
                start_date = paste0(format(Sys.Date() - 365, "%Y"), "1028"),
                end_date = paste0(format(Sys.Date() - 365, "%Y"), "1028"),
                api_key = Sys.getenv("DPIRD_API_KEY"),
                interval = "30min",
                values = "wind"
              )
            })
            expect_s3_class(x, "data.table")
            expect_identical(dim(x), c(4L, 18L))
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "longitude",
                "latitude",
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
# This test checks that the for loop in get_dpird_summaries() properly parses
# paginated results by checking the number of rows returned to ensure that the
# data are not truncated
test_that("get_dpird_summaries() returns 15min values",
          {
            vcr::use_cassette("dpird_summaries_15min", {
              skip_if_offline()
              x <- get_dpird_summaries(
                station_code = "BI",
                start_date = paste0(format(Sys.Date()-365, "%Y"), "1028"),
                end_date = paste0(format(Sys.Date()-365, "%Y"), "1028"),
                api_key = Sys.getenv("DPIRD_API_KEY"),
                interval = "15min",
                values = "wind"
              )
            })
            expect_s3_class(x, "data.table")
            expect_identical(dim(x), c(8L, 18L))
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "longitude",
                "latitude",
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
