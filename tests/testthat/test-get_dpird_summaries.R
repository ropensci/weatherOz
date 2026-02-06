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
            expect_identical(dim(x), c(2L, 19L))
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "longitude",
                "latitude",
                "year",
                "wind_avg_speed_3m",
                "wind_max_direction_compass_point_3m",
                "wind_max_direction_degrees_3m",
                "wind_max_speed_3m",
                "wind_max_time_3m",
                "wind_max_date_3m",
                "wind_max_time_of_day_3m",
                "wind_avg_speed_10m",
                "wind_max_direction_compass_point_10m",
                "wind_max_direction_degrees_10m",
                "wind_max_speed_10m",
                "wind_max_time_10m",
                "wind_max_date_10m",
                "wind_max_time_of_day_10m"
              )
            )
            expect_type(x$station_code, "integer")
            expect_type(x$station_name, "character")
            expect_type(x$year, "integer")
            expect_s3_class(x$wind_max_time_3m, "POSIXct")
            expect_s3_class(x$wind_max_time_10m, "POSIXct")
            date_only_idx <- is.na(x$wind_max_time_of_day_10m) &
              !is.na(x$wind_max_time_10m)
            expect_true(any(date_only_idx))
            expect_true(all(!is.na(x$wind_max_date_10m[date_only_idx])))
            expect_identical(attr(x$wind_max_time_10m, "tzone"), "Australia/West")
            expect_true(all(
              format(
                x$wind_max_time_10m[date_only_idx],
                "%H:%M:%S",
                tz = "Australia/West"
              ) == "08:00:00"
            ))
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
            expect_identical(dim(x), c(13L, 21L))
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
                "wind_avg_speed_3m",
                "wind_max_direction_compass_point_3m",
                "wind_max_direction_degrees_3m",
                "wind_max_speed_3m",
                "wind_max_time_3m",
                "wind_max_date_3m",
                "wind_max_time_of_day_3m",
                "wind_avg_speed_10m",
                "wind_max_direction_compass_point_10m",
                "wind_max_direction_degrees_10m",
                "wind_max_speed_10m",
                "wind_max_time_10m",
                "wind_max_date_10m",
                "wind_max_time_of_day_10m"
              )
            )
            expect_type(x$station_code, "integer")
            expect_type(x$station_name, "character")
            expect_type(x$year, "integer")
            expect_type(x$month, "integer")
            expect_s3_class(x$date, "Date")
            expect_s3_class(x$wind_max_time_3m, "POSIXct")
            expect_s3_class(x$wind_max_time_10m, "POSIXct")
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
            expect_identical(dim(x), c(2L, 22L))
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
                "wind_avg_speed_3m",
                "wind_max_direction_compass_point_3m",
                "wind_max_direction_degrees_3m",
                "wind_max_speed_3m",
                "wind_max_time_3m",
                "wind_max_date_3m",
                "wind_max_time_of_day_3m",
                "wind_avg_speed_10m",
                "wind_max_direction_compass_point_10m",
                "wind_max_direction_degrees_10m",
                "wind_max_speed_10m",
                "wind_max_time_10m",
                "wind_max_date_10m",
                "wind_max_time_of_day_10m"
              )
            )
            expect_type(x$station_code, "integer")
            expect_type(x$station_name, "character")
            expect_type(x$year, "integer")
            expect_type(x$month, "integer")
            expect_type(x$day, "integer")
            expect_s3_class(x$date, "Date")
            expect_s3_class(x$wind_max_time_3m, "POSIXct")
            expect_s3_class(x$wind_max_time_10m, "POSIXct")
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
            expect_identical(dim(x), c(25L, 27L))
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
                "wind_avg_direction_compass_point_3m",
                "wind_avg_direction_degrees_3m",
                "wind_avg_speed_3m",
                "wind_max_direction_compass_point_3m",
                "wind_max_direction_degrees_3m",
                "wind_max_speed_3m",
                "wind_max_time_3m",
                "wind_max_date_3m",
                "wind_max_time_of_day_3m",
                "wind_avg_direction_compass_point_10m",
                "wind_avg_direction_degrees_10m",
                "wind_avg_speed_10m",
                "wind_max_direction_compass_point_10m",
                "wind_max_direction_degrees_10m",
                "wind_max_speed_10m",
                "wind_max_time_10m",
                "wind_max_date_10m",
                "wind_max_time_of_day_10m"
              )
            )
            expect_type(x$station_code, "integer")
            expect_type(x$station_name, "character")
            expect_type(x$year, "integer")
            expect_type(x$month, "integer")
            expect_type(x$day, "integer")
            expect_type(x$hour, "integer")
            expect_s3_class(x$date, "POSIXct")
            expect_s3_class(x$wind_max_time_3m, "POSIXct")
            expect_s3_class(x$wind_max_time_10m, "POSIXct")
          })

test_that("padding missing wind heights preserves integer columns", {
  x <- data.table::data.table(
    station_code = factor("BI"),
    station_name = "Binnu",
    year = 2025L,
    wind_height = 3L,
    wind_max_direction_degrees = 180L,
    wind_max_speed = 12.3
  )

  out <- weatherOz:::.widen_wind_height_cols(x)

  expect_type(out$wind_max_direction_degrees_3m, "integer")
  expect_type(out$wind_max_direction_degrees_10m, "integer")
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
            expect_identical(dim(x), c(2L, 28L))
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
                "wind_avg_direction_compass_point_3m",
                "wind_avg_direction_degrees_3m",
                "wind_avg_speed_3m",
                "wind_max_direction_compass_point_3m",
                "wind_max_direction_degrees_3m",
                "wind_max_speed_3m",
                "wind_max_time_3m",
                "wind_max_date_3m",
                "wind_max_time_of_day_3m",
                "wind_avg_direction_compass_point_10m",
                "wind_avg_direction_degrees_10m",
                "wind_avg_speed_10m",
                "wind_max_direction_compass_point_10m",
                "wind_max_direction_degrees_10m",
                "wind_max_speed_10m",
                "wind_max_time_10m",
                "wind_max_date_10m",
                "wind_max_time_of_day_10m"
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
            expect_s3_class(x$wind_max_time_3m, "POSIXct")
            expect_s3_class(x$wind_max_time_10m, "POSIXct")
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
            expect_identical(dim(x), c(4L, 28L))
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
                "wind_avg_direction_compass_point_3m",
                "wind_avg_direction_degrees_3m",
                "wind_avg_speed_3m",
                "wind_max_direction_compass_point_3m",
                "wind_max_direction_degrees_3m",
                "wind_max_speed_3m",
                "wind_max_time_3m",
                "wind_max_date_3m",
                "wind_max_time_of_day_3m",
                "wind_avg_direction_compass_point_10m",
                "wind_avg_direction_degrees_10m",
                "wind_avg_speed_10m",
                "wind_max_direction_compass_point_10m",
                "wind_max_direction_degrees_10m",
                "wind_max_speed_10m",
                "wind_max_time_10m",
                "wind_max_date_10m",
                "wind_max_time_of_day_10m"
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
            expect_s3_class(x$wind_max_time_3m, "POSIXct")
            expect_s3_class(x$wind_max_time_10m, "POSIXct")
          })
