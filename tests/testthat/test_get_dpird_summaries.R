test_that("user-input checks stop if invalid values are provided", {
  # missing station code
  expect_error(
    get_dpird_summaries(
      start_date = "20220501",
      end_date = "20220501",
      api_key = Sys.getenv("DPIRD_API_KEY"),
      interval = "daily",
      which_values = "wind",
      api_group = "rtd",
      include_closed = FALSE
    )
  )

  # missing start_date
  expect_error(
    get_dpird_summaries(
      station_code = "BI",
      end_date = "20220501",
      api_key = Sys.getenv("DPIRD_API_KEY"),
      interval = "daily",
      which_values = "wind",
      api_group = "rtd",
      include_closed = FALSE
    )
  )

  # missing api key
  expect_error(
    get_dpird_summaries(
      station_code = "BI",
      start_date = "20220501",
      end_date = "20220501",
      interval = "daily",
      which_values = "wind",
      api_group = "rtd",
      include_closed = FALSE
    )
  )

  # invalid 'which_values'
  expect_error(
    get_dpird_summaries(
      station_code = "BI",
      start_date = "20220501",
      end_date = "20220501",
      api_key = Sys.getenv("DPIRD_API_KEY"),
      interval = "daily",
      which_values = "phytophthora",
      api_group = "rtd",
      include_closed = FALSE
    )
  )

  # invalid 'interval'
  expect_error(
    get_dpird_summaries(
      station_code = "BI",
      start_date = "20220501",
      end_date = "20220501",
      api_key = Sys.getenv("DPIRD_API_KEY"),
      interval = "fortnightly",
      which_values = "wind",
      api_group = "rtd",
      include_closed = FALSE
    )
  )

  # invalid 'api_group'
  expect_error(
    get_dpird_summaries(
      station_code = "BI",
      start_date = "20220501",
      end_date = "20220501",
      api_key = Sys.getenv("DPIRD_API_KEY"),
      interval = "daily",
      which_values = "wind",
      api_group = "swordfish trombones",
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
      which_values = "wind",
      api_group = "rtd",
      include_closed = FALSE
    )
  )
})

## yearly ----
test_that("get_dpird_summaries() returns yearly values",
          {
            vcr::use_cassette("dpird_yearly_summaries", {
              skip_if_offline()
              x <- get_dpird_summaries(
                station_code = "BI",
                start_date = "20171028",
                end_date = "20231028",
                api_key = Sys.getenv("DPIRD_API_KEY"),
                interval = "yearly",
                which_values = "wind"
              )
            })
            expect_s3_class(x, "data.table")
            expect_equal(ncol(x), 13)
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "period.year",
                "period.month",
                "period.day",
                "period.hour",
                "period.minute",
                "wind.avg.speed",
                "wind.height",
                "wind.max.direction.compass_point",
                "wind.max.direction.degrees",
                "wind.max.speed",
                "wind.max.time"
              )
            )
            expect_type(x$station_code, "character")
            expect_type(x$station_name, "character")
            expect_type(x$period.year, "integer")
            expect_type(x$period.month, "logical")
            expect_type(x$period.day, "logical")
            expect_type(x$period.hour, "logical")
            expect_type(x$period.minute, "logical")
            expect_s3_class(x$wind.max.time, "POSIXct")
          })

## monthly ----

test_that("get_dpird_summaries() returns monthly values",
          {
            vcr::use_cassette("dpird_monthly_summaries", {
              skip_if_offline()
              x <- get_dpird_summaries(
                station_code = "BI",
                start_date = "20171028",
                end_date = "20231028",
                api_key = Sys.getenv("DPIRD_API_KEY"),
                interval = "monthly",
                which_values = "wind"
              )
            })
            expect_s3_class(x, "data.table")
            expect_equal(ncol(x), 14)
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "period.year",
                "period.month",
                "period.day",
                "period.hour",
                "period.minute",
                "date",
                "wind.avg.speed",
                "wind.height",
                "wind.max.direction.compass_point",
                "wind.max.direction.degrees",
                "wind.max.speed",
                "wind.max.time"
              )
            )
            expect_type(x$station_code, "character")
            expect_type(x$station_name, "character")
            expect_type(x$period.year, "integer")
            expect_type(x$period.month, "integer")
            expect_type(x$period.day, "logical")
            expect_type(x$period.hour, "logical")
            expect_type(x$period.minute, "logical")
            expect_s3_class(x$date, "Date")
            expect_s3_class(x$wind.max.time, "POSIXct")
          })

## daily ----

test_that("get_dpird_summaries() returns daily values",
          {
            vcr::use_cassette("dpird_daily_summaries", {
              skip_if_offline()
              x <- get_dpird_summaries(
                station_code = "BI",
                start_date = "20171028",
                end_date = "20181028",
                api_key = Sys.getenv("DPIRD_API_KEY"),
                interval = "daily",
                which_values = "wind"
              )
            })
            expect_s3_class(x, "data.table")
            expect_equal(ncol(x), 14)
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "period.year",
                "period.month",
                "period.day",
                "period.hour",
                "period.minute",
                "date",
                "wind.avg.speed",
                "wind.height",
                "wind.max.direction.compass_point",
                "wind.max.direction.degrees",
                "wind.max.speed",
                "wind.max.time"
              )
            )
            expect_type(x$station_code, "character")
            expect_type(x$station_name, "character")
            expect_type(x$period.year, "integer")
            expect_type(x$period.month, "integer")
            expect_type(x$period.day, "integer")
            expect_type(x$period.hour, "logical")
            expect_type(x$period.minute, "logical")
            expect_s3_class(x$date, "Date")
            expect_s3_class(x$wind.max.time, "POSIXct")
          })

## hourly ----

test_that("get_dpird_summaries() returns hourly values",
          {
            vcr::use_cassette("dpird_hourly_summaries", {
              skip_if_offline()
              x <- get_dpird_summaries(
                station_code = "BI",
                start_date = "20171028",
                end_date = "20171031",
                api_key = Sys.getenv("DPIRD_API_KEY"),
                interval = "hourly",
                which_values = "wind"
              )
            })
            expect_s3_class(x, "data.table")
            expect_equal(ncol(x), 16)
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "period.year",
                "period.month",
                "period.day",
                "period.hour",
                "period.minute",
                "date",
                "wind.avg.direction.compass_point",
                "wind.avg.direction.degrees",
                "wind.avg.speed",
                "wind.height",
                "wind.max.direction.compass_point",
                "wind.max.direction.degrees",
                "wind.max.speed",
                "wind.max.time"
              )
            )
            expect_type(x$station_code, "character")
            expect_type(x$station_name, "character")
            expect_type(x$period.year, "integer")
            expect_type(x$period.month, "integer")
            expect_type(x$period.day, "integer")
            expect_type(x$period.hour, "integer")
            expect_type(x$period.minute, "logical")
            expect_s3_class(x$date, "POSIXct")
            expect_s3_class(x$wind.max.time, "POSIXct")
          })

## 30min ----

test_that("get_dpird_summaries() returns 30min values",
          {
            vcr::use_cassette("dpird_30min_summaries", {
              skip_if_offline()
              x <- get_dpird_summaries(
                station_code = "BI",
                start_date = "20221028",
                end_date = "20221029",
                api_key = Sys.getenv("DPIRD_API_KEY"),
                interval = "30min",
                which_values = "wind"
              )
            })
            expect_s3_class(x, "data.table")
            expect_equal(ncol(x), 16)
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "period.year",
                "period.month",
                "period.day",
                "period.hour",
                "period.minute",
                "date",
                "wind.avg.direction.compass_point",
                "wind.avg.direction.degrees",
                "wind.avg.speed",
                "wind.height",
                "wind.max.direction.compass_point",
                "wind.max.direction.degrees",
                "wind.max.speed",
                "wind.max.time"
              )
            )
            expect_type(x$station_code, "character")
            expect_type(x$station_name, "character")
            expect_type(x$period.year, "integer")
            expect_type(x$period.month, "integer")
            expect_type(x$period.day, "integer")
            expect_type(x$period.hour, "integer")
            expect_type(x$period.minute, "integer")
            expect_s3_class(x$date, "POSIXct")
            expect_s3_class(x$wind.max.time, "POSIXct")
          })

## 15min ----

test_that("get_dpird_summaries() returns 15min values",
          {
            vcr::use_cassette("dpird_15min_summaries", {
              skip_if_offline()
              x <- get_dpird_summaries(
                station_code = "BI",
                start_date = "20221028",
                end_date = "20221029",
                api_key = Sys.getenv("DPIRD_API_KEY"),
                interval = "15min",
                which_values = "wind"
              )
            })
            expect_s3_class(x, "data.table")
            expect_equal(ncol(x), 16)
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "period.year",
                "period.month",
                "period.day",
                "period.hour",
                "period.minute",
                "date",
                "wind.avg.direction.compass_point",
                "wind.avg.direction.degrees",
                "wind.avg.speed",
                "wind.height",
                "wind.max.direction.compass_point",
                "wind.max.direction.degrees",
                "wind.max.speed",
                "wind.max.time"
              )
            )
            expect_type(x$station_code, "character")
            expect_type(x$station_name, "character")
            expect_type(x$period.year, "integer")
            expect_type(x$period.month, "integer")
            expect_type(x$period.day, "integer")
            expect_type(x$period.hour, "integer")
            expect_type(x$period.minute, "integer")
            expect_s3_class(x$date, "POSIXct")
            expect_s3_class(x$wind.max.time, "POSIXct")
          })
