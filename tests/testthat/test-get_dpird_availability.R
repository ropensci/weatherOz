## check user-inputs

### no api_key

test_that("get_dpird_availability() fails with bad user inputs", {
  expect_error(
    get_dpird_availability(
      station_code = "BD002",
      start_date = "20230401",
      end_date = "20230430",
      api_key = ""
    )
  )
  expect_error(
    get_dpird_availability(
      station_code = "BD002",
      end_date = "20230430"
    )
  )
  expect_error(
    get_dpird_availability(
      station_code = "BD002",
      start_date = "20230430"
    )
  )

  expect_error(
    get_dpird_availability(
      station_code = "NO",
      start_date = "20230430",
      api_key = "your_api_key"
    ),
    regexp = "You have copied the example code *"
  )
})

## custom period availability ----
test_that("get_dpird_availability() returns values", {
  vcr::use_cassette("dpird_period_availability", {
    skip_if_offline()
    x <- get_dpird_availability(
      station_code = c("BD002", "SP"),
      start_date = "20230401",
      end_date = "20230402"
    )
  })
  expect_s3_class(x, "data.table")
  expect_named(
    x,
    c(
      "station_code",
      "station_name",
      "start_date",
      "end_date",
      "availability_since_9_am",
      "availability_since_12_am"
    )
  )
  expect_type(x$station_code, "integer")
  expect_type(x$station_name, "character")
  expect_s3_class(x$start_date, "POSIXct")
  expect_s3_class(x$end_date, "POSIXct")
})

## all station availability ----
test_that("get_dpird_availability() returns values", {
  vcr::use_cassette("dpird_all_availability", {
    skip_if_offline()
    x <-
      get_dpird_availability()
  })
  expect_s3_class(x, "data.table")
  expect_named(
    x,
    c(
      "station_code",
      "station_name",
      "to9_am",
      "since9_am",
      "since12_am",
      "current_hour",
      "last24_hours",
      "last7_days_since9_am",
      "last7_days_since12_am",
      "last14_days_since9_am",
      "last14_days_since12_am",
      "month_to_date_to9_am",
      "month_to_date_since12_am",
      "year_to_date_to9_am",
      "year_to_date_since12_am"
    )
  )
  expect_type(x$station_code, "integer")
  expect_type(x$station_name, "character")
})
