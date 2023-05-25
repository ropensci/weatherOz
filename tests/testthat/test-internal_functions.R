
## Check Haversine distance ----

# bankstown to sydney airports approximately 17628m
test_that("Bankstown airport to Sydney airport approximately 17628m", {
  expect_lt(
    .haversine_distance(
      -33 - 56 / 60 - 46 / 3600,
      151 + 10 / 60 + 38 / 3600,
      -33 - 55 / 60 - 28 / 3600,
      150 + 59 / 60 + 18 / 3600
    ) / 17.628 - 1,
    0.01
  )
})

test_that("Broken Hill airport to Sydney airport approximately 932158", {
  expect_lt(
    .haversine_distance(
      -33 - 56 / 60 - 46 / 3600,
      151 + 10 / 60 + 38 / 3600,
      -32 - 00 / 60 - 05 / 3600,
      141 + 28 / 60 + 18 / 3600
    ) / 932.158 - 1,
    0.01
  )
})


## Check date ----

test_that("a `date` entered in incorrect format is corrected", {
  dates <- "Jan-01-1983"
  dates <- .check_date(dates)
  expect_s3_class(dates, "POSIXct")
})

test_that("a `date` entered in incorrect format and corrected", {
  dates <- "Jan-01-n"
  expect_error(.check_date(dates),
               regexp = "*Please enter a valid date format.")
})

## Check date order ----

test_that(".check_date_order() catches errors, passes otherwise", {
  end <- as.Date("2022-01-02")
  start <- as.Date("2021-12-31")

  expect_no_error(.check_date_order(.start_date = start, .end_date = end))
  expect_error(.check_date_order(.start_date = end, .end_date = start))
})

## Check lat/lon params

test_that("if lat lon or station_code are correct, no error", {
  expect_no_error(.check_location_params(
    .latitude = -38.5,
    .longitude = 114.5,
    .station_code = NULL
  ))
  expect_no_error(.check_location_params(
    .latitude = NULL,
    .longitude = NULL,
    .station_code = "NO"
  ))
})

test_that("if no lat, lon or station code are provided, error", {
  expect_error(.check_location_params(
    .latitude = NULL,
    .longitude = NULL,
    .station_code = NULL
  ))
})

test_that("if lon outside Oz bounding box are provided, error", {
  expect_error(.check_lonlat(latitude = -38.5, longitude = 153))
  expect_error(.check_lonlat(latitude = 0, longitude = 151))
})

## Check user-input for selecting an API

test_that("check user-input for `which_api`", {
  expect_error(.check_which_api(which_api = "none"))
  for (i in c("all", "silo", "dpird")) {
    expect_equal(i, .check_which_api(which_api = i))
  }
})

## .check_states ----

test_that(".check_states will return reasonable values", {
  state_code <- "ans"
  expect_message(.check_states(state_code))
})
