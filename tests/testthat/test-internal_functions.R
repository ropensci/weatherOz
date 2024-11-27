
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


## Check date order ----

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

test_that(".check_date_order() catches errors, passes otherwise", {
  start <- as.Date("2021-12-31")
  end <- as.Date("2022-01-02")

  expect_invisible(.check_date_order(.start_date = start, .end_date = end))
  expect_error(.check_date_order(.start_date = end, .end_date = start))
  expect_error(.check_date_order(.start_date = as.Date("2050-01-01"),
                                 .end_date = as.Date("2050-01-31")))
})

## Check earliest available SILO data ----

test_that("if `start_date` preceeds earliest available BOM data, fn() stops", {
  expect_error(.check_earliest_available_silo("1800-01-01"))
})

## Check lat/lon params ----

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
  expect_error(.check_lonlat(latitude = -38.5, longitude = 155))
  expect_error(.check_lonlat(latitude = 0, longitude = 151))
})

test_that(".check_lonlat() returns invisible `NULL` if no errors encountered", {
  expect_invisible(.check_lonlat(latitude = -38.5,
                                 longitude = 114.5))
})

## Check user-input for selecting an API ----

test_that("check user-input for `which_api`", {
  expect_error(.check_which_api(which_api = "none"))
  for (i in c("all", "silo", "dpird")) {
    expect_identical(i, .check_which_api(which_api = i))
  }
})

## Test that the user didn't provide a `NULL` value for an API Key

test_that("check user-input for a SILO API key isn't `NULL`", {
  expect_error(.is_valid_email_silo_api_key(.api_key = NULL))
})

test_that("check user-input for a DPIRD API key isn't `NULL`", {
  expect_error(.is_valid_dpird_api_key(.api_key = NULL))
})

## .check_lon_lat() ----
test_that(".check_lonlat() returns invisible `NULL` if no errors", {
  expect_invisible(.check_lonlat(longitude = 150.05,
                                 latitude = -27.85))
})

## .check_states() ----

test_that(".check_states() will return reasonable values", {
  expect_error(.check_states(state = "ans"))
  expect_message(.check_states(state = "Quld"))
})

## .convert_state() ----

test_that(".convert_state() will return the proper match", {
  expect_identical(.convert_state(state = "WA"), "WA")
  expect_identical(.convert_state(state = "Western Australia"), "WA")
  expect_error(.convert_state(state = "Kansas"))
})

## .set_snake_case_names() ----
test_that(".snake_case_names() converts CamelCase colnames to snake_case", {
  x <- data.table("UpperCase" = 1)
  expect_named(.set_snake_case_names(x), "upper_case")
  expect_error(.set_snake_case_names(c("UpperCase" = 1)))
})
