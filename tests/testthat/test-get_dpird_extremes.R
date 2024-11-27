
test_that("user-input checks stop if invalid values are provided", {
  # missing station code
  expect_error(get_dpird_extremes(values = "wind"))

  # invalid 'values'
  expect_error(
    get_dpird_extremes(
      station_code = "BI",
      values = "phytophthora"
    )
  )

  # no api_key
  expect_error(
    get_dpird_extremes(
      station_code = "BI",
      values = "all",
      api_key = ""
    )
  )

  # too many stations
  expect_error(
    get_dpird_extremes(
      station_code = c("BI", "NO"),
      values = "all"
    )
  )
})

test_that("get_dpird_extremes() returns all values for a station", {
  vcr::use_cassette("dpird_extreme_all_values", {
    skip_if_offline()
    x <- get_dpird_extremes(
      station_code = "BI",
      values = "all"
    )
  })
  expect_s3_class(x, "data.table")
  expect_length(x, 41L)
  expect_identical(nrow(x), 1L)
  expect_named(
    x,
    c(
      "station_code",
      "longitude",
      "latitude",
      "frost_condition_since9_am_minutes",
      "frost_condition_since9_am_start_time",
      "frost_condition_to9_am_minutes",
      "frost_condition_to9_am_start_time",
      "frost_condition_last7_days_minutes",
      "frost_condition_last7_days_days",
      "frost_condition_last14_days_minutes",
      "frost_condition_last14_days_days",
      "frost_condition_month_to_date_minutes",
      "frost_condition_month_to_date_start_time",
      "frost_condition_month_to_date_days",
      "frost_condition_year_to_date_minutes",
      "frost_condition_year_to_date_start_time",
      "frost_condition_year_to_date_days",
      "heat_condition_since12_am_minutes",
      "heat_condition_since12_am_start_time",
      "heat_condition_last7_days_minutes",
      "heat_condition_last7_days_days",
      "heat_condition_last14_days_minutes",
      "heat_condition_last14_days_days",
      "heat_condition_month_to_date_minutes",
      "heat_condition_month_to_date_start_time",
      "heat_condition_month_to_date_days",
      "heat_condition_year_to_date_minutes",
      "heat_condition_year_to_date_start_time",
      "heat_condition_year_to_date_days",
      "erosion_condition_since12_am_minutes",
      "erosion_condition_since12_am_start_time",
      "erosion_condition_last7_days_minutes",
      "erosion_condition_last7_days_days",
      "erosion_condition_last14_days_minutes",
      "erosion_condition_last14_days_days",
      "erosion_condition_month_to_date_minutes",
      "erosion_condition_month_to_date_start_time",
      "erosion_condition_month_to_date_days",
      "erosion_condition_year_to_date_minutes",
      "erosion_condition_year_to_date_start_time",
      "erosion_condition_year_to_date_days"
    )
  )
})

test_that("get_dpird_extremes() returns selected values for a station", {
  vcr::use_cassette("dpird_extreme_erosion_conditions", {
    skip_if_offline()
    x <- get_dpird_extremes(
      station_code = "BI",
      values = "erosionCondition"
    )
  })
  expect_s3_class(x, "data.table")
  expect_length(x, 15L)
  expect_identical(nrow(x), 1L)
  expect_named(
    x,
    c(
      "station_code",
      "longitude",
      "latitude",
      "erosion_condition_since12_am_minutes",
      "erosion_condition_since12_am_start_time",
      "erosion_condition_last7_days_minutes",
      "erosion_condition_last7_days_days",
      "erosion_condition_last14_days_minutes",
      "erosion_condition_last14_days_days",
      "erosion_condition_month_to_date_minutes",
      "erosion_condition_month_to_date_start_time",
      "erosion_condition_month_to_date_days",
      "erosion_condition_year_to_date_minutes",
      "erosion_condition_year_to_date_start_time",
      "erosion_condition_year_to_date_days"
    )
  )
})
