

test_that("user-input checks stop if invalid values are provided", {
  # missing station code
  expect_error(get_dpird_extremes(api_key = Sys.getenv("DPIRD_API_KEY"),
                                  values = "wind"))

  # invalid 'values'
  expect_error(
    get_dpird_extremes(
      station_code = "BI",
      api_key = Sys.getenv("DPIRD_API_KEY"),
      values = "phytophthora"
    )
  )

  # invalid group
  expect_error(
    get_dpird_extremes(
      station_code = "BI",
      api_key = Sys.getenv("DPIRD_API_KEY"),
      values = "all",
      api_group = "Tame Impala"
    )
  )
})

test_that("get_dpird_extremes() returns all values for a station", {
  vcr::use_cassette("dpird_extreme_all_values", {
    skip_if_offline()
    x <- get_dpird_extremes(
      station_code = "BI",
      api_key = Sys.getenv("DPIRD_API_KEY"),
      values = "all"
    )
  })
  expect_s3_class(x, "data.table")
  expect_length(x, 41)
  expect_equal(nrow(x), 1)
  expect_named(
    x,
    c(
      "station_code",
      "latitude",
      "longitude",
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
