
test_that("user-input checks stop if invalid values are provided", {
  # missing station code
  expect_error(
    get_dpird_extremes(
      api_key = Sys.getenv("DPIRD_API_KEY"),
      values = "wind"
    )
  )

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
  }
  )
  expect_s3_class(x, "data.table")
  expect_equal(ncol(x), 8)
  expect_named(
    x,
    c(

    )
  )

})

