
test_that("get_station_metadata functions properly", {
  skip_on_cran()
  x <- get_station_metadata(check_location = FALSE)
  expect_equal(ncol(x), 13)
  expect_s3_class(x, "data.table")
  expect_named(
    x,
    c(
      "station_code",
      "dist",
      "station_name",
      "start",
      "end",
      "lat",
      "lon",
      "source",
      "state",
      "elev.m",
      "bar_height.m",
      "wmo",
      "status"
    )
  )
})
