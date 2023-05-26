
test_that("get_station_metadata() functions properly for SILO", {
  skip_on_cran()
  skip_if_offline()
  x <-
    get_station_metadata(which_api = "silo")
  expect_equal(ncol(x), 11)
  expect_s3_class(x, "data.table")
  expect_named(
    x,
    c(
      "station_code",
      "station_name",
      "start",
      "end",
      "latitude",
      "longitude",
      "state",
      "elev.m",
      "source",
      "status",
      "wmo"
    )
  )
})

test_that("get_station_metadata() errors if no API key is provide for DPIRD",{
  expect_error(get_station_metadata(which_api = "dpird"))
  expect_error(get_station_metadata(which_api = "all"))
})
