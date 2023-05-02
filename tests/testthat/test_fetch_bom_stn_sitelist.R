
test_that("fetch_bom_stn_sitelist() functions properly", {
  skip_on_cran()
  x <- fetch_bom_stn_sitelist()
  expect_s3_class(x, "data.table")
  expect_equal(ncol(x), 13)
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
