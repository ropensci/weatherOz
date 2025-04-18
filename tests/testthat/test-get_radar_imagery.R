# get_available_radar()---------------------------------------------------------
test_that("get_available_radar error handling works", {
  skip_on_cran()
  expect_error(expect_warning(get_available_radar(radar_id = "abc")))
})

test_that("get_available_radar functions properly", {
  skip_on_cran()

  x <- get_available_radar(radar_id = "all")
  expect_s3_class(x, "data.frame")
  expect_gt(nrow(x), 0)

  xxx <- get_available_radar(radar_id = 3)
  expect_s3_class(xxx, "data.frame")
  expect_gt(nrow(xxx), 0)
  expect_lt(nrow(xxx), 5)
  expect_identical(unique(xxx$Radar_id), 3L)
})

# get_radar_imagery()-----------------------------------------------------------
test_that("Error handling works", {
  skip_on_cran()
  expect_error(expect_warning(get_radar_imagery(product_id = "abc")))
  expect_error(expect_warning(get_radar_imagery()))
  expect_error(get_radar_imagery(c("IDR644", "IDR644")))
})

test_that("get_radar_imagery functions properly", {
  skip_on_cran()
  y <- get_radar_imagery(product_id = "IDR644")
  expect_type(y, "externalptr")

  yy <- get_radar_imagery(product_id = "IDR644", download_only = TRUE)
  expect_null(yy)
})
