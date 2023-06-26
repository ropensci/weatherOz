test_that("find_forecast_towns returns correct default", {
  DT <- find_forecast_towns()
  expect_named(DT,
               c("aac", "town", "longitude", "latitude", "elev_m", "distance"))
})
