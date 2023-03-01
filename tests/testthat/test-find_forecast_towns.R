test_that("sweep_for_forecast_towns returns correct default", {
  DT <- find_forecast_towns()
  expect_named(DT, c("aac", "town", "lon", "lat", "elev", "distance"))
})
