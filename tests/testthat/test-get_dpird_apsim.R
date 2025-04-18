
test_that("get_dpird_apsim() returns a 'met' object", {
  vcr::use_cassette("dpird_get_apsim", {
    skip_if_offline()
    expect_warning(
      x <- get_dpird_apsim(
        station_code = "BI",
        start_date = "20170101",
        end_date = "20171231"
      )
    )
  })
  expect_s3_class(x, "met")
  expect_identical(dim(x), c(365L, 9L))
  expect_named(x,
               c(
                 "year",
                 "day",
                 "radn",
                 "maxt",
                 "mint",
                 "rain",
                 "evap",
                 "rh",
                 "windspeed"
               ))
})
