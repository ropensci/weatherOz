
test_that("get_data_drill_apsim() user-input checks stop on invalid values",
          {
            # no longitude
            expect_error(
              get_data_drill_apsim(
                latitude = -27.85,
                start_date = "20220501",
                end_date = "20220501",
                api_key = Sys.getenv("SILO_API_KEY")
              )
            )

            # no latitude
            expect_error(
              get_data_drill_apsim(
                longitude = 150.05,
                end_date = "20220501",
                api_key = Sys.getenv("SILO_API_KEY")
              )
            )

            # no start date
            expect_error(
              get_data_drill_apsim(
                latitude = -27.85,
                longitude = 150.05,
                end_date = "20220501",
                api_key = Sys.getenv("SILO_API_KEY")
              )
            )

            # no api_key
            expect_error(get_data_drill_apsim(
              latitude = -27.85,
              longitude = 150.05,
              start_date = "20220501"
            ))

          })

test_that("get_data_drill_apsim() returns all daily values", {
  vcr::use_cassette("silo_get_data_drill_apsim", {
    skip_if_offline()
    withr::local_timezone(tz = "Australia/Perth")
    wd <- get_data_drill_apsim(
      latitude = -27.85,
      longitude = 150.05,
      start_date = "2021-06-01",
      end_date = "2021-07-01",
      api_key = Sys.getenv("SILO_API_KEY"),
    )
  })
  expect_s3_class(wd, class = "met")
  expect_equal(nrow(wd), 31)
  expect_length(wd, 9)
  expect_named(wd,
               c(
                 "year",
                 "day",
                 "radn",
                 "maxt",
                 "mint",
                 "rain",
                 "evap",
                 "vp",
                 "code"
               ))
})
