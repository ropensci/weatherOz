
test_that("get_data_drill_apsim() user-input checks stop on invalid values",
          {
            # no longitude
            expect_error(
              get_data_drill_apsim(
                latitude = -27.85,
                start_date = "20220101",
                end_date = "20221231"
              )
            )

            # no latitude
            expect_error(
              get_data_drill_apsim(
                longitude = 150.05,
                end_date = "20220501"
              )
            )

            # no start date
            expect_error(
              get_data_drill_apsim(
                latitude = -27.85,
                longitude = 150.05,
                end_date = "20220501"
              )
            )

            # no api_key
            expect_error(get_data_drill_apsim(
              latitude = -27.85,
              longitude = 150.05,
              start_date = "20220501",
              api_key = ""
            ))

          })

test_that("get_data_drill_apsim() returns all daily values", {
  vcr::use_cassette("silo_get_data_drill_apsim", {
    skip_if_offline()
    withr::local_timezone(tz = "Australia/Perth")
    wd <- get_data_drill_apsim(
      latitude = -27.85,
      longitude = 150.05,
      start_date = "2021-01-01",
      end_date = "2021-12-31",
      api_key = "slavish_moo_0k@icloud.com"
    )
  })
  expect_s3_class(wd, class = "met")
  expect_identical(dim(wd), c(364L, 9L))
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
