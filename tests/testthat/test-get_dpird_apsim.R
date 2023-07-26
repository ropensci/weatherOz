
test_that("get_dpird_apsim() returns a 'met' object",
          {
            vcr::use_cassette("dpird_apsim", {
              skip_if_offline()
              x <- get_dpird_apsim(
                station_code = "BI",
                start_date = "20170228",
                end_date = "20170228",
                api_key = Sys.getenv("DPIRD_API_KEY")
              )
            })
            expect_s3_class(x, "met")
            expect_length(x, 8)
            expect_named(x,
                         c(
                           "year",
                           "day",
                           "radn",
                           "maxt",
                           "mint",
                           "rain",
                           "rh",
                           "windspeed"
                         ))
          })
