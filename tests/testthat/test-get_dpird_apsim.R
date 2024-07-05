
test_that("get_dpird_apsim() returns a 'met' object",
          {
            vcr::use_cassette("dpird_get_apsim", {
              skip_if_offline()
              expect_warning(x <- get_dpird_apsim(
                station_code = "BI",
                start_date = "20170101",
                end_date = "20170101",
                api_key = Sys.getenv("DPIRD_API_KEY")
              ))
            })
            expect_s3_class(x, "met")
            expect_length(x, 9)
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
