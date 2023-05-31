
test_that("get_dpird_minute() returns minute values", {
              vcr::use_cassette("dpird_minute_summaries", {
                skip_if_offline()
                x <- get_dpird_minute(
                  station_code = "SP",
                  start_date_time = "2018-02-01 13:00:00",
                  minutes = 1440,
                  api_key = Sys.getenv("DPIRD_API_KEY"),
                  which_values = "wind"
                )
              })
            expect_s3_class(x, "data.table")
            expect_equal(ncol(x), 8)
            expect_named(
              x,
              c(
                "station_code",
                "date_time",
                "wind.height",
                "wind.avg.speed",
                "wind.avg.direction.compass_point",
                "wind.avg.direction.degrees",
                "wind.min.speed",
                "wind.max.speed"
              )
            )
            expect_type(x$station_code, "character")
            expect_s3_class(x$date_time, "POSIXct")
          })
