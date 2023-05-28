test_that("get_dpird_summaries() returns yearly values",
          {
            vcr::use_cassette("dpird_yearly_summaries", {
              skip_if_offline()
              x <- get_dpird_summaries(
                station_code = "BI",
                start_date = "20171028",
                end_date = "20231028",
                api_key = Sys.getenv("DPIRD_API_KEY"),
                interval = "yearly",
                which_values = "wind"
              )
            })
            expect_s3_class(x, "data.table")
            expect_equal(ncol(x), 13)
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "period.year",
                "period.month",
                "period.day",
                "period.hour",
                "period.minute",
                "wind.avg.speed",
                "wind.height",
                "wind.max.direction.compass_point",
                "wind.max.direction.degrees",
                "wind.max.speed",
                "wind.max.time"
              )
            )
          })
