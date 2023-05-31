
test_that("get_station_metadata() functions properly for which_api = 'SILO'",
          {
            skip_if_offline()
            x <-
              get_station_metadata(which_api = "silo", status = TRUE)
            expect_equal(ncol(x), 11)
            expect_s3_class(x, "data.table")
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "start",
                "end",
                "latitude",
                "longitude",
                "state",
                "elev_m",
                "source",
                "status",
                "wmo"
              )
            )
          })

test_that("get_station_metata() functions properly for which_api = 'DPIRD'",
          {
            vcr::use_cassette("dpird_station_metadata", {
              skip_if_offline()
              x <-
                get_station_metadata(which_api = "dpird",
                                     api_key = Sys.getenv("DPIRD_API_KEY"),
                                     status = TRUE,
                                     rich = FALSE
                                     )
            })
            expect_s3_class(x, "data.table")
            expect_equal(ncol(x), 11)
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "start",
                "end",
                "latitude",
                "longitude",
                "state",
                "elev_m",
                "source",
                "status",
                "wmo"
              )
            )
          })

test_that("get_station_metata() functions properly for which_api = 'all'",
          {
            vcr::use_cassette("all_station_metadata", {
              skip_if_offline()
              x <-
                get_station_metadata(which_api = "all",
                                     api_key = Sys.getenv("DPIRD_API_KEY"))
            })
            expect_s3_class(x, "data.table")
            expect_equal(ncol(x), 11)
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "start",
                "end",
                "latitude",
                "longitude",
                "state",
                "elev_m",
                "source",
                "status",
                "wmo"
              )
            )
          })

test_that("get_station_metadata() errors if no API key is provide for DPIRD",
          {
            expect_error(get_station_metadata(which_api = "dpird"))
            expect_error(get_station_metadata(which_api = "all"))
          })
