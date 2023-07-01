
test_that("get_station_metadata() functions properly for which_api = 'SILO'",
          {
            vcr::use_cassette("nearby_dpird_stations", {
              skip_if_offline()
              x <- find_nearby_stations(
                station_code = "NO",
                distance_km = 50,
                api_key = Sys.getenv("DPIRD_API_KEY"),
                which_api = "dpird"
              )
            })
            expect_length(x, 8)
            expect_s3_class(x, "data.table")
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "longitude",
                "latitude",
                "state",
                "elev_m",
                "owner",
                "distance_km"
              )
            )
          })

test_that("get_station_metata() functions properly for which_api = 'DPIRD'",
          {
            vcr::use_cassette("nearby_all_stations", {
              skip_if_offline()
              x <- find_nearby_stations(
                station_code = "NO",
                distance_km = 50,
                api_key = Sys.getenv("DPIRD_API_KEY"),
                which_api = "all"
              )
            })
            expect_s3_class(x, "data.table")
            expect_length(x, 8)
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "longitude",
                "latitude",
                "state",
                "elev_m",
                "owner",
                "distance_km"
              )
            )
          })

test_that("get_station_metata() functions properly for which_api = 'all'",
          {
            vcr::use_cassette("nearby_silo_stations", {
              skip_if_offline()
              x <- find_nearby_stations(
                latitude = -35.1583,
                longitude = 147.4575,
                distance_km = 200,
                which_api = "silo"
              )
            },
            record = "new_episodes")
            expect_s3_class(x, "data.table")
            expect_length(x, 8)
            expect_named(
              x,
              c(
                "station_code",
                "station_name",
                "longitude",
                "latitude",
                "state",
                "elev_m",
                "owner",
                "distance_km"
              )
            )
          })
