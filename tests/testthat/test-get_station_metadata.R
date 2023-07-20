
test_that("get_station_metadata() functions properly for which_api = 'SILO'",
          {
            vcr::use_cassette("silo_station_metadata", {
              skip_if_offline()
              x <-
                get_station_metadata(which_api = "silo", include_closed = TRUE)
            }
            )
            expect_length(x, 11)
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
                                     include_closed = TRUE,
                                     rich = FALSE
                                     )
            }
            )
            expect_s3_class(x, "data.table")
            expect_length(x, 11)
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
            vcr::use_cassette("dpird_station_metadata_rich_TRUE", {
              skip_if_offline()
              x <-
                get_station_metadata(
                  which_api = "dpird",
                  api_key = Sys.getenv("DPIRD_API_KEY"),
                  include_closed = TRUE,
                  rich = TRUE
                )
            })
            expect_s3_class(x, "data.table")
            expect_length(x, 36)
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
                "wmo",
                "probe_height",
                "rain_gauge_height",
                "wind_probe_heights",
                "air_temperature",
                "battery_voltage",
                "delta_t",
                "dew_point",
                "pan_evaporation",
                "relative_humidity",
                "barometric_pressure",
                "rainfall",
                "soil_temperature",
                "solar_irradiance",
                "wet_bulb",
                "wind1",
                "wind2",
                "wind3",
                "apparent_temperature",
                "eto_short",
                "eto_tall",
                "frost_condition",
                "heat_condition",
                "wind_erosion_condition",
                "richardson_unit",
                "chill_hour"
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
            expect_length(x, 11)
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
