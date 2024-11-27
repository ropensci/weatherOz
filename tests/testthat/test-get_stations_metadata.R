
test_that("get_stations_metadata() functions properly for which_api = 'SILO'",
          {
            vcr::use_cassette("metadata_silo_station", {
              skip_if_offline()
              x <-
                get_stations_metadata(which_api = "silo", include_closed = TRUE)
            }
            )
            expect_identical(dim(x), c(7995L, 11L))
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

test_that("get_stations_metadata() functions properly for which_api = 'DPIRD'",
          {
            vcr::use_cassette("metadata_dpird_station", {
              skip_if_offline()
              x <-
                get_stations_metadata(which_api = "dpird",
                                     api_key = Sys.getenv("DPIRD_API_KEY"),
                                     include_closed = TRUE,
                                     rich = FALSE
                                     )
            }
            )
            expect_s3_class(x, "data.table")
            expect_identical(dim(x), c(239L, 11L))
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

test_that("get_stations_metadata() functions properly for which_api = 'DPIRD'
          w/ rich data",
          {
            vcr::use_cassette("metadata_dpird_station_rich_TRUE", {
              skip_if_offline()
              x <-
                get_stations_metadata(
                  which_api = "dpird",
                  api_key = Sys.getenv("DPIRD_API_KEY"),
                  include_closed = TRUE,
                  rich = TRUE
                )
            })
            expect_s3_class(x, "data.table")
            expect_identical(dim(x), c(239L, 36L))
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
            vcr::use_cassette("metadata_all_cassette", {
              skip_if_offline()
              x <-
                get_stations_metadata(which_api = "all",
                                     api_key = Sys.getenv("DPIRD_API_KEY"))
            })

            expect_s3_class(x, "data.table")
            expect_identical(dim(x), c(3883L, 11L))
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

test_that("get_stations_metadata() fuzzy matches station names",
          {
            vcr::use_cassette("metadata_all_cassette", {
              skip_if_offline()
              x <-
                get_stations_metadata(api_key = Sys.getenv("DPIRD_API_KEY"),
                                     station_name = "Brisbane")
            })

            expect_s3_class(x, "data.table")
            expect_identical(dim(x), c(3L, 11L))
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
            expect_identical(x$station_name,
                         c("Brisbane", "Brisbane Aero", "Mt Brisbane"))
          })

test_that("get_stations_metadata() matches station_codes",
            {
              vcr::use_cassette("metadata_all_cassette", {
                skip_if_offline()
                x <-
                  get_stations_metadata(
                    api_key = Sys.getenv("DPIRD_API_KEY"),
                    station_code = c("YU001", "YU002", "YU003")
                  )
              })

            expect_s3_class(x, "data.table")
            expect_identical(dim(x), c(3L, 11L))
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
            expect_identical(as.character(x$station_code),
                         c("YU001", "YU002", "YU003"))
          })

# no api_key
test_that("get_stations_metadata() errors if no api_key is provide for DPIRD",
          {
            expect_error(get_stations_metadata(which_api = "dpird",
                                               api_key = ""))
            expect_error(get_stations_metadata(which_api = "all",
                                               api_key = ""))
          })
