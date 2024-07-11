
test_that("find_nearby_stations() w/ which_api = 'dpird' & station_code",
          {
            vcr::use_cassette("nearby_dpird_stations_dpird_code", {
              skip_if_offline()
              x <- find_nearby_stations(
                station_code = "NO",
                distance_km = 20,
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

test_that("find_nearby_stations() w/ which_api = 'dpird' & lonlat w/ no stns",
          {
            vcr::use_cassette("nearby_dpird_stations_lonlat_with_no_stations", {
              skip_if_offline()
              expect_message(
                x <- find_nearby_stations(
                  longitude = 147.4575,
                  latitude = -35.1583,
                  distance_km = 20,
                  which_api = "dpird"
                )
              )
            })
            expect_length(x, 0)
          })

test_that("find_nearby_stations() w/ which_api = 'dpird' & lonlat w/ stations",
          {
            vcr::use_cassette("nearby_dpird_stations_lonlat_with_stations", {
              skip_if_offline()
                x <- find_nearby_stations(
                  longitude = 116.694194,
                  latitude = -31.651611,
                  distance_km = 20,
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

test_that("find_nearby_stations() w/ which_api = 'all' & latlon",
          {
            vcr::use_cassette("nearby_all_stations_latlon", {
              skip_if_offline()
              x <- find_nearby_stations(
                longitude = 116.694194,
                latitude = -31.651611,
                distance_km = 10,
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

test_that("find_nearby_stations() w/ which_api = 'all' & SILO station",
          {
            vcr::use_cassette("nearby_all_stations_SILO_station_code", {
              skip_if_offline()
              x <- find_nearby_stations(
                station_code = "010111",
                distance_km = 10,
                which_api = "all",
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

test_that("find_nearby_stations() w/ which_api = 'all' & DPIRD station",
          {
            vcr::use_cassette("nearby_all_stations_DPIRD_station_code", {
              skip_if_offline()
              x <- find_nearby_stations(
                station_code = "NO",
                distance_km = 10,
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

test_that("find_nearby_stations() w/ which_api = 'silo' & station_code",
          {
            vcr::use_cassette("metadata_silo_station", {
              skip_if_offline()
              x <- find_nearby_stations(
                latitude = -35.1583,
                longitude = 147.4575,
                distance_km = 10,
                which_api = "silo"
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

test_that("find_nearby_stations() w/ which_api = 'silo' & lonlat",
          {
            vcr::use_cassette("metadata_silo_station", {
              skip_if_offline()
              x <- find_nearby_stations(
                latitude = -35.1583,
                longitude = 147.4575,
                distance_km = 10,
                which_api = "silo"
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


test_that("find_nearby_stations() fails on DPIRD API w/ no api_key",
          {
              skip_if_offline()
              expect_error(find_nearby_stations(
                latitude = -35.1583,
                longitude = 147.4575,
                distance_km = 10,
                which_api = "dpird",
                api_key = ""
              ))
          })

test_that("find_nearby_stations() fails on all API w/ no api_key",
          {
            skip_if_offline()
            expect_error(find_nearby_stations(
              latitude = -35.1583,
              longitude = 147.4575,
              distance_km = 10,
              which_api = "all",
              api_key = ""
            ))
          })
