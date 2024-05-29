
test_that("find_stations_in() works with {sf} polygon", {
  vcr::use_cassette("find_stations_in_polygon_all", {
    x <- find_stations_in(
      x = south_west_agricultural_region,
      api_key = Sys.getenv("DPIRD_API_KEY"),
      include_closed = TRUE,
      crs = sf::st_crs(south_west_agricultural_region)
    )
  })

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
  expect_true(unique(x$state) == "WA")
})

test_that("find_stations_in() works with {sf} polygon and centroid", {
  vcr::use_cassette("find_stations_in_polygon_centroid", {
    x <- find_stations_in(
      x = south_west_agricultural_region,
      api_key = Sys.getenv("DPIRD_API_KEY"),
      include_closed = FALSE,
      centroid = TRUE,
      crs = sf::st_crs(south_west_agricultural_region)
    )
  })
  expect_length(x, 11)
  expect_equal(nrow(x), 5)
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
  expect_true(unique(x$state) == "WA")
})

test_that("find_stations_in() works with bbox", {
  vcr::use_cassette("find_stations_in_bbox", {
    x <- find_stations_in(
      x = c(144.470215, -38.160476, 145.612793, -37.622934),
      which_api = "SILO",
      include_closed = FALSE
    )
  })
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
  expect_true(unique(x$state) == "VIC")
})
