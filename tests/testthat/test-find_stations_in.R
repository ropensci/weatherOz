
test_that("find_stations_in() works with {sf} polygon", {
  vcr::use_cassette("metadata_all_cassette", {
    x <- find_stations_in(
      x = south_west_agricultural_region,
      include_closed = FALSE,
      crs = sf::st_crs(south_west_agricultural_region)
    )
  })

  expect_identical(dim(x), c(679L, 11L))
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
  vcr::use_cassette("metadata_all_cassette", {
    x <- find_stations_in(
      x = south_west_agricultural_region,
      include_closed = FALSE,
      centroid = TRUE,
      crs = sf::st_crs(south_west_agricultural_region)
    )
  })
  expect_identical(dim(x), c(5L, 11L))
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
  vcr::use_cassette("metadata_all_cassette", {
    x <- find_stations_in(
      x = c(144.470215, -38.160476, 145.612793, -37.622934),
      which_api = "SILO",
      include_closed = FALSE
    )
  })
  expect_identical(dim(x), c(37L, 11L))
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
