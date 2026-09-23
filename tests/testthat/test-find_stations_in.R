
test_that("find_stations_in() works with {sf} polygon", {
  skip_on_cran()
  skip_if_offline()
  vcr::use_cassette("metadata_all_cassette", {
    x <- find_stations_in(
      x = south_west_agricultural_region,
      include_closed = FALSE,
      crs = sf::st_crs(south_west_agricultural_region)
    )
  })

  expect_equal(ncol(x), 11L)
  # The station count depends on the live BOM station list, which drifts over
  # time, so test behaviour rather than a fixed count: a substantial number of
  # stations is returned and every one of them lies inside the polygon.
  expect_gt(nrow(x), 500L)
  stations_sf <- sf::st_as_sf(x, coords = c("longitude", "latitude"), crs = 4326)
  area <- sf::st_transform(south_west_agricultural_region, crs = 4326)
  expect_true(all(lengths(sf::st_within(stations_sf, area)) > 0))
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
  skip_on_cran()
  skip_if_offline()
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
  skip_on_cran()
  skip_if_offline()
  vcr::use_cassette("metadata_all_cassette", {
    x <- find_stations_in(
      x = c(144.470215, -38.160476, 145.612793, -37.622934),
      which_api = "SILO",
      include_closed = FALSE
    )
  })
  expect_equal(ncol(x), 11L)
  # Count drifts with the live station lists; check behaviour instead.
  expect_gt(nrow(x), 20L)
  stations_sf <- sf::st_as_sf(x, coords = c("longitude", "latitude"), crs = 4326)
  bbox_sf <- sf::st_as_sfc(sf::st_bbox(c(xmin = 144.470215, ymin = -38.160476,
                                         xmax = 145.612793, ymax = -37.622934),
                                       crs = 4326))
  expect_true(all(lengths(sf::st_within(stations_sf, bbox_sf)) > 0))
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
