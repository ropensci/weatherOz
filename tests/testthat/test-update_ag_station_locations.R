
test_that("update_ag_station_locations() stops if 'no'", {
  skip_on_cran()

  f <- file()
  options(weatherOz.connection = f)
  answer <- "no"
  write(answer, f)
  expect_error(update_ag_station_locations())
  options(weatherOz.connection = stdin())
  close(f)
})


test_that("update_station_locations() downloads and imports the proper file", {
  skip_on_cran()
  f <- file()
  options(weatherOz.connection = f)
  ans <- "yes"
  write(ans, f)
  update_ag_station_locations()

  # Load AAC code/town name list to join with final output
  load(system.file("extdata",
                   "stations_site_list.rda",
                   package = "weatherOz"))

  expect_equal(ncol(stations_site_list), 11)
  expect_named(
    stations_site_list,
    c(
      "site",
      "dist",
      "name",
      "start",
      "end",
      "lat",
      "lon",
      "state",
      "elev",
      "bar_ht",
      "wmo"
    )
  )

  # reset connection
  options(weatherOz.connection = stdin())
  # close the file
  close(f)
})
