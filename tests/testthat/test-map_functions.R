# setup for tests ----
rainfall <- read.csv(
    system.file(
      "extdata",
      "raintodate_example_data.csv",
      package = "wrapique",
      mustWork = TRUE
    ),
    stringsAsFactors = FALSE
)
rainfall$last.date <- as.Date(rainfall$last.date)

# make_col_set -----
# test sets - all planned are written
# * col, cuts
# * col, cuts, key
# * col, cuts, sep
test_that("make_col_set() creates correctly formatted dataframe", {

  x <- make_col_set(
    col = rainbow(7),
    cuts = c(-Inf, -35:-30, Inf)
  )

  # check overall class, names, and individual columns
  expect_s3_class(x,
                  "data.frame")
  expect_named(x, c("col", "min", "max", "label"))
  expect_equal(x$col,
               c("#FF0000FF", "#FFDB00FF", "#49FF00FF","#00FF92FF","#0092FFFF", "#4900FFFF", "#FF00DBFF"))
  expect_equal(x$min,
               c(-Inf,  -35,  -34,  -33,  -32,  -31,  -30))
  expect_equal(x$max,
               c(-35, -34, -33, -32, -31, -30, Inf))
  expect_equal(x$label,
               c("< -35",   "-35--34", "-34--33", "-33--32", "-32--31", "-31--30", "> -30"))
})
test_that("make_col_set() key parameter works correctly", {
  # uses a simpler set of numbers
  x <- make_col_set(
    col = rainbow(7),
    cuts = c(0:7),
    key = c("one", "two", "three", "four", "five", "six", "seven")
  )
  # doesn't test overall class or names; doesn't check the colour set; does
  # check the other values
  expect_equal(x$min,
               0:6)
  expect_equal(x$max,
               1:7)
  expect_equal(x$label,
               c("one", "two", "three", "four", "five", "six", "seven")
  )
})
test_that("make_col_set() sep parameter works correctly", {
  expect_snapshot(
    print(
      make_col_set(
        col = rainbow(7),
        cuts = c(0:7),
        sep = "~"
      )
    )
  )
})


# embed_legend ----
# no tests written at present
# assumed that this works if we get a correctly formatted set of maps

# weather_plot ----
# no tests written at present
# assumed to work if we can successfully generate three formats of file:
# * open plot in new window;
# saved jpg
# saved .png

# color_df -----
# not currently tested - plan is to do this later
# two tests sets to be generated
# * with trunc = FALSE
# * with trunc = TRUE
# (same data? separate data? need to decide where the categories are coming from)

# convert_krig_to_col ----
# not currently tested - plan is to do this later
# one test set; requires appropriately formated 'im' object and a colour set.

# map_krig ----
# only testing the one run, just to make sure that the output
# hasn't been changed. Large items are checked using expect_snapshot
# with simplified output
test_that("map_krig() gives the same outputs", {

  x <-  map_krig(
    data = rainfall,
    varname = "rtd",
    lambda = 0.01,
    theta = 1,
    lat_lims = c(-35.5, -27.5),
    long_lims = c(114, 123.5)
  )

  load(
    system.file(
      "extdata",
      "map_krig_comp.rda",
      package = "wrapique",
      mustWork = TRUE
    )
  )

  # check everything
  expect_equal(x, map_krig_comp)

  # # the small items - this was the original
  # set of tests; no longer needed now that
  # the other file is saved.
  # expect_equal(x$nx, c(x = 381L))
  # expect_equal(x$ny, c(x = 321L))
  # expect_equal(x$xlab, "x")
  # expect_equal(x$ylab, "y")
  # expect_equal(x$xy, 1:2)
  # the large ones
  expect_snapshot(
    head(x$x)
  )
  expect_snapshot(
    tail(x$x)
  )
  expect_snapshot(
    head(x$y)
  )
  expect_snapshot(
    tail(x$y)
  )

})
test_that("map_krig() throws errors when it should ", {
  expect_error(
    map_krig(
      data = towns_wa_grainbelt,
      varname = "LATITIDE", # typo
      lambda = 0.01,
      theta = 1,
      lat_lims = c(-35.5, -27.5),
      long_lims = c(114, 123.5)
    )
  )
  expect_error(
    map_krig(
      data = towns_wa_grainbelt,
      varname = "LATITIDE",
      # missing lambda & theta; only one of which has a default value
      lat_lims = c(-35.5, -27.5),
      long_lims = c(114, 123.5)
    )
  )

})


# map_krig_layer ----
# not currently tested - plan is to do this later
# use the same test sets as map_krig; output will differ enough
# for this to be sufficient
test_that("map_krig_layer() gives the same outputs", {

  x <-  map_krig_layer(
    data = rainfall,
    varname = "rtd",
    lambda = 0.01,
    lat_lims = c(-35.5, -27.5),
    long_lims = c(114, 123.5),
    col = make_col_set(
      col = rainbow(7),
      cuts = c(0:6*150, Inf)
    ),
    agregion = agregion_img,
    coast =  coast_img
  )

  load(
    system.file(
      "extdata",
      "map_krig_layer_comp.rda",
      package = "wrapique",
      mustWork = TRUE
    )
  )

  # check everything
  expect_equal(x, map_krig_layer_comp)

})

# map_weather ----
# not currently tested - plan is to do this later
#

# map_weather_swld ----
# testing here is incomplete; need a way to test that
# an image is generated, and that saved images are correct.
# These are not priorities. Testing sets
# * just the three necessary parameters
# * parameters for creating a saved file
# * ? test set with everything passed?

test_that("map_weather_swld() saves a .png object", {
  data <- towns_wa_grainbelt
  lat.col <- make_col_set(col = rainbow(7),
                          cuts = c(-Inf, -35:-30, Inf)
  )
  path.test <- tempdir()
  wrapique::map_weather_swld(
    data = data,
    varname = "LATITUDE",
    col_df = lat.col,
    plot_type = 4,
    name = "test_png",
    html_path = path.test
  )
  # vdiffr::expect_doppelganger(
  #   "A png file",
  #   file.path(
  #     path.test,
  #     "test_png.png"
  #   )
  # )

  expect_true(
    file.exists(
      file.path(
        path.test,
        "test_png.png" )
    )
  )

})

# test_that("map_weather_swld() returns an image", {
#   data <- towns_wa_grainbelt
#   lat.col <- make_col_set(col = rainbow(7),
#                           cuts = c(-Inf, -35:-30, Inf)
#   )
#   x <- wrapique::map_weather_swld(
#     data = data,
#     varname = "LATITUDE",
#     col_df = lat.col,
#     plot_type = 0
#   )
#   vdiffr::expect_doppelganger(
#     "A png file",
#     x
#   )
# })
