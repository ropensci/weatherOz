# get_ag_bulletin() ------------------------------------------------------------
# Test function is defunct
test_that("query_bulletin is defunct", {
  skip_on_cran()
  expect_error(get_ag_bulletin(state = "AUS"), "defunct", fixed = TRUE)
})

# parse_ag_bulletin() ----------------------------------------------------------
# Test function is defunct
test_that("parse_bulletin is defunct", {
  expect_error(parse_ag_bulletin(), "defunct", fixed = TRUE)
})
