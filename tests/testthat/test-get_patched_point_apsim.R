
test_that("get_patched_point_apsim() input checks stop on invalid values", {
  # missing station_code
  expect_error(
    get_patched_point_apsim(
      start_date = "20220101",
      end_date = "20221231",
      api_key = Sys.getenv("SILO_API_KEY")
    )
  )


  # no start_date
  expect_error(
    get_patched_point_apsim(
      station_code = "008137",
      end_date = "20220501",
      api_key = Sys.getenv("SILO_API_KEY")
    )
  )

  # no e-mail (api_key)
  expect_error(
    get_patched_point_apsim(
      station_code = "008137",
      start_date = "20220501",
      end_date = "20220501"
    )
  )
})

test_that("get_patched_point_apsim() returns daily values", {
  vcr::use_cassette("silo_get_patched_point_apsim", {
    skip_if_offline()
    withr::local_timezone(tz = "Australia/Perth")
    expect_warning(wd <- get_patched_point_apsim(
      station_code = "008137",
      start_date = "2021-01-01",
      end_date = "2021-01-31",
      api_key = "slavish_moo_0k@icloud.com"
    ))
  })
  expect_equal(nrow(wd), 30)
  expect_length(wd, 9)
  expect_named(wd,
               c(
                 "year",
                 "day",
                 "radn",
                 "maxt",
                 "mint",
                 "rain",
                 "evap",
                 "vp",
                 "code"
               ))
  expect_s3_class(wd, class = "met")
})
