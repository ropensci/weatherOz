
test_that("get_data_drill() user-input checks stop on invalid values", {
  # no longitude
  expect_error(
    get_data_drill(
      latitude = -27.85,
      start_date = "20220501",
      end_date = "20220501"
    )
  )

  # no latitude
  expect_error(get_data_drill(
    longitude = 150.05,
    end_date = "20220501"
  ))

  # too few decimal places in lat/lon
  expect_error(get_data_drill(
    longitude = 150.05,
    latitude = -27.8,
    end_date = "20220501"
  ))

  # no start date
  expect_error(
    get_data_drill(
      latitude = -27.85,
      longitude = 150.05,
      end_date = "20220501"
    )
  )

  # no api_key
  expect_error(get_data_drill(
    latitude = -27.85,
    longitude = 150.05,
    start_date = "20220501",
    api_key = ""
  ))

  # bad values
  expect_error(
    get_data_drill(
      latitude = -27.85,
      longitude = 150.05,
      start_date = "20220501",
      end_date = "20220501",
      values = "Fusarium_thapsinum"
    )
  )
})

test_that("get_data_drill() returns all daily values", {
  vcr::use_cassette("silo_get_data_drill_all_daily_values", {
    skip_if_offline()
    withr::local_timezone(tz = "Australia/Perth")
    wd <- get_data_drill(
      latitude = -27.85,
      longitude = 150.05,
      start_date = "2021-06-01",
      end_date = "2021-06-01",
      api_key = "slavish_moo_0k@icloud.com"
    )
  })
  expect_identical(dim(wd), c(1L, 44L))
  expect_named(
    wd,
    c(
      "longitude",
      "latitude",
      "year",
      "month",
      "day",
      "date",
      "air_tmax",
      "air_tmax_source",
      "air_tmin",
      "air_tmin_source",
      "elev_m",
      "et_morton_actual",
      "et_morton_actual_source",
      "et_morton_potential",
      "et_morton_potential_source",
      "et_morton_wet",
      "et_morton_wet_source",
      "et_short_crop",
      "et_short_crop_source",
      "et_tall_crop",
      "et_tall_crop_source",
      "evap_comb",
      "evap_comb_source",
      "evap_morton_lake",
      "evap_morton_lake_source",
      "evap_pan",
      "evap_pan_source",
      "evap_syn",
      "evap_syn_source",
      "extracted",
      "mslp",
      "mslp_source",
      "radiation",
      "radiation_source",
      "rainfall",
      "rainfall_source",
      "rh_tmax",
      "rh_tmax_source",
      "rh_tmin",
      "rh_tmin_source",
      "vp",
      "vp_deficit",
      "vp_deficit_source",
      "vp_source"
    )
  )
  expect_s3_class(wd, class = "data.table")
})

test_that("get_data_drill() returns selected daily values", {
  vcr::use_cassette("silo_get_data_drill_selected_daily_values", {
    skip_if_offline()
    withr::local_timezone(tz = "Australia/Perth")
    wd <- get_data_drill(
      latitude = -27.85,
      longitude = 150.05,
      values = c("rain", "max_temp", "min_temp"),
      start_date = "2021-06-01",
      end_date = "2021-06-01",
      api_key = "slavish_moo_0k@icloud.com"
    )
  })
  expect_identical(dim(wd), c(1L, 14L))
  expect_named(
    wd,
    c(
      "longitude",
      "latitude",
      "year",
      "month",
      "day",
      "date",
      "air_tmax",
      "air_tmax_source",
      "air_tmin",
      "air_tmin_source",
      "elev_m",
      "extracted",
      "rainfall",
      "rainfall_source"
    )
  )
  expect_s3_class(wd, class = "data.table")
})
