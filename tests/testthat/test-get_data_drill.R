
test_that("get_data_drill() returns daily values", {
  vcr::use_cassette("silo_get_data_drill_daily_values", {
    skip_if_offline()
    wd <- get_data_drill(
      latitude = -27.85,
      longitude = 150.05,
      start_date = "2021-06-01",
      end_date = "2021-07-01",
      api_key = Sys.getenv("SILO_API_KEY")
    )
  })
  expect_equal(nrow(wd), 31)
  expect_length(wd, 43)
  expect_named(
    wd,
    c(
      "longitude",
      "latitude",
      "elev_m",
      "date",
      "year",
      "month",
      "day",
      "daily_rain",
      "daily_rain_source",
      "max_temp",
      "max_temp_source",
      "min_temp",
      "min_temp_source",
      "vp",
      "vp_source",
      "vp_deficit",
      "vp_deficit_source",
      "evap_pan",
      "evap_pan_source",
      "evap_syn",
      "evap_syn_source",
      "evap_comb",
      "evap_comb_source",
      "evap_morton_lake",
      "evap_morton_lake_source",
      "radiation",
      "radiation_source",
      "rh_tmax",
      "rh_tmax_source",
      "rh_tmin",
      "rh_tmin_source",
      "et_short_crop",
      "et_short_crop_source",
      "et_tall_crop",
      "et_tall_crop_source",
      "et_morton_actual",
      "et_morton_actual_source",
      "et_morton_potential",
      "et_morton_potential_source",
      "et_morton_wet",
      "et_morton_wet_source",
      "mslp",
      "mslp_source"
    )
  )
  expect_s3_class(wd, class = "data.table")
})
