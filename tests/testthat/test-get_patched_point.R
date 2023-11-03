
test_that("get_patched_point() user-input checks stop on invalid values", {
  # missing station code
  expect_error(
    get_patched_point(
      start_date = "20220501",
      end_date = "20220501",
      api_key = Sys.getenv("SILO_API_KEY")
    )
  )

  expect_error(
    get_patched_point(
      station_code = "008137",
      end_date = "20220501",
      api_key = Sys.getenv("SILO_API_KEY")
    )
  )

  expect_error(
    get_patched_point(
      station_code = "008137",
      start_date = "20220501",
      end_date = "20220501"
    )
  )

  expect_error(
    get_patched_point(
      station_code = "008137",
      start_date = "20220501",
      end_date = "20220501",
      api_key = Sys.getenv("SILO_API_KEY"),
      values = "Fusarium_verticilliodes"
    )
  )
})

test_that("get_patched_point() returns daily values", {
  vcr::use_cassette("silo_get_patched_point_daily_values", {
    skip_if_offline()
    withr::local_timezone(tz = "Australia/Perth")
    expect_message(
      wd <- get_patched_point(
        station_code = "008137",
        start_date = "2021-06-01",
        end_date = "2021-07-01",
        api_key = Sys.getenv("SILO_API_KEY"),
      )
    )
  })
  expect_equal(nrow(wd), 31)
  expect_length(wd, 46)
  expect_named(
    wd,
    c(
      "station_code",
      "station_name",
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
      "latitude",
      "longitude",
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

test_that("get_patched_point() returns selected daily values", {
  vcr::use_cassette("silo_get_patched_point_selected_daily_values",
                    {
                      skip_if_offline()
                      withr::local_timezone(tz = "Australia/Perth")
                      expect_message(
                        wd <- get_patched_point(
                          station_code = "008137",
                          values = c("rain", "max_temp", "min_temp"),
                          start_date = "2021-06-01",
                          end_date = "2021-07-01",
                          api_key = Sys.getenv("SILO_API_KEY"),
                        )
                      )
                    })
  expect_equal(nrow(wd), 31)
  expect_length(wd, 16)
  expect_named(
    wd,
    c(
      "station_code",
      "station_name",
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
      "latitude",
      "longitude",
      "rainfall",
      "rainfall_source"
    )
  )
  expect_s3_class(wd, class = "data.table")
})
