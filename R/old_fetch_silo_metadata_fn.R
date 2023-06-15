.fetch_silo_metadata <- function(.check_location = check_location) {
  tryCatch({
    curl::curl_download(
      url = "ftp://ftp.bom.gov.au/anon2/home/ncc/metadata/sitelists/stations.zip",
      destfile = file.path(tempdir(), "stations.zip"),
      mode = "wb",
      quiet = TRUE
    )
  }, error = function(x)
    stop(
      "\nThe server with the location information is not responding. ",
      "Please retry again later.\n",
      call. = FALSE
    ))
  utils::unzip(file.path(tempdir(), "stations.zip"), exdir = tempdir())

  file_in <- file.path(tempdir(), "stations.txt")
  bom_stations <-
    data.table::setDT(
      readr::read_fwf(
        file = file_in,
        na = c("..", "...", ".....", " "),
        skip = 4,
        col_positions = readr::fwf_cols(
          station_code = c(1,             8),
          dist = c(9, 14),
          station_name = c(15, 55),
          start = c(56,             63),
          end = c(64, 71),
          lat = c(72, 80),
          lon = c(81,             90),
          source = c(91, 105),
          state = c(106, 109),
          elev.m = c(110,             120),
          bar_height.m = c(121, 129),
          wmo = c(130, 136)
        ),
        col_types = c(
          station_code = readr::col_character(),
          dist = readr::col_character(),
          site_name = readr::col_character(),
          start = readr::col_integer(),
          end = readr::col_integer(),
          lat = readr::col_double(),
          lon = readr::col_double(),
          source = readr::col_character(),
          state = readr::col_character(),
          elev.m = readr::col_double(),
          bar_height.m = readr::col_double(),
          wmo = readr::col_integer()
        ),
        n_max = length(utils::count.fields(file_in)) - 6
      )
    )
  bom_stations[, `:=`(station_code, as.factor(station_code))]
  bom_stations[, `:=`(station_name, DescTools::StrCap(x = tolower(station_name),
                                                      method = "word"))]
  bom_stations[, `:=`(start, as.integer(start))]
  bom_stations[, `:=`(end, as.integer(end))]
  bom_stations[, `:=`(status, ifelse(!is.na(end), "closed",
                                     "open"))]
  bom_stations[, `:=`(dist, NULL)]
  bom_stations[, `:=`(source, NULL)]
  bom_stations[, `:=`(bar_height.m, NULL)]
  bom_stations[, `:=`(source, "Bureau of Meteorology")]
  if (requireNamespace("ASGS.foyer", quietly = TRUE)) {
    if (isTRUE(.check_location)) {
      message(
        "The package {ASGS.foyer} is installed. Station locations will\n",
        "be checked against lat/lon location values and corrected if necessary."
      )             data.table::setDT(bom_stations)

      latlon2state <-
        function(lat, lon) {
          ASGS.foyer::latlon2SA(lat,
                                lon,
                                to = "STE",
                                yr = "2016",
                                return = "v")
        }             bom_stations[lon > -50, `:=`(state_from_latlon, latlon2state(lat,
                                                                                   lon))]
      bom_stations[state_from_latlon == "New South Wales",
                   `:=`(actual_state, "NSW")]
      bom_stations[state_from_latlon == "Victoria", `:=`(actual_state,
                                                         "VIC")]
      bom_stations[state_from_latlon == "Queensland", `:=`(actual_state,
                                                           "QLD")]
      bom_stations[state_from_latlon == "South Australia",
                   `:=`(actual_state, "SA")]
      bom_stations[state_from_latlon == "Western Australia",
                   `:=`(actual_state, "WA")]
      bom_stations[state_from_latlon == "Tasmania", `:=`(actual_state,
                                                         "TAS")]
      bom_stations[state_from_latlon == "Australian Capital Territory",
                   `:=`(actual_state, "ACT")]
      bom_stations[state_from_latlon == "Northern Territory",
                   `:=`(actual_state, "NT")]
      bom_stations[actual_state != state &&
                     state %notin% c("ANT", "ISL"),
                   `:=`(state, actual_state)]
      bom_stations[, `:=`(actual_state, NULL)]
    }
  }

  data.table::setcolorder(
    bom_stations,
    neworder = c(
      "station_code",
      "station_name",
      "start",
      "end",
      "lat",
      "lon",
      "state",
      "elev.m",
      "source",
      "status",
      "wmo"
    )
  )

  silo_stations <-
    find_nearby_stations(
      latitude = -25.5833,
      longitude = 134.5667,
      distance_km = 10000,
      which_api = "silo"
    )

  return(bom_stations[toupper(station_name) %in% silo_stations$station_name])
}
