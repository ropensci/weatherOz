#' Fetch and Clean BOM Metadata for SILO Stations
#'
#' @return a \CRANpkg{data.table} of BOM station metadata.
#' @keywords internal
#' @noRd

.get_bom_metadata <- function() {
  tryCatch({
    curl::curl_download(
      url =
        "ftp://ftp.bom.gov.au/anon2/home/ncc/metadata/sitelists/stations.zip",
      destfile = file.path(tempdir(), "stations.zip"),
      mode = "wb",
      quiet = TRUE
    )
  },
  error = function(x)
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
          "station_code" = c(1, 8),
          "dist" = c(9, 14),
          "station_name" = c(15, 55),
          "start" = c(56, 63),
          "end" = c(64, 71),
          "lat" = c(72, 80),
          "lon" = c(81, 90),
          "source" = c(91, 105),
          "state" = c(106, 109),
          "elev_m" = c(110, 120),
          "bar_height.m" = c(121, 129),
          "wmo" = c(130, 136)
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
          elev_m = readr::col_double(),
          bar_height.m = readr::col_double(),
          wmo = readr::col_integer()
        ),
        # drop last six rows
        n_max = length(utils::count.fields(file_in)) - 6
      )
    )

  bom_stations[, station_code := trimws(station_code)]
  data.table::setkey(x = bom_stations, station_code)
  bom_stations[, station_code := as.factor(sprintf("%06s", station_code))]
  bom_stations[, station_name := trimws(station_name)]
  bom_stations[, station_name := .strcap(x = station_name)]
  bom_stations[, start := as.integer(start)]
  bom_stations[, end := as.integer(end)]
  bom_stations[, status := ifelse(!is.na(end), "closed", "open")]
  bom_stations[, dist := NULL]
  bom_stations[, source := NULL]
  bom_stations[, bar_height.m := NULL]
  bom_stations[, source := "Bureau of Meteorology"]

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
      "elev_m",
      "source",
      "status",
      "wmo"
    )
  )

  data.table::setnames(
    bom_stations,
    old = c("lat", "lon"),
    new = c("latitude", "longitude")
  )
  return(bom_stations[])
}
