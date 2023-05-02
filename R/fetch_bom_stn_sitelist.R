#' Get BOM station site list for metadata and ag bulletion location updates
#'
#' Fetches and formats the BOM stations site-list for use in
#'  `update_ag_station_locations()` and `get_station_metadata()`.
#'
#' @return A `data.table` of BOM station information
#'
#' @example fetch_bom_stn_sitelist()
#'
#' @noRd
#' @keywords internal

fetch_bom_stn_sitelist <- function() {
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
        na = c("..", ".....", " "),
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
          "elev.m" = c(110, 120),
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
          elev.m = readr::col_double(),
          bar_height.m = readr::col_double(),
          wmo = readr::col_integer()
        ),
        n_max = length(utils::count.fields(file_in)) - 6,
        # drop last six rows
      )
    )

  bom_stations[, station_code := as.factor(station_code)]
  bom_stations[, station_name := DescTools::StrCap(
    x = tolower(bom_stations$station_name),
    method = "word")]
  bom_stations[, start := as.integer(start)]
  bom_stations[, end := as.integer(end)]
  bom_stations[, status := ifelse(!is.na(end), "Closed", "Open")]
  bom_stations[is.na(end), end := as.integer(format(Sys.Date(), "%Y"))]

  return(bom_stations)
}
