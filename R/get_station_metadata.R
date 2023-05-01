
#' Get latest BOM station metadata
#'
#' Download the latest station locations and metadata.
#'
#' If \CRANpkg{ASGS.foyer} is installed locally, this function will
#' automatically check and correct any invalid state values for stations located
#' in Australia.
#'
#' @examples
#' \dontrun{
#' get_station_metadata()
#' }
#' @return a `data.table` of BOM weather station metadata.
#'
#' @references
#' Station location and other metadata are sourced from the Australian Bureau of
#' Meteorology (\acronym{BOM}) webpage, Bureau of Meteorology Site Numbers:\cr
#' \url{http://www.bom.gov.au/climate/cdo/about/site-num.shtml}
#'
#' @family BOM
#'
#' @author Adam H. Sparks, \email{adam.sparks@@dpird.wa.gov.au}
#' @export

get_station_metadata <- function() {
  # CRAN NOTE avoidance
  site <- state_code <- wmo <- state <- lon <- lat <- # nocov start
    actual_state <-
    state_from_latlon <- start <- end <- NULL # nocov end


  tryCatch({
    curl::curl_download(
      url =
        "ftp://ftp.bom.gov.au/anon2/home/ncc/metadata/sitelists/stations.zip",
      destfile = file.path(tempdir(), "stations.zip"),
      mode = "wb",
      quiet = FALSE
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
    data.table::setDT(readr::read_fwf(
      file = file_in,
      na = c("..", ".....", " "),
      skip = 4,
      col_positions = readr::fwf_cols("site" = c(1, 8),
                                      "dist" = c(9, 14),
                                      "name" = c(15, 55),
                                      "start" = c(56, 63),
                                      "end" = c(64, 71),
                                      "lat" = c(72, 80),
                                      "lon" = c(81, 90),
                                      "source" = c(91, 105),
                                      "state" = c(106, 109),
                                      "elev.m" = c(110, 120),
                                      "bar_height.m" = c(121, 129),
                                      "wmo" = c(130, 136)),
      col_types = c(
        site = readr::col_character(),
        dist = readr::col_character(),
        name = readr::col_character(),
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
      n_max = length(utils::count.fields(file_in)) - 6, # drop last six rows
    ))

  bom_stations[, start := as.integer(start)]
  bom_stations[, end := as.integer(end)]
  bom_stations[is.na(end), end := as.integer(format(Sys.Date(), "%Y"))]


  # if ASGS.foyer is installed, correct the state column, otherwise skip
  if (requireNamespace("ASGS.foyer", quietly = TRUE)) {
    message(
      "The package 'ASGS.foyer' is installed. Station locations will\n",
      "be checked against lat/lon location values and corrected if necessary."
    )
    data.table::setDT(bom_stations)
    latlon2state <- function(lat, lon) {
      ASGS.foyer::latlon2SA(lat,
                            lon,
                            to = "STE",
                            yr = "2016",
                            return = "v")
    }

    bom_stations %>%
      .[lon > -50, state_from_latlon := latlon2state(lat, lon)] %>%
      .[state_from_latlon == "New South Wales", actual_state := "NSW"] %>%
      .[state_from_latlon == "Victoria", actual_state := "VIC"] %>%
      .[state_from_latlon == "Queensland", actual_state := "QLD"] %>%
      .[state_from_latlon == "South Australia", actual_state := "SA"] %>%
      .[state_from_latlon == "Western Australia", actual_state := "WA"] %>%
      .[state_from_latlon == "Tasmania", actual_state := "TAS"] %>%
      .[state_from_latlon == "Australian Capital Territory",
        actual_state := "ACT"] %>%
      .[state_from_latlon == "Northern Territory", actual_state := "NT"] %>%
      .[actual_state != state &
          state %notin% c("ANT", "ISL"), state := actual_state] %>%
      .[, actual_state := NULL]
  }
  return(bom_stations)
}
