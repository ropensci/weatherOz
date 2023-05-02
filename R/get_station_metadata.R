
#' Get latest BOM and DPIRD weather station network station metadata
#'
#' Download the latest station locations and metadata for stations in the
#'  \acronym{SILO} and \acronym{DPIRD} data sets.
#'
#' If \CRANpkg{ASGS.foyer} is installed locally, this function will
#' automatically check and correct any invalid state values for stations located
#' in Australia in the \acronym{SILO} data.
#'
#' @param check_location `Boolean`. An optional check to use
#'  \CRANpkg{ASGS.foyer} to double check the station's physical locations and
#'  correct any errors in the state where the station is located.
#'  \CRANpkg{ASGS.foyer} must be installed to use this.
#' @param api_key A `character` string containing your \acronym{API} key from
#'  \acronym{DPIRD}, <https://www.agric.wa.gov.au/web-apis>, for the
#'  \acronym{DPIRD} weather \acronym{API}.
#' @param which_api A `string` value that indicates which API to use.  Defaults
#'  to "silo". Valid values are "all", for both \acronym{SILO} (\acronym{BOM})
#'  and \acronym{DPIRD} weather station networks; "silo" for only stations in
#'  the \acronym{SILO} network; or "dpird" for stations in the \acronym{DPIRD}
#'  network.
#'
#' @examples
#' \dontrun{
#' get_station_metadata()
#' }
#' @return a `data.table` of \acronym{BOM} weather stations' metadata for
#'  stations available from \acronym{SILO}.
#'
#' @references
#' Station location and other metadata are sourced from the Australian Bureau of
#' Meteorology (\acronym{BOM}) webpage, Bureau of Meteorology Site Numbers:\cr
#' <http://www.bom.gov.au/climate/cdo/about/site-num.shtml> and
#' <http://www.bom.gov.au/climate/data/lists_by_element/stations.txt>
#'
#' @family SILO
#'
#' @author Adam H. Sparks, \email{adam.sparks@@dpird.wa.gov.au}
#' @export

get_station_metadata <-
  function(check_location = FALSE,
           api_key = NULL,
           which_api = "silo") {

    which_api <- .check_which_api()

    if (which_api == "silo") {
      silo <- .fetch_silo_metadata(.check_location = check_location)
    } else if (which_api == "dpird") {
      dpird <- .fetch_dpird_metadata(.api_key = api_key)
    } else if (which_api == "all") {
      silo <- .fetch_silo_metadata(.check_location = check_location)
      dpird <- .fetch_dpird_metadata(.api_key = api_key)
    }

    out <- #Merge dpird and silo dfs here
      return(out)
  }

.fetch_silo_metadata <- function(.check_location = check_location) {
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
        # drop last six rows
        n_max = length(utils::count.fields(file_in)) - 6
      )
    )

  bom_stations[, station_code := as.factor(station_code)]
  bom_stations[, station_name := DescTools::StrCap(x = tolower(station_name),
                                                   method = "word")]
  bom_stations[, start := as.integer(start)]
  bom_stations[, end := as.integer(end)]
  bom_stations[, status := ifelse(!is.na(end), "Closed", "Open")]
  bom_stations[is.na(end), end := as.integer(format(Sys.Date(), "%Y"))]

  # if ASGS.foyer is installed, correct the state column, otherwise skip
  if (requireNamespace("ASGS.foyer", quietly = TRUE)) {
    if (isTRUE(.check_location)) {
      message(
        "The package {ASGS.foyer} is installed. Station locations will\n",
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

      bom_stations[lon > -50, state_from_latlon := latlon2state(lat, lon)]
      bom_stations[state_from_latlon == "New South Wales", actual_state := "NSW"]
      bom_stations[state_from_latlon == "Victoria", actual_state := "VIC"]
      bom_stations[state_from_latlon == "Queensland", actual_state := "QLD"]
      bom_stations[state_from_latlon == "South Australia", actual_state := "SA"]
      bom_stations[state_from_latlon == "Western Australia", actual_state := "WA"]
      bom_stations[state_from_latlon == "Tasmania", actual_state := "TAS"]
      bom_stations[state_from_latlon == "Australian Capital Territory",
                   actual_state := "ACT"]
      bom_stations[state_from_latlon == "Northern Territory",
                   actual_state := "NT"]
      bom_stations[actual_state != state &
                     state %notin% c("ANT", "ISL"), state := actual_state]
      bom_stations[, actual_state := NULL, dist := NULL]
    }
  }

  silo_stations <-
    find_nearby_stations(
      latitude = -25.5833,
      longitude = 134.5667,
      distance_km = 10000,
      which_api = "silo"
    )

  return(bom_stations[station_name %in% silo_stations$station_name])
}


.fetch_dpird_metadata <- function(.api_key = api_key) {
  base_url = "https://api.dpird.wa.gov.au/v2/weather/stations/"

  query_list <- list(
    station_code = "SP%2CAN001",
    offset = "0",
    limit = 300,
    includeClosed = "true",
    select = paste0(
      list(
        "altitude",
        "startDate",
        "endDate",
        "stationCode",
        "stationName",
        "latitude",
        "longitude",
        "owner",
        "status"
      ),
      collapse = ","
    ),
    group = "api",
    api_key = .api_key
  )

  client <- crul::HttpClient$new(url = base_url)

  # nocov begin
  response <- client$get(query = query_list,
                         retry = 6L,
                         timeout = 30L)

  # check to see if request failed or succeeded
  # - a custom approach this time combining status code,
  #   explanation of the code, and message from the server
  if (response$status_code > 201) {
    mssg <- jsonlite::fromJSON(response$parse("UTF-8"))$message
    x <- response$status_http()
    stop(sprintf("HTTP (%s) - %s\n  %s", x$status_code, x$explanation, mssg),
         call. = FALSE)
  }

  response$raise_for_status()
  # create meta object
  r <- jsonlite::fromJSON(response$parse("UTF8"))
  r <- data.table::data.table(r$collection)

  data.table::setnames(
    r,
    old = c(
      "stationCode",
      "stationName",
      "latitude",
      "longitude",
      "altitude",
      "owner",
      "startDate",
      "endDate",
      "status"
    ),
    new = c(
      "station_code",
      "station_name",
      "lat",
      "lon",
      "elev.m",
      "owner",
      "start",
      "end"
    )
  )

  return(r)
}
