

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
#'  @note \acronym{BOM} does not report the exact date on which stations opened
#'   or closed, only the year. Therefore the 'start' and 'end' columns will
#'   indicate January 1 of the year that a station opened or closed for
#'   \acronym{BOM} stations whereas stations in the \acronym{DPIRD} network
#'   have the date to the day. For \acronym{BOM} stations that are closed
#'   for the current year, this indicates that the station closed sometime
#'   during the current year prior to the request being made.
#'
#' @examplesIf interactive()
#' # fetch SILO metadata
#' get_station_metadata()
#'
#' @return a `data.table` of \acronym{BOM} weather stations' metadata for
#'  stations available from \acronym{SILO} with the following columns.
#'  \tabular{rl}{
#'   **station_code**:\tab Unique station ID. `factor`\cr
#'   **station_name**:\tab Unique station name. `character`\cr
#'   **start**:\tab Date observations start. `date`\cr
#'   **end**:\tab Date observations end. `date`\cr
#'   **lat**:\tab Latitude in decimal degrees. `numeric`\cr
#'   **lon**:\tab Longitude in decimal degrees. `numeric`\cr
#'   **state**:\tab State in which the station is located. `character`\cr
#'   **elev.m**:\tab Station elevation in metres. `numeric`\cr
#'   **source**:\tab Organisation responsible for the data or station
#'    maintenance. `character`\cr
#'   **status**:\tab Station status, one of "open" or "closed". `character`\cr
#'   **wmo**:\tab World Meteorlogical Organisation, acronym{WMO}, number if
#'    applicable. `numeric`\cr
#'   }
#'
#' @references
#' Station location and other metadata are sourced from the Australian Bureau of
#' Meteorology (\acronym{BOM}) webpage, Bureau of Meteorology Site Numbers:\cr
#' <http://www.bom.gov.au/climate/cdo/about/site-num.shtml> and
#' <http://www.bom.gov.au/climate/data/lists_by_element/stations.txt> and
#' \acronym{DPIRD} Weather 2.0 \acronym{API}.
#'
#' @author Adam H. Sparks, \email{adam.sparks@@dpird.wa.gov.au}
#' @export

get_station_metadata <-
  function(check_location = FALSE,
           api_key = NULL,
           which_api = "silo") {
    which_api <- .check_which_api(which_api)

    if (which_api == "silo") {
      out <- .fetch_silo_metadata(.check_location = check_location)
    } else if (which_api == "dpird") {
      out <- .fetch_dpird_metadata(.api_key = api_key)
    } else if (which_api == "all") {
      silo <- .fetch_silo_metadata(.check_location = check_location)
      dpird <- .fetch_dpird_metadata(.api_key = api_key)
      out <- data.table::rbindlist(list(silo, dpird))
    }

    data.table::setkey(out, "station_code")
    data.table::setorderv(out, cols = c("state", "station_name"))
    out[, start := data.table::fifelse(is.na(start),
                                       as.character(lubridate::year(Sys.Date())),
                                       as.character(start))]
    out[, start := data.table::fifelse(nchar(start) == 4,
                                       paste(start, "01", "01", sep = "-"),
                                       start)]
    out[, start := lubridate::ymd(start)]

    out[, end := data.table::fifelse(is.na(end),
                                     as.character(lubridate::year(Sys.Date())),
                                     as.character(end))]
    out[, end := data.table::fifelse(nchar(end) == 4,
                                     paste(end, "01", "01", sep = "-"),
                                     end)]
    out[, end := lubridate::ymd(end)]
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
  bom_stations[, status := ifelse(!is.na(end), "closed", "open")]
  bom_stations[, dist := NULL]
  bom_stations[, source := NULL]
  bom_stations[, bar_height.m := NULL]
  bom_stations[, source := "Bureau of Meteorology"]

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
      bom_stations[, actual_state := NULL]
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
  dpird_stations <- jsonlite::fromJSON(response$parse("UTF8"))
  dpird_stations <- data.table::data.table(dpird_stations$collection)

  data.table::setnames(
    dpird_stations,
    old = c(
      "stationCode",
      "stationName",
      "latitude",
      "longitude",
      "altitude",
      "startDate",
      "endDate",
      "owner"
    ),
    new = c(
      "station_code",
      "station_name",
      "lat",
      "lon",
      "elev.m",
      "start",
      "end",
      "source"
    )
  )

  dpird_stations[, wmo := NA]
  dpird_stations[, state := "WA"]

  data.table::setcolorder(
    dpird_stations,
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
  return(dpird_stations[])
}
