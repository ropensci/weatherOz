
#' Get Weather Station Metadata for Both DPIRD and SILO Weather Stations
#'
#' Download the latest station locations and metadata for stations in the
#'   \acronym{SILO} and \acronym{DPIRD} networks.  For \acronym{BOM} stations
#'   that exist in \acronym{SILO}, but lack metadata from \acronym{BOM}, the
#'   rows will exist to indicate that the station is in the \acronym{SILO} data
#'   set, but there is no corresponding \acronym{BOM} metadata available.
#'
#' @param which_api A `string` value that indicates which \acronym{API} to use.
#'   Valid values are `all`, for both \acronym{SILO} (\acronym{BOM} data) and
#'   \acronym{DPIRD} \acronym{API}s; `silo` for only stations from the
#'   \acronym{SILO} \acronym{API} (\acronym{BOM} data); or `dpird` for stations
#'   from the \acronym{DPIRD} Weather 2.0 \acronym{API}.  Defaults to `all`.
#' @param api_key A `character` string containing your \acronym{API} key from
#'   \acronym{DPIRD}, <https://www.agric.wa.gov.au/web-apis>, for the
#'   \acronym{DPIRD} Weather 2.0 \acronym{API}.
#' @param status A `Boolean` string indicating whether to include closed
#'   stations' metadata.  Use `TRUE` to include these.  Defaults to `FALSE`.
#' @param rich A `Boolean` string indicating whether to return rich information
#'   about \acronym{DPIRD}'s weather station(s), this does not affect the
#'   \acronym{SILO} stations' metadata, the variables for these observations
#'   will be `NA`.  Defaults to `FALSE`.
#'
#' @note For stations in the \acronym{SILO} \acronym{API}, \acronym{BOM} does
#'   not report the exact date on which stations opened or closed, only the
#'   year.  Therefore the `start` and `end` columns will indicate January 1 of
#'   the year that a station opened or closed, whereas stations in the
#'   \acronym{DPIRD} network have the date to the day.  For \acronym{BOM}
#'   stations that are closed for the current year, this indicates that the
#'   station closed sometime during the current year prior to the request being
#'   made. `NA` in the current year indicates a station is still open.
#'
#' @note There are discrepancies between the \acronym{BOM}'s official station
#'   metadata, *e.g.* longitude and latitude values and \acronym{SILO} metadata.
#'   In these cases, the \acronym{BOM} metadata is used as it is considered to
#'   be the authority on the stations' locations.
#'
#' @note The station names are returned by both \acronym{API}s in full caps.
#'   For purposes of cleaner graphs and maps where these data may be sued, this
#'   function converts them to proper name formats/title case with the first
#'   letter of every word capitalised excepting words like \dQuote{at} or
#'   \dQutoe{on} and keeps acronyms like \dQuote{AWS} or \dQuote{PIRSA} or state
#'   abbreviations in the station names as all caps.
#'
#' @examples
#' \dontrun{
#' # fetch metadata for all stations available in {weatherOz}
#' get_station_metadata(api_key = "your_api_key")
#' }
#'
#' @return a [data.table::data.table] of \acronym{BOM} weather stations'
#'   metadata for stations available from \acronym{SILO} and weather stations'
#'   metadata for stations available from \acronym{DPIRD}'s Weather 2.0
#'   \acronym{API} with the following columns sorted by `state` and
#'   `station_name`.
#'   \tabular{rl}{
#'     **station_code**:\tab Unique station code. `factor`\cr
#'     **station_name**:\tab Unique station name. `character`\cr
#'     **start**:\tab Date observations start. `date`\cr
#'     **end**:\tab Date observations end. `date`\cr
#'     **latitude**:\tab Latitude in decimal degrees. `numeric`\cr
#'     **longitude**:\tab Longitude in decimal degrees. `numeric`\cr
#'     **state**:\tab State in which the station is located. `character`\cr
#'     **elev_m**:\tab Station elevation in metres. `numeric`\cr
#'     **source**:\tab Organisation responsible for the data or station
#'       maintenance. `character`\cr
#'     **status**:\tab Station status, one of 'open' or 'closed'. `character`\cr
#'     **wmo**:\tab World Meteorological Organisation, (\acronym{WMO}), number
#'       if applicable. `numeric`\cr
#'     **`rich` values**\tab\cr
#'     **capabilities**:\tab a list of the station's capabilities (data that it
#'       records). `character`\cr
#'     **probe_height**:\tab temperature probe height in metres. `double`\cr
#'     **rain_gauge_height**\tab rain gauge height in metres. `double`\cr
#'     **wind_probe_heights**:\tab wind probe heights always 3 metres, although
#'       some have 10 metre probes. `integer`\cr
#'   }
#'
#' @references
#' Station location and other metadata are sourced from the Australian Bureau of
#'   Meteorology (\acronym{BOM}) webpage, Bureau of Meteorology Site Numbers:\cr
#'   <http://www.bom.gov.au/climate/cdo/about/site-num.shtml> and
#'   <http://www.bom.gov.au/climate/data/lists_by_element/stations.txt> and the
#'   \acronym{DPIRD} Weather 2.0 \acronym{API}.
#'
#' @author Adam H. Sparks, \email{adam.sparks@@dpird.wa.gov.au}
#'
#' @family DPIRD
#' @family SILO
#' @family metadata
#'
#' @export

get_station_metadata <-
  function(which_api = "all",
           api_key,
           status = FALSE,
           rich = FALSE) {

    which_api <- .check_which_api(which_api)

    if (which_api == "silo") {
      out <- .fetch_silo_metadata()
    } else if (which_api == "dpird") {
      if (missing(api_key)) {
        stop(call. = FALSE,
             "You must provide an API key for this query.")
      }
      out <- .fetch_dpird_metadata(.api_key = api_key, .rich = rich)
    } else if (which_api == "all") {
      if (missing(api_key)) {
        stop(call. = FALSE,
             "You must provide an API key for this query.")
      }
      silo <- .fetch_silo_metadata()
      dpird <- .fetch_dpird_metadata(.api_key = api_key, .rich = rich)
      out <- data.table::rbindlist(list(silo, dpird), fill = TRUE)
    }

    out[, start := data.table::fifelse(is.na(start),
                                       as.character(lubridate::year(Sys.Date())),
                                       as.character(start))]
    out[, start := data.table::fifelse(nchar(start) == 4,
                                       paste(start, "01", "01", sep = "-"),
                                       start)]
    out[, start := lubridate::ymd(start)]

    out[, end := data.table::fifelse(is.na(end),
                                     as.character(Sys.Date()),
                                     as.character(end))]
    out[, end := data.table::fifelse(nchar(end) == 4,
                                     paste(end, "01", "01", sep = "-"),
                                     end)]
    out[, end := lubridate::ymd(end)]

    data.table::setkey(out, station_code)

    data.table::setorderv(out, cols = c("state",
                                        "station_name"))
    data.table::setcolorder(out, c(
      "station_code",
      "station_name",
      "start",
      "end",
      "latitude",
      "longitude",
      "state",
      "elev_m",
      "source",
      "status",
      "wmo"
    ))

    # lastly, if user wants all stations return them, else return only open ones
    if (isTRUE(status)) {
      return(out)
    } else {
      return(subset(out, status == "open"))
    }
  }


#' SILO Metadata
#'
#' Returns metadata about stations in the SILO network
#'
#' Fetches metadata directly from BOM and then fetches a station list from SILO
#' to use in matching against BOM metadata. Returns only stations in SILO's
#' network to the user. A bit slow and clunky but SILO doesn't have a proper
#' way to do this.
#'
#' @return A `data.table` of SILO station metadata
#' @keywords Internal
#' @noRd

.fetch_silo_metadata <- function() {
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

  silo_stations <- .query_silo_api(
      .station_code = "015526",
      .radius = 10000,
      .format = "near",
      .dataset = "PatchedPoint"
  )

  station_metadata <- merge(silo_stations, bom_stations, by = c("station_code"))
  # drops the unwanted columns that are added after using `find_nearby_stations`
  station_metadata[, grep(".y", names(station_metadata)) := NULL]
  station_metadata[, owner := NULL]
  station_metadata[, distance_km := NULL]
  data.table::setnames(station_metadata,
                       names(station_metadata),
                       gsub(".x", "", names(station_metadata)))
  return(station_metadata)
}

#' DPIRD Metadata
#'
#' Returns metadata about stations in the DPIRD network
#'
#' Fetches metadata directly from DPIRD's API.
#'
#' @param .api_key the user's API key as provided by them
#' @param .rich `TRUE`/`FALSE` values indicating whether to return rich
#'  metadata about the weather stations
#'
#' @return A `data.table` of DPIRD station metadata
#' @keywords Internal
#' @noRd


.fetch_dpird_metadata <- function(.api_key, .rich) {

  if (isFALSE(.rich)) {
    query_list <- list(
      offset = "0",
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
  } else {
    query_list <- list(
      offset = "0",
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
          "status",
          "capabilities",
          "probeHeight",
          "rainGaugeHeight",
          "windProbeHeights"
        ),
        collapse = ","
      ),
      group = "api",
      api_key = .api_key
    )
  }

  response <- .query_dpird_api(.end_point = NULL,
                               .query_list = query_list,
                               .limit = 300)

  parsed <- jsonlite::fromJSON(response[[1]]$parse("UTF8"))

  if (.rich) {
    dpird_stations <-
      data.table::as.data.table(cbind(parsed$collection[, c(1:10, 12:13)],
                                      parsed$collection$capabilities))
  } else {
    dpird_stations <- data.table::as.data.table(parsed$collection)
  }

  dpird_stations[, wmo := NA]
  dpird_stations[, state := "WA"]
  data.table::setnames(
    dpird_stations,
    old = c("startDate", "endDate", "altitude", "owner"),
    new = c("start", "end", "elev_m", "source")
  )

  .set_snake_case_names(dpird_stations)
  data.table::setcolorder(
    dpird_stations,
    neworder = c(
      "station_code",
      "station_name",
      "start",
      "end",
      "latitude",
      "longitude",
      "state",
      "elev_m",
      "source",
      "status",
      "wmo"
    )
  )
  return(dpird_stations[])
}
