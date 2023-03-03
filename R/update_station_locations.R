
#' Update internal databases with latest BOM station metadata
#'
#' Download the latest station locations and metadata and update internal
#' databases that support the use of \code{\link{get_ag_bulletin}}.  There is no
#' need to use this unless you know that a station exists in \acronym{BOM}'s
#' database that is not available in the databases distributed with
#' \pkg{wrapique}. In fact, for reproducibility purposes, users are discouraged
#' from using this function.  Ported from \pkg{bomrang}.
#'
#' If \CRANpkg{ASGS.foyer} is installed locally, this function will
#' automatically check and correct any invalid state values for stations located
#' in Australia.  If \CRANpkg{ASGS.foyer} is not installed, the function will
#' update the internal database without validating the state values for stations
#' by reported longitude/latitude location.
#'
#' @examplesIf interactive()
#'
#' update_station_locations()
#'
#' @return Updated internal databases of \acronym{BOM} station locations and
#' \acronym{JSON} \acronym{URL}s
#'
#' @references
#' Station location and other metadata are sourced from the Australian Bureau of
#' Meteorology (\acronym{BOM}) webpage, Bureau of Meteorology Site Numbers:\cr
#' \url{http://www.bom.gov.au/climate/cdo/about/site-num.shtml}.
#'
#' @family bomrang-ported
#' @author Adam H. Sparks, \email{adam.sparks@@dpird.wa.gov.au}
#' @export update_station_locations

update_station_locations <- function() {
  message(
    "This will overwrite the current internal databases of BOM stations.\n",
    "If reproducibility is necessary, you may not wish to proceed.\n",
    "Do you understand and wish to proceed (Y/n)?\n"
  )

  answer <-
    readLines(con = getOption("wrapique.connection"), n = 1)

  answer <- toupper(answer)

  if (answer %notin% c("Y", "YES")) {
    stop("Station databases were not updated.",
         call. = FALSE)
  }

  message("Updating internal station databases.\n")

  # CRAN NOTE avoidance
  site <- state_code <- wmo <- state <- lon <- lat <- # nocov start
    actual_state <- state_from_latlon <- start <- end <- NULL # nocov end

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


  bom_stations_raw <-
    readr::read_fwf(
      file = file.path(tempdir(), "stations.txt"),
      col_positions = readr::fwf_empty(
        file = file.path(tempdir(), "stations.txt"),
        skip = 4,
        n = 1000,
        col_names = c(
          "site",
          "dist",
          "name",
          "start",
          "end",
          "lat",
          "lon",
          "NULL1",
          "state",
          "elev",
          "bar_ht",
          "wmo"
        )
      ),
      skip = 4,
      col_types = c(
        site = "character",
        dist = "character",
        name = "character",
        start = readr::col_integer(),
        end = readr::col_integer(),
        lat = readr::col_double(),
        lon = readr::col_double(),
        NULL1 = "character",
        state = "character",
        elev = readr::col_double(),
        bar_ht = readr::col_double(),
        wmo = readr::col_integer()
      )
    )

  bom_stations_raw[bom_stations_raw == "...."] <- NA
  bom_stations_raw[bom_stations_raw == ".."] <- NA

  # remove extra columns for source of location
  bom_stations_raw <- bom_stations_raw[, -8]

  # add current year to stations that are still active
  bom_stations_raw$end <- as.numeric(bom_stations_raw$end)

  bom_stations_raw["end"][is.na(bom_stations_raw["end"])] <-
    as.integer(format(Sys.Date(), "%Y"))

  data.table::setDT(bom_stations_raw)

  # if sf is installed, correct the state column, otherwise skip
  if (requireNamespace("ASGS.foyer", quietly = TRUE)) {
    message(
      "The package 'ASGS.foyer' is installed. Station locations will\n",
      "be checked against lat/lon location values and corrected in the\n",
      "updated internal database lists of stations."
    )

    latlon2state <- function(lat, lon) {
      ASGS.foyer::latlon2SA(lat,
                            lon,
                            to = "STE",
                            yr = "2016",
                            return = "v")
    }

    bom_stations_raw[lon > -50, state_from_latlon := latlon2state(lat, lon)]
    bom_stations_raw[state_from_latlon == "New South Wales",
                     actual_state := "NSW"]
    bom_stations_raw[state_from_latlon == "Victoria", actual_state := "VIC"]
    bom_stations_raw[state_from_latlon == "Queensland", actual_state := "QLD"]
    bom_stations_raw[state_from_latlon == "South Australia",
                     actual_state := "SA"]
    bom_stations_raw[state_from_latlon == "Western Australia",
                     actual_state := "WA"]
    bom_stations_raw[state_from_latlon == "Tasmania", actual_state := "TAS"]
    bom_stations_raw[state_from_latlon == "Australian Capital Territory",
                     actual_state := "ACT"]
    bom_stations_raw[state_from_latlon == "Northern Territory",
                     actual_state := "NT"]
    bom_stations_raw[actual_state != state &
                       state %notin% c("ANT", "ISL"), state := actual_state]
    bom_stations_raw[, actual_state := NULL]

    data.table::setDF(bom_stations_raw)
  }

  message("Overwriting existing databases")

  stations_site_list[, site := gsub("^0{1,2}", "", stations_site_list$site)]

  fname <-
    system.file("extdata", "stations_site_list.rda", package = "wrapique")

  save(stations_site_list, file = fname, compress = "bzip2")
}
