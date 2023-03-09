
#' Update internal databases with latest BOM ag bulletin location data
#'
#' Download the latest ag bulletin station locations and metadata and update
#' the internal database that supports the use of \code{\link{get_ag_bulletin}}.
#' There is no need to use this unless you know that a station exists in
#' \acronym{BOM}'s database that is not available in the databases distributed
#' with \pkg{wrapique}. In fact, for reproducibility purposes. In fact, users
#' are strongly discouraged from using this function.  Ported from
#' \pkg{bomrang}.
#'
#' If \CRANpkg{ASGS.foyer} is installed locally, this function will
#' automatically check and correct any invalid state values for stations located
#' in Australia.  If \CRANpkg{ASGS.foyer} is not installed, the function will
#' update the internal database without validating the state values for stations
#' by reported longitude/latitude location.
#'
#' @examplesIf interactive()
#'
#' update_station_ag_locations()
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
#' @return `NULL`, called for it's side-effects writing an updated file to local
#' disk for the package to use.

update_ag_station_locations <- function() {
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
    actual_state <- state_from_latlon <- start <- end <-
    stations_site_list <- NULL # nocov end

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

  bom_stations_lines <- readr::read_lines(file.path(tempdir(), "stations.zip"))
  keep <- length(bom_stations_lines) - 7
  bom_stations_lines <- bom_stations_lines[1:keep]
  readr::write_lines(x = bom_stations_lines,
                     file = file.path(tempdir(), "stations.txt"))

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

  message("Overwriting existing databases")

  stations_site_list[, site := gsub("^0{1,2}", "", stations_site_list$site)]

  fname <-
    system.file("extdata", "stations_site_list.rda", package = "wrapique")

  save(stations_site_list, file = fname, compress = "bzip2")
  return(invisible(NULL))
}
