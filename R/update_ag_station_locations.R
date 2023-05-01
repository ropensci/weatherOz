
#' Update internal databases with latest BOM ag bulletin location data
#'
#' Download the latest ag bulletin station locations and metadata and update
#' the internal database that supports the use of [get_ag_bulletin()].
#' There is no need to use this unless you know that a station exists in
#' \acronym{BOM}'s database that is not available in the databases distributed
#' with \pkg{weatherOz}. In fact, for reproducibility purposes. In fact, users
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
#' <http://www.bom.gov.au/climate/cdo/about/site-num.shtml>.
#'
#' @family bomrang-ported
#' @author Adam H. Sparks, \email{adam.sparks@@dpird.wa.gov.au}
#' @export
#' @return `NULL`, called for it's side-effects writing an updated file to local
#' disk for the package to use.

update_ag_station_locations <- function() {
  message(
    "This will overwrite the current internal databases of BOM stations.\n",
    "If reproducibility is necessary, you may not wish to proceed.\n",
    "Do you understand and wish to proceed (Y/n)?\n"
  )

  answer <-
    readLines(con = getOption("weatherOz.connection"), n = 1)

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



  bom_stations <- fetch_bom_stn_sitelist()

  # remove extra columns for source of location
  stations_site_list <- stations_site_list[, -8]

  # add current year to stations that are still active
  stations_site_list$end <- as.numeric(stations_site_list$end)

  stations_site_list["end"][is.na(stations_site_list["end"])] <-
    as.integer(format(Sys.Date(), "%Y"))

  data.table::setDT(stations_site_list)

  message("Overwriting existing databases")

  stations_site_list[, site := gsub("^0{1,2}", "", stations_site_list$site)]

  fname <-
    system.file("extdata", "stations_site_list.rda", package = "weatherOz")

  save(stations_site_list, file = fname, compress = "bzip2")
  return(invisible(NULL))
}
