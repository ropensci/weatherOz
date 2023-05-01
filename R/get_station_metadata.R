
#' Get latest BOM station metadata
#'
#' Download the latest station locations and metadata for stations in the
#'  \acronym{SILO} data set.
#'
#' If \CRANpkg{ASGS.foyer} is installed locally, this function will
#' automatically check and correct any invalid state values for stations located
#' in Australia.
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
#' \url{http://www.bom.gov.au/climate/cdo/about/site-num.shtml}
#'
#' @family SILO
#'
#' @author Adam H. Sparks, \email{adam.sparks@@dpird.wa.gov.au}
#' @export

get_station_metadata <- function() {

  bom_stations <- fetch_bom_stn_sitelist()

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

  silo_stations <-
    find_nearby_stations(
      latitude = -25.5833,
      longitude = 134.5667,
      distance_km = 10000,
      which_api = "silo"
    )

  return(bom_stations[station_name %in% silo_stations$station_name])
}
