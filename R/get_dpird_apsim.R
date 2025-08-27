
#' Get DPIRD Summary Weather Data in the APSIM Format From the Weather 2.0 API
#'
#' Automates the retrieval and conversion of summary data from the
#'   \acronym{DPIRD} Weather 2.0 \acronym{API} to an \acronym{APSIM} .met file
#'   formatted weather data object.
#'
#' @param station_code A `character` string or `factor` from
#'   [get_stations_metadata()] of the \acronym{BOM} station code for the station
#'   of interest.
#' @param start_date A `character` string or `Date` object representing the
#'   beginning of the range to query in the format \dQuote{yyyy-mm-dd}
#'   (ISO8601).  Data returned is inclusive of this date.
#' @param end_date A `character` string or `Date` object representing the end of
#'   the range query in the format  \dQuote{yyyy-mm-dd} (ISO8601).  Data
#'   returned is inclusive of this date.  Defaults to the current system date.
#' @param api_key A `character` string containing your \acronym{API} key from
#'   \acronym{DPIRD}, <https://www.dpird.wa.gov.au/online-tools/apis/>, for the
#'   \acronym{DPIRD} Weather 2.0 \acronym{API}.  Defaults to automatically
#'   detecting your key from your local .Renviron, .Rprofile or similar.
#'   Alternatively, you may directly provide your key as a string here.  If
#'   nothing is provided, you will be prompted on how to set up your \R session
#'   so that it is auto-detected.
#'
#' @section Saving objects:
#' To save \dQuote{met} objects the [apsimx::write_apsim_met()] is reexported.
#'   Note that when saving, comments from SILO will be included, but these will
#'   not be printed as a part of the resulting `met` object in your \R session.
#'
#' @examples
#' \dontrun{
#' # Get an APSIM format object for Binnu
#' # Note that you need to supply your own API key
#'
#' wd <- get_dpird_apsim(
#'   station_code = "BI",
#'   start_date = "20220101",
#'   end_date = "20221231",
#'   api_key = "your_api_key"
#' )
#' }
#'
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#'
#' @return An \CRANpkg{apsimx} object of class \sQuote{met} with attributes.
#'
#' @family DPIRD
#' @family data fetching
#' @family APSIM
#' @encoding UTF-8
#' @autoglobal
#' @export

get_dpird_apsim <- function(station_code,
                            start_date,
                            end_date = Sys.Date(),
                            api_key = get_key(service = "DPIRD")) {

  apsim <- get_dpird_summaries(
    station_code = station_code,
    start_date = start_date,
    end_date = end_date,
    interval = "daily",
    values = c(
      "airTemperatureMax",
      "airTemperatureMin",
      "panEvaporation",
      "rainfall",
      "relativeHumidityAvg",
      "solarExposure",
      "windAvgSpeed"
    ),
    api_key = api_key
  )

  site <- apsim$station_name[1]
  latitude <- apsim$latitude[1]
  longitude <- apsim$longitude[1]
  apsim[, day := NULL]
  apsim[, day := lubridate::yday(apsim$date)]
  apsim <-
    apsim[, c(
      "year",
      "day",
      "radiation",
      "air_tmax",
      "air_tmin",
      "pan_evaporation",
      "rainfall",
      "rh_avg",
      "wind_avg"
    )]

  data.table::setnames(
    apsim,
    old = c(
      "radiation",
      "air_tmax",
      "air_tmin",
      "pan_evaporation",
      "rainfall",
      "rh_avg",
      "wind_avg"
    ),
    new = c("radn", "maxt", "mint", "rain", "evap", "rh", "windspeed")
  )

  apsim <- apsimx::as_apsim_met(
    filename = "weather.met.met",
    x = apsim,
    site = site,
    latitude = latitude,
    longitude = longitude,
    colnames = names(apsim),
    units =  c("()",
               "()",
               "(MJ/m2/day)",
               "(oC)",
               "(oC)",
               "(mm)",
               "(mm)",
               "(%)",
               "(m/s)"),
    comments = sprintf("!data from DPIRD Weather 2.0 API. retrieved: %s",
                       Sys.time())
  )

  return(apsim)
}
