
#' Get DataDrill Weather Data in the APSIM Format From SILO
#'
#' Fetch \acronym{APSIM} .met file formatted weather data from the weather data
#'   from the \acronym{SILO} \acronym{API} of spatially interpolated weather
#'   data (DataDrill).  The daily climate surfaces have been derived either by
#'   splining or kriging the observational data.  The returned values contain
#'   \dQuote{source} columns, which denote how the observations were derived.
#'   The grid spans 112° to 154°, -10° to -44° with resolution 0.05° latitude by
#'   0.05° longitude (approximately 5 km × 5 km).
#'
#' Note that when saving, comments from SILO will be included, but these will
#'   not be printed as a part of the resulting `met` object in your \R session.
#'
#' @param longitude A single `numeric` value  representing the longitude of the
#'    point-of-interest.
#' @param latitude A single `numeric` value representing the latitude of the
#'   point-of-interest.
#' @param start_date A `character` string or `Date` object representing the
#'   beginning of the range to query in the format \dQuote{yyyy-mm-dd}
#'   (ISO8601).  Data returned is inclusive of this date.
#' @param end_date A `character` string or `Date` object representing the end of
#'   the range query in the format  \dQuote{yyyy-mm-dd} (ISO8601).  Data
#'   returned is inclusive of this date.  Defaults to the current system date.
#' @param api_key A `character` string containing your \acronym{API} key,
#'   an e-mail address, for the request.  Defaults to automatically detecting
#'   your key from your local .Renviron, .Rprofile or similar.  Alternatively,
#'   you may directly provide your key as a string here.  If nothing is
#'   provided, you will be prompted on how to set up your \R session so that it
#'   is auto-detected.
#'
#' @section Included Values:
#'
#' \describe{
#'  \item{rain (mm)}{Rainfall}
#'  \item{maxt (degrees C)}{Maximum temperature}
#'  \item{mint (degrees C)}{Minimum temperature}
#'  \item{vp (hPa)}{Vapour pressure}
#'  \item{evap_pan (mm)}{Class A pan evaporation}
#'  \item{radiation (Mj/\ifelse{html}{\out{m<sup>1</sup>}}{m\eqn{^1}})}{Solar
#'    exposure, consisting of both direct and diffuse components}
#' }
#'
#' @section Value information:
#'
#' Solar radiation: total incoming downward shortwave radiation on a horizontal
#'   surface, derived from estimates of cloud oktas and sunshine
#'   \ifelse{html}{\out{duration<sup>2</sup>}}{duration\eqn{^2}}.
#'
#' Evaporation and evapotranspiration: an overview of the variables provided by
#'   \acronym{SILO} is available here,
#'   <https://data.longpaddock.qld.gov.au/static/publications/Evapotranspiration_overview.pdf>.
#'
#' @section Data codes:
#' Where the source code is a 6 digit string comprising the source code for the
#'   6 variables. The single digit code for each variable is:
#'
#'   \describe{
#'    \item{0}{an actual observation;}
#'    \item{1}{an actual observation from a composite station;}
#'    \item{2}{a value interpolated from daily observations;}
#'    \item{3}{a value interpolated from daily observations using the anomaly
#'      interpolation method for \acronym{CLIMARC} data;}
#'    \item{6}{a synthetic pan value; or}
#'    \item{7}{an interpolated long term average.}
#'   }
#'
#' @section Saving objects:
#' To save \dQuote{met} objects the [apsimx::write_apsim_met()] is reexported.
#'   Note that when saving, comments from SILO will be included, but these will
#'   not be printed as a part of the resulting `met` object in your \R session.
#'
#' @return An \CRANpkg{apsimx} object of class \sQuote{met} with attributes.
#'
#' @references
#' 1. Rayner, D. (2005). Australian synthetic daily Class A pan evaporation.
#'   Technical Report December 2005, Queensland Department of Natural Resources
#'   and Mines, Indooroopilly, Qld., Australia, 40 pp.
#'
#' 2. Morton, F. I. (1983). Operational estimates of areal evapotranspiration
#'   and their significance to the science and practice of hydrology, *Journal
#'   of Hydrology*, Volume 66, 1-76.
#'
#' @examples
#' \dontrun{
#' # requires an API key as your email address
#' # Source data from latitude and longitude coordinates (gridded data) for
#' # max and minimum temperature and rainfall for Southwood, QLD.
#' wd <- get_data_drill_apsim(
#'   latitude = -27.85,
#'   longitude = 150.05,
#'   start_date = "20220101",
#'   end_date = "20221231",
#'   api_key = "your_api_key"
#' )
#' }
#'
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au}, and Adam
#'   Sparks, \email{adamhsparks@@gmail.com}
#'
#' @family SILO
#' @family data fetching
#' @family APSIM
#' @encoding UTF-8
#' @autoglobal
#' @export

get_data_drill_apsim <- function(longitude,
                                 latitude,
                                 start_date,
                                 end_date = Sys.Date(),
                                 api_key = get_key(service = "SILO")) {
  if (missing(longitude) || missing(latitude)) {
    stop(call. = FALSE,
         "Please supply a valid values for `longitude` and `latitude`.")
  }

  if (missing(start_date)) {
    stop(call. = FALSE,
         "Please supply a valid start date as `start_date`.")
  }

  .check_not_example_api_key(api_key)
  .is_valid_email_silo_api_key(api_key)

  # validate user-provided lon and lat values
  lonlat <- .check_lonlat(longitude = longitude, latitude = latitude)

  user_longitude <- lonlat["longitude"]
  user_latitude <- lonlat["latitude"]

  # validate user provided dates
  start_date <- .check_date(start_date)
  end_date <- .check_date(end_date)
  .check_date_order(start_date, end_date)
  .check_earliest_available_silo(start_date)

  # reformat date for sending to SILO
  start_date <- gsub("-", "", start_date)
  end_date <- gsub("-", "", end_date)

  return(
    .query_silo_api(
      .longitude = longitude,
      .latitude = latitude,
      .start_date = start_date,
      .end_date = end_date,
      .format = "apsim",
      .api_key = api_key,
      .dataset = "DataDrill"
    )
  )
}
