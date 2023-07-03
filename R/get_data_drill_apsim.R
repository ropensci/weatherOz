
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
#' @param longitude A single `numeric` value  representing the longitude of the
#'    point-of-interest.
#' @param latitude A single `numeric` value representing the latitude of the
#'   point-of-interest.
#' @param start_date A `character` string or `Date` object representing the
#'   beginning of the range to query in the format \dQuote{yyyy-mm-dd}
#'   (ISO8601).  Data returned is inclusive of this range.
#' @param end_date A `character` string or `Date` object representing the end of
#'   the range query in the format  \dQuote{yyyy-mm-dd} (ISO8601).  Data
#'   returned is inclusive of this range.  Defaults to the current system date.
#' @param api_key A `character `string specifying a valid email address to use
#'   for the request.  The query will return an error if a valid email address
#'   is not provided.
#'
#' @section Included Values:
#'
#' \describe{
#'  \item{rain (mm)}{Rainfall}
#'  \item{maxt (˚C)}{Maximum temperature}
#'  \item{mint (˚C)}{Minimum temperature}
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
#'   start_date = "20221001",
#'   end_date = "20221201",
#'   api_key = "your@email"
#' )
#' }
#'
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au}, and Adam
#'   Sparks, \email{adam.sparks@@dpird.wa.gov.au}
#'
#' @family SILO
#' @family data fetching
#'
#' @export

get_data_drill_apsim <- function(longitude,
                                 latitude,
                                 start_date,
                                 end_date = Sys.Date(),
                                 api_key) {
  if (missing(longitude) || missing(latitude)) {
    stop(call. = FALSE,
         "Please supply a valid values for `longitude` and `latitude`.")
  }

  if (missing(start_date)) {
    stop(call. = FALSE,
         "Please supply a valid start date as `start_date`.")
  }

  # Error if api_key is not provided
  if (missing(api_key)) {
    stop(
      "A valid email address must be provided for `api_key`.",
      call. = FALSE
    )
  }

  # validate user-provided lon and lat values
  .check_lonlat(longitude = longitude, latitude = latitude)

  # validate user provided dates
  start_date <- .check_date(start_date)
  end_date <- .check_date(end_date)
  .check_date_order(start_date, end_date)

  # reformat date for sending to SILO
  start_date <- gsub("-", "", start_date)
  end_date <- gsub("-", "", end_date)


  .query_silo_api(
    .longitude = longitude,
    .latitude = latitude,
    .start_date = start_date,
    .end_date = end_date,
    .format = "apsim",
    .api_key = api_key,
    .dataset = "DataDrill"
  )
}
