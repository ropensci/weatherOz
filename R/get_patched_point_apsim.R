
#' Get PatchedPoint Weather Data in the APSIM Format From SILO
#'
#' Fetch \acronym{APSIM} .met file formatted weather data from the
#'   \acronym{SILO} \acronym{API} derived from the \acronym{BOM} station
#'   observations (PatchedPoint) data.
#'
#' @details The \acronym{SILO} documentation provides the following information
#'   for the PatchedPoint data.
#'
#'   *These data are a continuous daily time series of data at either recording
#'   stations or grid points across Australia:*
#'
#'   * *Data at station locations consists of observational records which have
#'   been supplemented by interpolated estimates when observed data are missing.
#'   Datasets are available at approximately 8,000 Bureau of Meteorology
#'   recording stations around Australia.*
#'
#'   * *Data at grid points consists entirely of interpolated estimates. The
#'   data are taken from the SILO gridded datasets and are available at any
#'   pixel on a 0.05° × 0.05° grid over the land area of Australia (including
#'   some islands).*
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
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au}, and Adam
#'   Sparks, \email{adamhsparks@@gmail.com}
#'
#' @examples
#' \dontrun{
#' # requires an API key as your email address
#' # Source observation data for Wongan Hills station, WA (008137)
#' wd <- get_patched_point_apsim(
#'   station_code = "008137",
#'   start_date = "20220101",
#'   end_date = "20221231",
#'   api_key = "your_api_key"
#' )
#' }
#' @encoding UTF-8
#'
#' @family SILO
#' @family APSIM
#' @family data fetching
#'
#' @autoglobal
#' @export

get_patched_point_apsim <- function(station_code,
                                    start_date,
                                    end_date = Sys.Date(),
                                    api_key = get_key(service = "SILO")) {

  # simplify using the metadata to fetch weather data by converting factors to
  # numeric values
  if (inherits(x = station_code, what = "factor")) {
    station_code <- as.character(station_code)
  }

  if (missing(station_code) | !is.character(station_code)) {
    stop(call. = FALSE,
         "Please supply a valid `station_code`.")
  }

  if (missing(start_date)) {
    stop(call. = FALSE,
         "Please supply a valid start date as `start_date`.")
  }

  .check_not_example_api_key(api_key)
  .is_valid_email_silo_api_key(api_key)

  # validate user provided dates
  start_date <- .check_date(start_date)
  end_date <- .check_date(end_date)
  .check_date_order(start_date, end_date)
  .check_earliest_available_silo(start_date)

  # reformat date for sending to SILO
  start_date <- gsub("-", "", start_date)
  end_date <- gsub("-", "", end_date)

  .query_silo_api(
    .station_code = station_code,
    .start_date = start_date,
    .end_date = end_date,
    .format = "apsim",
    .api_key = api_key,
    .dataset = "PatchedPoint"
  )
}
