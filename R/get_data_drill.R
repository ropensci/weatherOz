
#' Get DataDrill Weather Data From SILO
#'
#' Fetch nicely formatted weather data from the \acronym{SILO} \acronym{API} of
#'   spatially interpolated weather data (DataDrill).  The daily climate
#'   surfaces have been derived either by splining or kriging the observational
#'   data.  The returned values contain \dQuote{source} columns, which denote
#'   how the observations were derived.  The grid spans 112° to 154°, -10° to
#'   -44° with resolution 0.05° latitude by 0.05° longitude (approximately 5 km
#'   × 5 km).
#'
#' # Column Name Details
#'
#' Column names are converted from the default returns of the API to be
#'    snake_case formatted and where appropriate, the names of the values that
#'    are analogous between \acronym{SILO} and \acronym{DPIRD} data are named
#'    using the same name for ease of interoperability, _e.g._, using
#'    `rbind()` to create a `data.table` that contains data from both APIs.
#'
#' @param longitude A single `numeric` value  representing the longitude of the
#'    point-of-interest to the hundredths (_e.g._, 0.05) of a degree.
#' @param latitude A single `numeric` value representing the latitude of the
#'   point-of-interest to the hundredths (_e.g._., 0.05) of a degree.
#' @param start_date A `character` string or `Date` object representing the
#'   beginning of the range to query in the format \dQuote{yyyy-mm-dd}
#'   (ISO8601).  Data returned is inclusive of this date.
#' @param end_date A `character` string or `Date` object representing the end of
#'   the range query in the format  \dQuote{yyyy-mm-dd} (ISO8601).  Data
#'   returned is inclusive of this date.  Defaults to the current system date.
#' @param values A `character` string with the type of weather data to
#'   return.  See **Available Values** for a full list of valid values.
#'   Defaults to `all` with all available values being returned.
#' @param api_key A `character` string containing your \acronym{API} key,
#'   an e-mail address, for the request.  Defaults to automatically detecting
#'   your key from your local .Renviron, .Rprofile or similar.  Alternatively,
#'   you may directly provide your key as a string here.  If nothing is
#'   provided, you will be prompted on how to set up your \R session so that it
#'   is auto-detected.
#'
#' @section Available Values:
#'
#' \describe{
#'  \item{all}{Which will return all of the following values}
#'  \item{rain (mm)}{Rainfall}
#'  \item{max_temp (degrees C)}{Maximum temperature}
#'  \item{min_temp (degrees C)}{Minimum temperature}
#'  \item{vp (hPa)}{Vapour pressure}
#'  \item{vp_deficit (hPa)}{Vapour pressure deficit}
#'  \item{evap_pan (mm)}{Class A pan evaporation}
#'  \item{evap_syn (mm)}{Synthetic
#'    \ifelse{html}{\out{estimate<sup>1</sup>}}{estimate\eqn{^1}}}
#'  \item{evap_comb (mm)}{Combination (synthetic estimate pre-1970, class A pan
#'    1970 onwards)}
#'  \item{evap_morton_lake (mm)}{Morton's shallow lake evaporation}
#'  \item{radiation (Mj/\ifelse{html}{\out{m<sup>2</sup>}}{m\eqn{^2}})}{Solar
#'    exposure, consisting of both direct and diffuse components}
#'  \item{rh_tmax (%)}{Relative humidity at the time of maximum temperature}
#'  \item{rh_tmin (%)}{Relative humidity at the time of minimum temperature}
#'  \item{et_short_crop (mm)}{
#'     \ifelse{html}{\out{FAO56<sup>4</sup>}}{FAO56\eqn{^4}} short crop}
#'  \item{et_tall_crop (mm)}{
#'     \ifelse{html}{\out{ASCE<sup>5</sup>}}{ASCE\eqn{^5}} tall
#'     \ifelse{html}{\out{crop<sup>6</sup>}}{crop\eqn{^6}}}
#'  \item{et_morton_actual (mm)}{Morton's areal actual evapotranspiration}
#'  \item{et_morton_potential (mm)}{Morton's point potential evapotranspiration}
#'  \item{et_morton_wet (mm)}{Morton's wet-environment areal potential
#'    evapotranspiration over land}
#'  \item{mslp (hPa)}{Mean sea level pressure}
#' }
#'
#' @section Value information:
#'
#' Solar radiation: total incoming downward shortwave radiation on a horizontal
#'   surface, derived from estimates of cloud oktas and sunshine
#'   \ifelse{html}{\out{duration<sup>3</sup>}}{duration\eqn{^3}}.
#'
#' Relative humidity: calculated using the vapour pressure measured at 9am, and
#'   the saturation vapour pressure computed using either the maximum or minimum
#'   \ifelse{html}{\out{temperature<sup>6</sup>}}{temperature\eqn{^6}}.
#'
#' Evaporation and evapotranspiration: an overview of the variables provided by
#'   \acronym{SILO} is available here,
#'   <https://data.longpaddock.qld.gov.au/static/publications/Evapotranspiration_overview.pdf>.
#'
#' @section Data codes:
#' Data codes
#' Where possible (depending on the file format), the data are supplied with
#'   codes indicating how each datum was obtained.
#'
#'   \describe{
#'     \item{0}{Official observation as supplied by the Bureau of Meteorology}
#'     \item{15}{Deaccumulated rainfall (original observation was recorded
#'       over a period exceeding the standard 24 hour observation period)}
#'     \item{25}{Interpolated from daily observations for that date}
#'     \item{26}{Synthetic Class A pan evaporation, calculated from
#'       temperatures, radiation and vapour pressure}
#'     \item{35}{Interpolated from daily observations using an anomaly
#'       interpolation method}
#'     \item{75}{Interpolated from the long term averages of daily
#'       observations for that day of year}
#'   }
#'
#' @return a [data.table::data.table()] with the weather data queried with
#'   the weather variables in alphabetical order. The first eight columns will
#'   always be:
#'
#'   * `longitude`,
#'   * `latitude`,
#'   * `elev_m` (elevation in metres),
#'   * `date` (ISO8601 format, YYYYMMDD),
#'   * `year`,
#'   * `month`,
#'   * `day`,
#'   * `extracted` (the date on which the query was made)
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
#' 3. Zajaczkowski, J., Wong, K., & Carter, J. (2013). Improved historical
#'   solar radiation gridded data for Australia, *Environmental Modelling &
#'   Software*, Volume 49, 64–77. DOI: \doi{10.1016/j.envsoft.2013.06.013}.
#'
#' 4. Food and Agriculture Organization of the United Nations,
#'   Irrigation and drainage paper 56: Crop evapotranspiration - Guidelines for
#'   computing crop water requirements, 1998.
#'
#' 5. ASCE’s Standardized Reference Evapotranspiration Equation, proceedings of
#'   the National Irrigation Symposium, Phoenix, Arizona, 2000.
#'
#' 6. For further details refer to Jeffrey, S.J., Carter, J.O., Moodie, K.B. and
#'   Beswick, A.R. (2001). Using spatial interpolation to construct a
#'   comprehensive archive of Australian climate data, *Environmental Modelling
#'   and Software*, Volume 16/4, 309-330. DOI:
#'   \doi{10.1016/S1364-8152(01)00008-1}.
#'
#' @examples
#' \dontrun{
#' # requires an API key as your email address
#' # Source data from latitude and longitude coordinates (gridded data) for
#' # max and minimum temperature and rainfall for Southwood, QLD.
#' wd <- get_data_drill(
#'   latitude = -27.85,
#'   longitude = 150.05,
#'   start_date = "20221001",
#'   end_date = "20221201",
#'   values = c("max_temp", "min_temp", "rain"),
#'   api_key = "your_api_key"
#' )
#' }
#'
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au}, and Adam H.
#'   Sparks, \email{adamhsparks@@gmail.com}
#'
#' @family SILO
#' @family data fetching
#' @encoding UTF-8
#' @export

get_data_drill <- function(longitude,
                           latitude,
                           start_date,
                           end_date = Sys.Date(),
                           values = "all",
                           api_key = get_key(service = "SILO")) {
  if (missing(longitude) || missing(latitude)) {
    stop("Please supply a valid values for `longitude` and `latitude`.")
  }

  if (missing(start_date)) {
    stop("Please supply a valid start date as `start_date`.")
  }

  .check_not_example_api_key(api_key)
  .is_valid_email_silo_api_key(api_key)

  # validate user-provided lon and lat values
  lonlat <- .check_lonlat(longitude = longitude, latitude = latitude)

  user_longitude <- lonlat["longitude"]
  user_latitude <- lonlat["latitude"]

  # validate user-provided weather values to query
  .values <- .check_silo_values(.values = values)

  # validate user provided dates
  start_date <- .check_date(start_date)
  end_date <- .check_date(end_date)
  .check_date_order(start_date, end_date)
  .check_earliest_available_silo(start_date)

  # reformat date for sending to SILO
  start_date <- gsub("-", "", start_date)
  end_date <- gsub("-", "", end_date)

  out <- .query_silo_api(
    .longitude = longitude,
    .latitude = latitude,
    .start_date = start_date,
    .end_date = end_date,
    .values = .values,
    .format = "csv",
    .api_key = api_key,
    .dataset = "DataDrill"
  )

  data.table::setcolorder(out, order(names(out)))
  data.table::setcolorder(out,
                          c("longitude",
                            "latitude",
                            "year",
                            "month",
                            "day",
                            "date"))

  return(out[])
}

