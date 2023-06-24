
#' PatchedPoint Weather Data
#'
#' Fetch nicely formatted weather data from the \acronym{SILO} \acronym{API}
#'   derived from the \acronym{BOM} station observations (PatchedPoint) data.
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
#' @param station_code A `character` string of the \acronym{BOM} station code
#'   for the station of interest.
#' @param start_date A `character` string or `Date` object representing the
#'   beginning of the range to query in the format 'yyyy-mm-dd' (ISO8601).  Will
#'   return data inclusive of this range.
#' @param end_date A `character` string or `Date` object representing the end of
#'   the range query in the format 'yyyy-mm-dd' (ISO8601).  Will return data
#'   inclusive of this range.  Defaults to the current system date.
#' @param values A `character` string with the type of weather data to
#'   return.  See **Available Values** for a full list of valid values.
#'   Defaults to 'all' with all available values being returned.
#' @param api_key A `character `string specifying a valid email address to use
#'   for the request.  The query will return an error if a valid email address
#'   is not provided.
#'
#' @section Available Values:
#'
#' \describe{
#'  \item{all}{Which will return all of the following values}
#'  \item{rain}{Rainfall}
#'  \item{max_temp}{Maximum temperature}
#'  \item{min_temp}{Minimum temperature}
#'  \item{vp}{Vapour pressure}
#'  \item{vp_deficit}{Vapour pressure deficit}
#'  \item{evap_pan}{Class A pan evaporation}
#'  \item{evap_syn}{Synthetic estimate^1}
#'  \item{evap_comb}{Combination (synthetic estimate pre-1970, class A pan 1970
#'    onwards)}
#'  \item{evap_morton_lake}{Morton's shallow lake evaporation}
#'  \item{radiation}{Solar exposure, consisting of both direct and diffuse
#'    components}
#'  \item{rh_tmax}{Relative humidity at the time of maximum temperature}
#'  \item{rh_tmin}{Relative humidity at the time of minimum temperature}
#'  \item{et_short_crop}{FAO56^4 short crop}
#'  \item{et_tall_crop}{ASCE^5 tall crop^6}
#'  \item{et_morton_actual}{Morton's areal actual evapotranspiration}
#'  \item{et_morton_potential}{Morton's point potential evapotranspiration}
#'  \item{et_morton_wet}{Morton's wet-environment areal potential
#'    evapotranspiration over land}
#'  \item{mslp}{Mean sea level pressure}
#' }
#'
#' @section Value information:
#'
#' Solar radiation: total incoming downward shortwave radiation on a horizontal
#'   surface, derived from estimates of cloud oktas and sunshine duration^3.
#'
#' Relative humidity: calculated using the vapour pressure measured at 9am, and
#'   the saturation vapour pressure computed using either the maximum or minimum
#'   temperature^6.
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
#'   \tabular{rl}{
#'     **Code**\tab **Source**\cr
#'     **0**:\tab Official observation as supplied by the Bureau of
#'       Meteorology.\cr
#'     **15**:\tab Deaccumulated rainfall (original observation was recorded
#'       over a period exceeding the standard 24 hour observation period).\cr
#'     **25**:\tab Interpolated from daily observations for that date.\cr
#'     **26**:\tab Synthetic Class A pan evaporation, calculated from
#'       temperatures, radiation and vapour pressure.\cr
#'     **35**:\tab Interpolated from daily observations using an anomaly
#'       interpolation method.\cr
#'     **75**:\tab Interpolated from the long term averages of daily
#'       observations for that day of year.\cr
#'   }
#'
#' @return a [data.table::data.table] with the weather data queried with the
#'   weather variables in alphabetical order. The first eight columns will
#'   always be:
#'
#'   * station_code,
#'   * station_name,
#'   * longitude,
#'   * latitude,
#'   * elev_m (elevation in metres),
#'   * date (ISO8601 format, "YYYYMMDD"),
#'   * year,
#'   * month,
#'   * day,
#'   * extracted (the date on which the query was made)
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
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au} and Adam
#'   Sparks, \email{adam.sparks@@dpird.wa.gov.au}
#'
#' @family SILO
#'
#' @examples
#' \dontrun{
#' # requires an API key as your email address
#' # Source observation data for station Wongan Hills station, WA (008137)
#' wd <- get_patched_point(station_code = "008137",
#'                start_date = "2021-06-01",
#'                end_date = "2021-07-01",
#'                values = "all",
#'                api_key = "your@@email")
#' }
#' @export

get_patched_point <- function(station_code,
                              start_date,
                              end_date = Sys.Date(),
                              values = "all",
                              api_key) {

  if (missing(station_code)) {
    stop(call. = FALSE,
         "Please supply a valid `station_code`.")
  }

  if (missing(start_date))
    stop(call. = FALSE,
         "Please supply a valid start date as `start_date`.")

  # Error if api_key is not provided
  if (missing(api_key)) {
    stop(
      "A valid email address must be provided for `api_key`.",
      call. = FALSE
    )
  }

  if (any(values == "all")) {
    .values <- unname(silo_daily_values)
  } else {
    if (any(values %notin% names(silo_daily_values))) {
      stop(call. = FALSE,
           "You have specified invalid weather values.")
    }
    .values <- silo_daily_values[names(silo_daily_values) %in%
                                        values]
  }

  # validate user provided dates
  start_date <- .check_date(start_date)
  end_date <- .check_date(end_date)
  .check_date_order(start_date, end_date)

  # reformat date for sending to SILO
  start_date <- gsub("-", "", start_date)
  end_date <- gsub("-", "", end_date)

  silo_return <- .query_silo_api(
    .station_code = station_code,
    .start_date = start_date,
    .end_date = end_date,
    .values = .values,
    .api_key = api_key,
    .dataset = "PatchedPoint"
  )

  data.table::setcolorder(silo_return, c("station_code", "station_name"))

  silo_return[]
}

