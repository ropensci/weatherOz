
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
#' @return An [apsimx] object of class \sQuote{met} with attributes.
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


  silo_return <- .query_silo_api(
    .longitude = longitude,
    .latitude = latitude,
    .start_date = start_date,
    .end_date = end_date,
    .format = "apsim",
    .api_key = api_key,
    .dataset = "DataDrill"
  )
}
