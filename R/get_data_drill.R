
#' Get DataDrill weather data from the SILO API
#'
#' Download weather data from the \acronym{SILO} \acronym{API} for spatially
#'   interpolated weather data (DataDrill).  The daily climate surfaces have
#'   been derived either by splining or kriging the observational data.  The
#'   grid spans 112° to 154°, -10° to -44° with resolution 0.05° latitude by
#'   0.05° longitude (approximately 5 km × 5 km).
#'
#' @param longitude A single `numeric` value  representing the longitude of the
#'    point-of-interest.
#' @param latitude A single `numeric` value representing the latitude of the
#'   point-of-interest.
#' @param start_date A `character` string representing the beginning of the
#'   range to query in the format 'yyyy-mm-dd' (ISO8601).  Will return data
#'   inclusive of this range.
#' @param end_date A `character` string representing the end of the range query
#'   in the format 'yyyy-mm-dd' (ISO8601).  Will return data inclusive of this
#'   range.  Defaults to the current system date.
#' @param which_values A `character` string with the type of weather data to
#'   return.  See **Available Values** for a full list of valid values.
#'   Defaults to 'all' with all available values being returned.
#' @param interval A `character` string that indicates the time interval
#'   requested.  Default is "daily", optionally "monthly" and "yearly" are
#'   available.  See Details for more.
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
#' @details
#' When `interval` is a summary, _i.e._, "monthly" or "yearly", (i) monthly or
#'   yearly totals for rainfall and evaporation; and (ii) monthly or yearly
#'   means for maximum and minimum temperatures, solar radiation and vapour
#'   pressure are returned.
#'
#' @return a [data.table::data.table] with 'station_code' and date interval
#'   queried together with the requested weather variables in alphabetical
#'   order. The first six columns will always be:
#'
#'   * station_code,
#'   * station_name,
#'   * year,
#'   * month (if daily or monthly)
#'   * day (if daily), and
#'   * date (if daily or monthly ISO8601 format, "YYYYMMDD")
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
#'
#' @family SILo
#'
#' @export

get_data_drill <- function(longitude,
                           latitude,
                           start_date,
                           end_date = Sys.Date(),
                           which_values = "all",
                           interval = "daily",
                           api_key) {


  if (missing(longitude) || missing(latitude)) {
    stop(call. = FALSE,
         "Please supply a valid values for `longitude` and `latitude`.")
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

  # validate user-provided lon and lat values
  .check_lonlat(longitude = longitude, latitude = latitude)

  # validate user-provided data values and return .which_values object
  if (any(which_values == "all")) {
    .which_values <- unname(silo_daily_values)
  } else {
    if (any(which_values %notin% names(silo_daily_values))) {
      stop(call. = FALSE,
           "You have specified invalid weather values.")
    }
    .which_values <- silo_daily_values[names(silo_daily_values) %in%
                                        which_values]
  }

  # validate user provided dates
  start_date <- .check_date(start_date)
  end_date <- .check_date(end_date)
  .check_date_order(start_date, end_date)

  # reformat date for sending to SILO
  start_date <- gsub("-", "", start_date)
  end_date <- gsub("-", "", end_date)

  # Use `agrep()` to fuzzy match the user-requested time interval
  approved_intervals <- c("daily",
                          "monthly",
                          "yearly")

  likely_interval <- agrep(pattern = interval,
                           x = approved_intervals)

  # Match time interval query to user requests
  checked_interval <- try(match.arg(approved_intervals[likely_interval],
                                    approved_intervals,
                                    several.ok = FALSE),
                          silent = TRUE
  )

  silo_return <- .query_silo_api(
    .longitude = longitude,
    .latitude = latitude,
    .start_date = start_date,
    .end_date = end_date,
    .which_values = .which_values,
    .api_key = api_key,
    .dataset = "DataDrill"
  )

  silo_return[]
}

