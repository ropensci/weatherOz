
#' Get patched point weather data from the SILO API
#'
#' @param station_code A `character` string of the \acronym{BOM} station code
#'   for the station of interest.
#' @param start_date A `character` string representing the beginning of the
#'   range to query in the format 'yyyy-mm-dd' (ISO8601).  Will return data
#'   inclusive of this range.
#' @param end_date A `character` string representing the end of the range query
#'   in the format 'yyyy-mm-dd' (ISO8601).  Will return data inclusive of this
#'   range.  Defaults to the current system date.
#' @param which_values A `character` string with the type of summarised weather
#'   to return.  See **Available Values** for a full list of valid values.
#'   Defaults to 'all' with all available values being returned.\
#' @param interval A `character` string that indicates the time interval to
#'   summarise over.  Default is "daily", optionally "monthly" and "yearly" are
#'   available.  See Details for more.
#' @param api_key A `character `string specifying a valid email address to use
#'   for the request.  The query will return an error if a valid email address
#'   is not provided.
#'
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
#' @export

get_patched_point <- function(station_code,
                              start_date,
                              end_date = Sys.Date(),
                              which_values = "all",
                              interval = "daily",
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

  if (any(which_values == "all")) {
    .which_values <- names(silo_daily_values)
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

  silo_return <- .query_silo_api(.station_code = station_code,
                                 .start_date = start_date,
                                 .end_date = end_date,
                                 .which_values = which_values,
                                 .api_key = api_key,
                                 .dataset = "PatchedPoint")

}

.query_silo_api <- function(.station_code,
                            .start_date,
                            .end_date,
                            .which_values,
                            .api_key,
                            .dataset) {
  base_url <- "https://www.longpaddock.qld.gov.au/cgi-bin/silo/"

  end_point <- data.table::fcase(
    .dataset == "PatchedPoint", "PatchedPointDataset.php",
    .dataset == "DataDrill", "DataDrillDataset.php"
  )

  silo_query_list <- list(
    station = as.integer(.station_code),
    start = as.character(.start_date),
    finish = as.character(.end_date),
    format = "csv",
    comment = paste(.which_values, collapse = ""),
    username = .api_key,
    password = "api_request"
  )

  client <-
    crul::HttpClient$new(url = sprintf("%s%s", base_url, end_point))
  response <- client$get(query = silo_query_list)

  # check responses for errors
  # check to see if request failed or succeeded
  # - a custom approach this time combining status code,
  #   explanation of the code, and message from the server
  if (response$status_code > 201) {
    mssg <- response$parse("UTF-8")
    x <- response$status_http()
    stop(sprintf("HTTP (%s) - %s\n  %s", x$status_code, x$explanation, mssg),
         call. = FALSE)
  }
  response$raise_for_status()

  # the API won't return proper responses for malformed requests, so, we check
  # for the word "Sorry" and parse the response to the user if something slips
  # through our user checks.
  if (grepl("Sorry", jsonlite::fromJSON(response$parse("UTF8")))) {
    stop(call. = FALSE,
         gettextf(response$parse("UTF8")),
         domain = NA)
  }

  if (grepl("Request Rejected", response$parse("UTF8"))) {
    stop(call. = FALSE,
         gettextf(response$parse("UTF8")),
         domain = NA)
  }

  response <- data.table::fread(I(response$parse("UTF8")))

}

#' Query the SILO API using {crul}
#'
#' @param .end_point A SILO API end point
#' @param .query_list a list of values in the API to query
#'
#' @return A `data.table` of data for manipulating before returning to the user
#'
#' @noRd
#' @keywords internal

.query_silo_api <- function(.end_point,
                             .query_list) {

  if (!is.null(.end_point)) {
    .base_url <- sprintf("https://api.dpird.wa.gov.au/v2/weather/stations/%s",
                         .end_point)
  } else {
    .base_url <- "https://api.dpird.wa.gov.au/v2/weather/stations/"
  }

  connection <- crul::HttpClient$new(url = .base_url)

  client <- crul::Paginator$new(
    client = connection,
    limit = .limit,
    limit_param = "limit",
    offset_param = "offset",
    chunk = 1000
  )
  response <- client$get(query = .query_list)

  # check to see if request failed or succeeded
  # - a custom approach this time combining status code,
  #   explanation of the code, and message from the server
  # check response from start_date item in list, should be same across all
  if (response[[1]]$status_code > 201) {
    x <- jsonlite::fromJSON(response[[1]]$parse("UTF8"))$error$errors
    stop(sprintf(
      "HTTP (%s) - %s\n  %s",
      x[, "code"],
      x[, "message"],
      x[, "description"]
    ),
    call. = FALSE)
  }

  response[[1]]$raise_for_status()
  response[[1]]$raise_for_ct_json()
  return(response)
}
