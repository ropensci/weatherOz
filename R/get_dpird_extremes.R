
#' Get DPIRD Extreme Weather Event Summaries
#'
#' Fetch nicely formatted individual extreme weather summaries from the
#'   \acronym{DPIRD} Weather 2.0 \acronym{API}.
#'
#' @param station_code A `character` string or `factor` from
#'   [get_stations_metadata()] of the \acronym{BOM} station code for the station
#'   of interest.
#' @param values A `character` string with the type of extreme weather to
#'   return.  See **Available Values** for a full list of valid values.
#'   Defaults to `all`, returning the full list of values unless otherwise
#'   specified.
#' @param api_key A `character` string containing your \acronym{API} key from
#'   \acronym{DPIRD}, <https://www.agric.wa.gov.au/web-apis>, for the
#'   \acronym{DPIRD} Weather 2.0 \acronym{API}.  Defaults to automatically
#'   detecting your key from your local .Renviron, .Rprofile or similar.
#'   Alternatively, you may directly provide your key as a string here.  If
#'   nothing is provided, you will be prompted on how to set up your \R session
#'   so that it is auto-detected.
#'
#' @section Available Values:
#'
#' * all (which will return all of the following values),
#' * erosionCondition,
#' * erosionConditionLast7Days,
#' * erosionConditionLast7DaysDays,
#' * erosionConditionLast7DaysMinutes,
#' * erosionConditionLast14Days,
#' * erosionConditionLast14DaysDays,
#' * erosionConditionLast14DaysMinutes,
#' * erosionConditionMonthToDate,
#' * erosionConditionMonthToDateDays,
#' * erosionConditionMonthToDateMinutes,
#' * erosionConditionMonthToDateStartTime,
#' * erosionConditionSince12AM,
#' * erosionConditionSince12AMMinutes,
#' * erosionConditionSince12AMStartTime,
#' * erosionConditionYearToDate,
#' * erosionConditionYearToDateDays,
#' * erosionConditionYearToDateMinutes,
#' * erosionConditionYearToDateStartTime,
#' * frostCondition,
#' * frostConditionLast7Days,
#' * frostConditionLast7DaysDays,
#' * frostConditionLast7DaysMinutes,
#' * frostConditionLast14Days,
#' * frostConditionLast14DaysDays,
#' * frostConditionLast14DaysMinutes,
#' * frostConditionMonthToDate,
#' * frostConditionMonthToDateDays,
#' * frostConditionMonthToDateMinutes,
#' * frostConditionMonthToDateStartTime,
#' * frostConditionSince9AM,
#' * frostConditionSince9AMMinutes,
#' * frostConditionSince9AMStartTime,
#' * frostConditionTo9AM,
#' * frostConditionTo9AMMinutes,
#' * frostConditionTo9AMStartTime,
#' * frostConditionYearToDate,
#' * frostConditionYearToDate,
#' * frostConditionYearToDateMinutes,
#' * frostConditionYearToDateStartTime,
#' * heatCondition,
#' * heatConditionLast7Days,
#' * heatConditionLast7DaysDays,
#' * heatConditionLast7DaysMinutes,
#' * heatConditionLast14Days,
#' * heatConditionLast14DaysDays,
#' * heatConditionLast14DaysMinutes,
#' * heatConditionMonthToDate,
#' * heatConditionMonthToDateDays,
#' * heatConditionMonthToDateMinutes,
#' * heatConditionMonthToDateStartTime,
#' * heatConditionSince12AM,
#' * heatConditionSince12AMMinutes,
#' * heatConditionSince12AMStartTime,
#' * heatConditionYearToDate,
#' * heatConditionYearToDateDays,
#' * heatConditionYearToDateMinutes, and
#' * heatConditionYearToDateStartTime
#'
#' @return a [data.table::data.table()] of one row with `station_code`,
#'   `station_name`, `latitude`, `longitude`, `date_time` of the query and the
#'   extreme weather information according to the value(s) selected.
#'
#' @family DPIRD
#' @family data fetching
#'
#' @examples
#' \dontrun{
#' # Query Bonnie Rock station for wind erosion and heat extreme events
#' # Note that you need to supply your own API key
#'
#' xtreme <- get_dpird_extremes(
#'   station_code = "BR",
#'   values = c("erosionCondition",
#'            "heatCondition"),
#'   api_key = "your_api_key"
#' )
#' }
#'
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au}, and Adam
#'   Sparks, \email{adamhsparks@@gmail.com}
#'
#' @autoglobal
#' @export

get_dpird_extremes <- function(station_code,
                                values = "all",
                                api_key = get_key(service = "DPIRD")) {

  .check_not_example_api_key(api_key)
  .is_valid_dpird_api_key(api_key)

  # simplify using the metadata to fetch weather data by converting factors to
  # numeric values
  if (inherits(x = station_code, what = "factor")) {
    station_code <- as.character(station_code)
  }

  if (missing(station_code) | !is.character(station_code)) {
    stop(
      call. = FALSE,
      "Please provide a station code via the `station_code` argument.\n",
      "It should take a string e.g., `AN001` for Allanooka station.\n"
    )
  }

  if (length(station_code) != 1L) {
    stop(call. = FALSE,
         "You have provided more than one `station_code`.\n",
         "This function only handles one `station_code` per query.\n")
  }

  .check_not_example_api_key(api_key)
  .is_valid_dpird_api_key(api_key)

  if (any(values != "all") &&
      any(values %notin% weatherOz::dpird_extreme_weather_values)) {
    stop(call. = FALSE,
         "You have specified invalid weather values.")
  }

  if (any(values == "all")) {
    .values <-  c("stationCode",
                  "longitude",
                  "latitude",
                  weatherOz::dpird_extreme_weather_values)
  } else {
    .values <-
      c("stationCode",
        "longitude",
        "latitude",
        weatherOz::dpird_extreme_weather_values[
          weatherOz::dpird_extreme_weather_values %in% values])
  }

  query_list <- list(
    stationCode = station_code,
    offset = 0L,
    select = paste(.values, collapse = ","),
    group = "all",
    includeClosed = TRUE,
    api_key = api_key
  )

  out <- .query_dpird_api(.end_point = "extreme-conditions",
                          .query_list = query_list,
                          .limit = 1L)

  out <- jsonlite::fromJSON(out[[1]]$parse("UTF8"))
  out <- data.table::data.table(out$collection)

  .set_snake_case_names(out)

  if (any(grep("time", colnames(out)))) {
    out[, grep("time", colnames(out)) := suppressMessages(lapply(
      .SD,
      lubridate::ymd_hms,
      truncated = 3,
      tz = "Australia/West"
    )),
    .SDcols = grep("time", colnames(out))]
  }

  out[, station_code := as.factor(station_code)]
  data.table::setkey(x = out, cols = station_code)
  data.table::setcolorder(out, c("station_code", "longitude", "latitude"))
  return(out[])
}
