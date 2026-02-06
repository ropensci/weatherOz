
#' Get DPIRD Weather Data in Summarised Formats
#'
#' Fetch nicely formatted individual station weather summaries from the
#'   \acronym{DPIRD} Weather 2.0 \acronym{API}.
#'
#' # Start Dates
#'
#' The earliest available data start from August of 2000 for Vasse, \dQuote{VA}.
#'
#' # Column Name Details
#'
#' Column names are converted from the default returns of the \acronym{API} to
#'    be snake_case formatted and where appropriate, the names of the values
#'    that are analogous between \acronym{SILO} and \acronym{DPIRD} data are
#'    named using the same name for ease of interoperability, _e.g._, using
#'    `rbind()` to create a `data.table` that contains data from both APIs.
#'    However, use with caution and don't mix datasets of different time-steps,
#'    _i.e._, this function gets many summary values not just \dQuote{daily}
#'    time-step data.  The functions that access the \acronym{SILO}
#'    \acronym{API} only provide access to daily data, so don't mix (sub)hourly,
#'    monthly or yearly data from \acronym{DPIRD} with \acronym{SILO}.
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
#' @param interval A `character` string that indicates the time interval to
#`   summarise over.  Default is `daily`; others are `15min`, `30min`, `hourly`,
#'   `monthly` or `yearly`.  For intervals shorter than 1 day, the time period
#'   covered will be midnight to midnight, with the end_date time interval being
#'   before midnight - hour/minute values are for the end of the time period.
#'   Data for shorter intervals (`15min`, `30min`) are available from January of
#'   the previous year.
#' @param values A `character` string with the type of summarised weather
#'   to return.  See **Available Values** for a full list of valid values.
#'   Defaults to `all` with all available values being returned.
#' @param api_key A `character` string containing your \acronym{API} key from
#'   \acronym{DPIRD}, <https://www.dpird.wa.gov.au/online-tools/apis/>, for the
#'   \acronym{DPIRD} Weather 2.0 \acronym{API}.  Defaults to automatically
#'   detecting your key from your local .Renviron, .Rprofile or similar.
#'   Alternatively, you may directly provide your key as a string here.  If
#'   nothing is provided, you will be prompted on how to set up your \R session
#'   so that it is auto-detected.
#'
#' @section Available Values:
#'
#'   * all (which will return all of the following values),
#'   * airTemperature,
#'   * airTemperatureAvg,
#'   * airTemperatureMax,
#'   * airTemperatureMaxTime,
#'   * airTemperatureMin,
#'   * airTemperatureMinTime,
#'   * apparentAirTemperature,
#'   * apparentAirTemperatureAvg,
#'   * apparentAirTemperatureMax,
#'   * apparentAirTemperatureMaxTime,
#'   * apparentAirTemperatureMin,
#'   * apparentAirTemperatureMinTime,
#'   * barometricPressure,
#'   * barometricPressureAvg,
#'   * barometricPressureMax,
#'   * barometricPressureMaxTime,
#'   * barometricPressureMin,
#'   * barometricPressureMinTime,
#'   * battery,
#'   * batteryMinVoltage,
#'   * batteryMinVoltageDateTime,
#'   * chillHours,
#'   * deltaT,
#'   * deltaTAvg,
#'   * deltaTMax,
#'   * deltaTMaxTime,
#'   * deltaTMin,
#'   * deltaTMinTime,
#'   * dewPoint,
#'   * dewPointAvg,
#'   * dewPointMax,
#'   * dewPointMaxTime,
#'   * dewPointMin,
#'   * dewPointMinTime,
#'   * erosionCondition,
#'   * erosionConditionMinutes,
#'   * erosionConditionStartTime,
#'   * errors,
#'   * etoShortCrop,
#'   * etoTallCrop,
#'   * evapotranspiration,
#'   * evapotranspirationShortCrop,
#'   * evapotranspirationTallCrop,
#'   * frostCondition,
#'   * frostConditionMinutes,
#'   * frostConditionStartTime,
#'   * heatCondition,
#'   * heatConditionMinutes,
#'   * heatConditionStartTime,
#'   * observations,
#'   * observationsCount,
#'   * observationsPercentage,
#'   * panEvaporation,
#'   * panEvaporation12AM,
#'   * rainfall,
#'   * relativeHumidity,
#'   * relativeHumidityAvg,
#'   * relativeHumidityMax,
#'   * relativeHumidityMaxTime,
#'   * relativeHumidityMin,
#'   * relativeHumidityMinTime,
#'   * richardsonUnits,
#'   * soilTemperature,
#'   * soilTemperatureAvg,
#'   * soilTemperatureMax,
#'   * soilTemperatureMaxTime,
#'   * soilTemperatureMin,
#'   * soilTemperatureMinTime,
#'   * solarExposure,
#'   * wetBulb,
#'   * wetBulbAvg,
#'   * wetBulbMax,
#'   * wetBulbMaxTime,
#'   * wetBulbMin,
#'   * wetBulbMinTime,
#'   * wind,
#'   * windAvgSpeed, and
#'   * windMaxSpeed
#'
#' @return a [data.table::data.table()] with `station_code` and the date
#'   interval queried together with the requested weather variables in
#'   alphabetical order.  The first ten columns will always be:
#'
#'   * `station_code`,
#'   * `station_name`,
#'   * `longitude`,
#'   * `latitude`,
#'   * `year`,
#'   * `month`,
#'   * `day`,
#'   * `hour`,
#'   * `minute`, and if `month` or finer is present,
#'   * `date` (a combination of year, month, day, hour, minute as appropriate).
#'
#' @note Please note this function converts date-time columns from Coordinated
#'   Universal Time \sQuote{UTC} to Australian Western Standard Time
#'   \sQuote{AWST}.
#'
#' @family DPIRD
#' @family data fetching
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}, and Rodrigo
#'   Pires, \email{rodrigo.pires@@dpird.wa.gov.au}
#'
#' @examples
#' \dontrun{
#' # Note that you need to supply your own API key
#' # Use default for end date (current system date) to get rainfall
#'
#' wd <- get_dpird_summaries(
#'    station_code = "CL001",
#'    start_date = "20171028",
#'    api_key = "your_api_key",
#'    interval = "yearly",
#'    values = "rainfall"
#' )
#'
#' # Only for wind and erosion conditions for daily time interval
#'
#' wd <- get_dpird_summaries(
#'   station_code = "BI",
#'   start_date = "20220501",
#'   end_date = "20220502",
#'   api_key = "your_api_key",
#'   interval = "daily",
#'   values = c(
#'     "wind",
#'     "erosionCondition",
#'     "erosionConditionMinutes",
#'     "erosionConditionStartTime"
#'     )
#' )
#' }
#' @autoglobal
#' @export

get_dpird_summaries <- function(station_code,
                                start_date,
                                end_date = Sys.Date(),
                                interval = c("daily",
                                             "15min",
                                             "30min",
                                             "hourly",
                                             "monthly",
                                             "yearly"),
                                values = "all",
                                api_key = get_key(service = "DPIRD")) {

  .check_not_example_api_key(api_key)
  .is_valid_dpird_api_key(api_key)

  # this section is necessary to double check availability dates of DPIRD
  # stations to give users a better experience. It slows down the first query
  # but avoids confusion when no values are returned for a station that exists
  # but the date requested pre-dates the station data
  if (Sys.info()['sysname'] == "Windows") {
    metadata_file <- file.path(tempdir(), "dpird_metadata.Rda", fsep = "\\")

    if (!file.exists(metadata_file)) {
      saveRDS(
        get_stations_metadata(
          which_api = "dpird",
          api_key = api_key,
          include_closed = TRUE
        ),
        file = metadata_file,
        compress = FALSE
      )
    }

  } else {
    metadata_file <- file.path(tempdir(), "dpird_metadata.Rda")

    if (!file.exists(metadata_file)) {
      saveRDS(
        get_stations_metadata(
          which_api = "dpird",
          api_key = api_key,
          include_closed = TRUE
        ),
        file = metadata_file,
        compress = FALSE
      )
    }
  }

  # simplify using the metadata to fetch weather data by converting factors to
  # numeric values
  if (inherits(x = station_code, what = "factor")) {
    station_code <- as.character(station_code)
  }

  if (missing(station_code) | !is.character(station_code)) {
    stop(call. = FALSE, "Please supply a valid `station_code`.")
  }

  if (missing(start_date))
    stop(call. = FALSE, "Please supply a valid start date as `start_date`.")

  if (any(values == "all")) {
    values <-
      c("stationCode",
        "stationName",
        "period",
        weatherOz::dpird_summary_values)
  } else {
    if (any(values %notin% weatherOz::dpird_summary_values)) {
      stop(call. = FALSE, "You have specified invalid weather values.")
    }
    values <-
      c("stationCode", "stationName", "period", values)
  }

  # validate user provided dates
  start_date <- .check_date(start_date)
  end_date <- .check_date(end_date)
  .check_date_order(start_date, end_date)
  .check_earliest_available_dpird(station_code, start_date, metadata_file)

  # if interval is not set, default to "daily", else check input to be sure
  approved_intervals <- c("daily",
                          "15min",
                          "30min",
                          "hourly",
                          "monthly",
                          "yearly")

  if (identical(interval, approved_intervals)) {
    interval <- "daily"
  }

  likely_interval <- agrep(pattern = interval, x = approved_intervals)

  # Match time interval query to user requests
  checked_interval <-
    try(match.arg(approved_intervals[likely_interval],
                  approved_intervals,
                  several.ok = FALSE),
        silent = TRUE)

  # TODO
  # Remove this once Phil fix the daily values names
  # Check if the interval is "daily" and modify values accordingly
  if (checked_interval == "daily") {
    values <-
      setdiff(values,
              c("etoShortCrop",
                "etoTallCrop")
      )
  } else {
    values <-
      setdiff(values,
              c("evapotranspirationShortCrop",
                "evapotranspirationTallCrop",
                "panEvaporation12AM")
      )
  }

  # Error if summary interval is not available. API only allows for daily,
  # 15 min, 30 min, hourly, monthly or yearly
  if (methods::is(checked_interval, "try-error")) {
    stop(call. = FALSE, "\"", interval, "\" is not a supported time interval")
  }

  request_interval <- lubridate::interval(start_date,
                                          end_date,
                                          tzone = "Australia/Perth")

  # Stop if query is for 15 and 30 min intervals and date is more than one
  # year in the past
  this_year <- lubridate::year(lubridate::today())

  if (checked_interval %in% c("15min", "30min") &
      lubridate::year(start_date) <
      this_year - 1 |
      checked_interval %in% c("15min", "30min") &
      lubridate::year(end_date) <
      this_year - 1) {
    stop(
      call. = FALSE,
      "Start date is too early. Data in 15 and 30 min intervals are only ",
      "available from the the 1st day of ", this_year - 1, "."
    )
  }

  # determine how many records are being requested. Default here is 'daily' as
  # with default user arguments
  total_records_req <- data.table::fcase(
    checked_interval == "yearly",
    floor(lubridate::time_length(request_interval, unit = "year") + 1),
    checked_interval == "monthly",
    floor(lubridate::time_length(request_interval, unit = "month") + 1),
    checked_interval == "hourly",
    floor(lubridate::time_length(request_interval, unit = "hour") + 1),
    checked_interval == "30min",
    floor((
      lubridate::time_length(request_interval, unit = "hour")
    ) + 1) * 2,
    checked_interval == "15min",
    floor((
      lubridate::time_length(request_interval, unit = "hour")
    ) + 1) * 4,
    default = floor(lubridate::time_length(request_interval, unit = "day") + 1)
  )

  if (total_records_req < 1) {
    stop(
      call. = FALSE,
      "You have submitted a query with 0 total records.\n",
      "Please extend the dates requested."
    )
  }

  # TODO: When Phil gets lat/lon values added to the summary results from the
  # API, remove this bit here and add lat/lon to the list of queried values
  if (Sys.info()['sysname'] == "Windows") {
    metadata_file <- file.path(tempdir(), "dpird_metadata.Rda", fsep = "\\")

    if (!file.exists(metadata_file)) {
      saveRDS(
        get_stations_metadata(which_api = "dpird", api_key = api_key),
        file = metadata_file,
        compress = FALSE
      )
    }

  } else {
    metadata_file <- file.path(tempdir(), "dpird_metadata.Rda")

    if (!file.exists(metadata_file)) {
      saveRDS(
        get_stations_metadata(which_api = "dpird", api_key = api_key),
        file = metadata_file,
        compress = FALSE
      )
    }
  }

  # END chunk to remove

  query_list <- .build_query(
    station_code = station_code,
    start_date_time = start_date,
    end_date_time = end_date,
    interval = checked_interval,
    values = values,
    include_closed = TRUE,
    api_key = api_key,
    limit = total_records_req
  )

  # set base URL according to interval
  end_point <- data.table::fcase(
    checked_interval == "15min",
    "summaries/15min",
    checked_interval == "30min",
    "summaries/30min",
    checked_interval == "hourly",
    "summaries/hourly",
    checked_interval == "daily",
    "summaries/daily",
    checked_interval == "monthly",
    "summaries/monthly",
    default = "summaries/yearly"
  )

  out <-
    .parse_summary(
      .ret_list = .query_dpird_api(
        .end_point = end_point,
        .query_list = query_list,
        .limit = total_records_req
      ),
      .values = values
    )

  out[, period.from := NULL]
  out[, period.to := NULL]

  .set_snake_case_names(out)

  # provide some standard names between DPIRD and SILO for easy merging where
  # data values are shared
  # not all columns are renamed, but almost all are listed for clarity

  data.table::setnames(
    out,
    old = c(
      "station_code",
      "station_name",
      "year",
      "month",
      "day",
      "date",
      "air_temperature_avg",
      "air_temperature_max",
      "air_temperature_max_time",
      "air_temperature_min",
      "air_temperature_min_time",
      "apparent_air_temperature_avg",
      "apparent_air_temperature_max",
      "apparent_air_temperature_max_time",
      "apparent_air_temperature_min",
      "apparent_air_temperature_min_time",
      "barometric_pressure",
      "battery_min_voltage",
      "battery_min_voltage_date_time",
      "chill_hours",
      "delta_t_avg",
      "delta_t_max",
      "delta_t_max_time",
      "delta_t_min",
      "delta_t_min_time",
      "dew_point_avg",
      "dew_point_max",
      "dew_point_max_time",
      "dew_point_min",
      "dew_point_min_time",
      "erosion_condition_minutes",
      "erosion_condition_start_time",
      "errors",
      "evapotranspiration",
      "evapotranspiration_short_crop",
      "evapotranspiration_tall_crop",
      "frost_condition_minutes",
      "frost_condition_start_time",
      "heat_condition_minutes",
      "heat_condition_start_time",
      "observations_count",
      "observations_percentage",
      "pan_evaporation",
      "pan_evaporation_12am",
      "rainfall",
      "relative_humidity_avg",
      "relative_humidity_max",
      "relative_humidity_max_time",
      "relative_humidity_min",
      "relative_humidity_min_time",
      "richardson_units",
      "soil_temperature",
      "solar_exposure",
      "wet_bulb_avg",
      "wet_bulb_max",
      "wet_bulb_max_time",
      "wet_bulb_min",
      "wet_bulb_min_time",
      "wind_avg_speed",
      "wind_height",
      "wind_max_direction_compass_point",
      "wind_max_direction_degrees",
      "wind_max_speed",
      "wind_max_time"
    ),
    new = c(
      "station_code",
      "station_name",
      "year",
      "month",
      "day",
      "date",
      "air_tavg",
      "air_tmax",
      "air_tmax_time",
      "air_tmin",
      "air_tmin_time",
      "apparent_air_tavg",
      "apparent_air_tmax",
      "apparent_air_tmax_time",
      "apparent_air_tmin",
      "apparent_air_tmin_time",
      "barometric_pressure",
      "battery_min_voltage",
      "battery_min_voltage_date_time",
      "chill_hours",
      "delta_tavg",
      "delta_tmax",
      "delta_tmax_time",
      "delta_tmin",
      "delta_tmin_time",
      "dew_point_avg",
      "dew_point_max",
      "dew_point_max_time",
      "dew_point_min",
      "dew_point_min_time",
      "erosion_condition_minutes",
      "erosion_condition_start_time",
      "errors",
      "et",
      "et_short_crop",
      "et_tall_crop",
      "frost_condition_minutes",
      "frost_condition_start_time",
      "heat_condition_minutes",
      "heat_condition_start_time",
      "observations_count",
      "observations_percentage",
      "pan_evaporation",
      "pan_evaporation_12am",
      "rainfall",
      "rh_avg",
      "rh_tmax",
      "rh_tmax_time",
      "rh_tmin",
      "rh_tmin_time",
      "richardson_units",
      "soil_temperature",
      "radiation",
      "wet_bulb_avg",
      "wet_bulb_tmax",
      "wet_bulb_tmax_time",
      "wet_bulb_tmin",
      "wet_bulb_tmin_time",
      "wind_avg_speed",
      "wind_height",
      "wind_max_direction_compass_point",
      "wind_max_direction_degrees",
      "wind_max_speed",
      "wind_max_time"
    ),
    skip_absent = TRUE
  )

  # TODO: When Phil gets lat/lon values added to the summary results from the
  # API, remove this bit here and add lat/lon to the list of queried values
  out <- merge(
    x = out,
    y = readRDS(file = metadata_file)[, c(1:2, 5:6)],
    by.x = c("station_code", "station_name"),
    by.y = c("station_code", "station_name")
  )
  # END chunk to remove

  data.table::setcolorder(out, order(names(out)))

  # this function contains several `if` and `if else` branches, so it's in its
  # own function to simplify this parent function and keep it easier to maintain
  # you can find it at the bottom of this file, so you can see what it's doing
  out <-
    .set_col_orders(.out = out, .checked_interval = checked_interval)

  # Determine where wind max times include a time-of-day component before parsing.
  wind_max_has_time <- NULL
  if ("wind_max_time" %in% names(out)) {
    wind_max_has_time <- grepl(":", out$wind_max_time)
  }

  # Parse all time-like columns using a robust helper that handles mixed formats.
  time_cols <- grep("time", colnames(out), value = TRUE)
  if (length(time_cols) > 0L) {
    out[, (time_cols) := suppressMessages(lapply(
      .SD,
      .parse_dpird_time_col
    )), .SDcols = time_cols]
  }

  # Derive date and time-of-day components for wind maxima per row; these will be
  # widened later to per-height columns.
  if (!is.null(wind_max_has_time) && "wind_max_time" %in% names(out)) {
    out[, wind_max_date := as.Date(wind_max_time, tz = "Australia/West")]
    out[, wind_max_time_of_day := ifelse(
      wind_max_has_time & !is.na(wind_max_time),
      format(wind_max_time, "%H:%M:%S", tz = "Australia/West"),
      NA_character_
    )]
  }

  # Reshape wind variables from long format (one row per height) to wide format
  # (one row per period with separate _3m and _10m columns).
  out <- .widen_wind_height_cols(out)

  data.table::setnames(out, gsub("period_", "", names(out)))

  out[, station_code := as.factor(station_code)]

  data.table::setkey(x = out, cols = station_code)

  return(out[])
}


#' Parse DPIRD API summary data
#'
#' Internal function that parses and tidy up data as returned by
#'  `.query_dpird_api()`
#'
#' @param .ret_list a list with the DPIRD weather API response
#' @param .values a character vector with the variables to query. See the
#' `get_dpird_summaries()` for further details.
#'
#' @return a tidy [data.table::data.table()] with station id and requested
#'  weather summaries
#'
#' @noRd
#' @autoglobal
#' @keywords Internal
#'
.parse_summary <- function(.ret_list, .values) {
  x <- vector(mode = "list", length = length(.ret_list))
  for (i in seq_len(length(.ret_list))) {
    y <- jsonlite::fromJSON(.ret_list[[i]]$parse("UTF8"))
    if ("summaries" %in% names(y$collection)) {
      nested_list_objects <-
        data.table::as.data.table(y$collection$summaries)
      # insert `station_name` and `station_code` into the nested_list_objects df
      nested_list_objects[, station_code := y$collection$stationCode]
      nested_list_objects[, station_name := y$collection$stationName]
      x[[i]] <- nested_list_objects
    }
  }

  nested_list_objects <- data.table::rbindlist(x)

  # get the nested list columns and convert them to data.table objects
  col_classes <-
    vapply(nested_list_objects, class, FUN.VALUE = character(1))

  col_lists <- which(col_classes == "list")

  if (length(col_lists) > 0L) {
    list_names <- names(col_lists)
    base_dt <- data.table::copy(nested_list_objects)
    base_dt[, .row_id := seq_len(.N)]

    list_dts <- vector(mode = "list", length = length(list_names))
    list_lengths <- vector(mode = "list", length = length(list_names))
    names(list_dts) <- names(list_lengths) <- list_names

    for (nm in list_names) {
      list_items <- base_dt[[nm]]
      list_item_dts <- lapply(list_items, function(item) {
        if (is.null(item) || length(item) == 0L) {
          return(data.table::data.table())
        }
        data.table::as.data.table(item)
      })

      list_lengths[[nm]] <- vapply(list_item_dts, nrow, integer(1))

      list_dts[[nm]] <- data.table::rbindlist(
        list_item_dts,
        idcol = ".row_id",
        fill = TRUE
      )
      list_dts[[nm]][, .row_id := as.integer(.row_id)]
      list_dts[[nm]][, .sub_index := seq_len(.N), by = .row_id]

      value_cols <- setdiff(names(list_dts[[nm]]), c(".row_id", ".sub_index"))
      if (length(value_cols) > 0L) {
        data.table::setnames(
          list_dts[[nm]],
          value_cols,
          paste0(nm, ".", value_cols)
        )
      }

      # drop the list column from the base data.table
      base_dt[, (nm) := NULL]
    }

    multi_list_names <- names(list_lengths)[
      vapply(list_lengths, function(x) any(x > 1L), logical(1))
    ]

    out <- base_dt

    if (length(multi_list_names) <= 1L) {
      for (nm in list_names) {
        out <- merge(
          out,
          list_dts[[nm]],
          by = ".row_id",
          all.x = TRUE,
          allow.cartesian = TRUE,
          sort = FALSE
        )
        if (".sub_index" %in% names(out)) {
          out[, .sub_index := NULL]
        }
      }
    } else {
      length_dt <- data.table::data.table(.row_id = seq_len(nrow(base_dt)))
      for (nm in multi_list_names) {
        length_dt[[nm]] <- list_lengths[[nm]]
      }

      equal_lengths <- apply(
        length_dt[, ..multi_list_names],
        1,
        function(x) length(unique(x[!is.na(x)])) <= 1L
      )

      if (!all(equal_lengths)) {
        warning(
          "List columns have differing lengths per period; ",
          "output uses cartesian expansion which may misalign values."
        )
        for (nm in list_names) {
          out <- merge(
            out,
            list_dts[[nm]],
            by = ".row_id",
            all.x = TRUE,
            allow.cartesian = TRUE,
            sort = FALSE
          )
          if (".sub_index" %in% names(out)) {
            out[, .sub_index := NULL]
          }
        }
      } else {
        for (nm in list_names) {
          out <- merge(
            out,
            list_dts[[nm]],
            by = c(".row_id", ".sub_index"),
            all.x = TRUE,
            allow.cartesian = TRUE,
            sort = FALSE
          )
        }
        out[, .sub_index := NULL]
      }
    }

    out[, .row_id := NULL]
    return(out)
  }

  return(nested_list_objects)
}

#' Parse DPIRD summary time columns
#'
#' Internal helper to parse mixed time formats returned by the DPIRD API.
#'
#' @param x A vector of time values.
#' @return A POSIXct vector parsed in Australia/West timezone.
#' @keywords Internal
#' @autoglobal
#' @noRd
.parse_dpird_time_col <- function(x) {
  # If already parsed, return as-is.
  if (inherits(x, "POSIXct")) {
    return(lubridate::with_tz(x, "Australia/West"))
  }

  x_chr <- as.character(x)
  x_chr[x_chr == ""] <- NA_character_

  # Clean up ISO-like strings: remove trailing Z, replace T with space, drop
  # fractional seconds. All inputs are treated as UTC before converting to
  # Australia/West.
  cleaned <- sub("Z$", "", x_chr)
  cleaned <- sub("T", " ", cleaned, fixed = TRUE)
  cleaned <- sub("\\.\\d+$", "", cleaned)

  parsed_utc <- suppressWarnings(
    lubridate::parse_date_time(
      cleaned,
      orders = c("ymd HMS", "ymd HM", "ymd", "dby", "dbY"),
      truncated = 3,
      tz = "UTC"
    )
  )

  suppressWarnings(
    lubridate::with_tz(parsed_utc, "Australia/West")
  )
}


#' Check user inputs for earliest available DPIRD weather data
#'
#' @param .start_date A date object passed from another function
#'
#' @return invisible `NULL`, called for its side-effects
#' @autoglobal
#' @noRd

.check_earliest_available_dpird <- function(.station_code, .start_date, .f) {
  y <- readRDS(file = .f)[, c(1, 3)]
  y <- y[y$station_code %in% .station_code]

  if (.start_date < y$start) {
    stop(
      call. = FALSE,
      "You have requested weather data prior to the establishment of this ",
      "DPIRD weather station's establishment.  You might try the SILO data ",
      "for data from the BOM station network instead."
    )
  }
  return(invisible(NULL))
}


#' Sets the Column Orders Based on Time Step in the Data
#'
#' Sets column orders and adds date columns as necessary to match the data's
#'   time-step values.
#'
#' @param out A `data.table` containing results from the DPIRD Weather 2.0 API
#' @param checked_interval a time-step interval value that has been checked for
#'   validity
#'
#' @return a `data.table` with ordered columns
#' @autoglobal
#' @noRd

.set_col_orders <- function(.out, .checked_interval) {
  if (.checked_interval == "monthly") {
    .out[, date := lubridate::ym(sprintf("%s-%s", .out$period_year, .out$period_month))]

    data.table::setcolorder(
      .out,
      c(
        "station_code",
        "station_name",
        "longitude",
        "latitude",
        "period_year",
        "period_month",
        "date"
      )
    )

    .out[, period_day := NULL]
    .out[, period_hour := NULL]
    .out[, period_minute := NULL]
    data.table::setorder(x = .out, cols = "period_year", "period_month")
  } else if (.checked_interval == "daily") {
    .out[, date := lubridate::ymd(sprintf(
      "%s-%s-%s",
      .out$period_year,
      .out$period_month,
      .out$period_day
    ))]

    data.table::setcolorder(
      .out,
      c(
        "station_code",
        "station_name",
        "longitude",
        "latitude",
        "period_year",
        "period_month",
        "period_day",
        "date"
      )
    )

    .out[, period_minute := NULL]
    .out[, period_hour := NULL]
    data.table::setorder(x = .out,
                         cols = "period_year",
                         "period_month",
                         "period_day")
  } else if (.checked_interval == "hourly") {
    .out[, date := lubridate::ymd_h(
      sprintf(
        "%s-%s-%s-%s",
        .out$period_year,
        .out$period_month,
        .out$period_day,
        .out$period_hour
      ),
      tz = "Australia/West"
    )]

    data.table::setcolorder(
      .out,
      c(
        "station_code",
        "station_name",
        "longitude",
        "latitude",
        "period_year",
        "period_month",
        "period_day",
        "period_hour",
        "date"
      )
    )

    .out[, period_minute := NULL]
    data.table::setorder(x = .out,
                         cols = "period_year",
                         "period_month",
                         "period_day",
                         "period_hour")

  } else if (.checked_interval == "30min" ||
             .checked_interval == "15min") {
    .out[, date := lubridate::ymd_hm(
      sprintf(
        "%s-%s-%s-%s-%s",
        .out$period_year,
        .out$period_month,
        .out$period_day,
        .out$period_hour,
        .out$period_minute
      ),
      tz = "Australia/West"
    )]

    data.table::setcolorder(
      .out,
      c(
        "station_code",
        "station_name",
        "longitude",
        "latitude",
        "period_year",
        "period_month",
        "period_day",
        "period_hour",
        "period_minute",
        "date"
      )
    )
    data.table::setorder(
      x = .out,
      cols = "period_year",
      "period_month",
      "period_day",
      "period_hour",
      "period_minute"
    )
  } else {
    data.table::setcolorder(.out,
                            c(
                              "station_code",
                              "station_name",
                              "longitude",
                              "latitude",
                              "period_year"
                            ))
    .out[, period_month := NULL]
    .out[, period_day := NULL]
    .out[, period_hour := NULL]
    .out[, period_minute := NULL]
    data.table::setorder(x = .out, cols = "period_year")
  }
  return(.out)
}


#' Reshape wind summaries from long (per height rows) to wide (per height columns)
#'
#' Internal helper that takes the parsed summaries and, when wind data are
#' present with a `wind_height` column, returns a single row per period with
#' separate columns for each height (e.g. `_3m`, `_10m`).
#'
#' @param .out A `data.table` as returned from `.set_col_orders()`.
#'
#' @return A `data.table` with one row per station/period and wind variables
#'   suffixed by height where applicable.
#' @noRd
#' @keywords Internal
#' @autoglobal
.widen_wind_height_cols <- function(.out) {
  if (!"wind_height" %in% names(.out)) {
    return(.out)
  }

  wind_cols <- grep("^wind_", names(.out), value = TRUE)
  wind_cols <- setdiff(wind_cols, "wind_height")

  if (length(wind_cols) == 0L) {
    .out[, wind_height := NULL]
    return(.out)
  }

  # Columns that define the unique station/period key; these are kept as-is.
  group_cols <- setdiff(names(.out), c("wind_height", wind_cols))

  # Helper to generate a suffix label for a given height value.
  height_suffix <- function(h) {
    paste0(h, "m")
  }

  # Helper to create a vector of NAs matching the type of a template column.
  na_like <- function(template, n) {
    if (is.integer(template)) {
      return(rep(NA_integer_, n))
    }
    if (is.numeric(template)) {
      return(rep(NA_real_, n))
    }
    if (is.logical(template)) {
      return(rep(NA, n))
    }
    if (inherits(template, "Date")) {
      return(as.Date(rep(NA_character_, n)))
    }
    if (inherits(template, "POSIXct")) {
      return(as.POSIXct(rep(NA_character_, n), tz = "Australia/West"))
    }
    if (is.factor(template)) {
      return(factor(rep(NA_character_, n), levels = levels(template)))
    }
    # default to character
    return(rep(NA_character_, n))
  }

  heights <- sort(unique(.out$wind_height))
  heights <- heights[!is.na(heights)]

  base_dt <- unique(.out[, ..group_cols])

  if (length(heights) == 0L) {
    # No height metadata supplied; treat values as 3m and pad 10m as NA.
    dt_h <- .out[, c(group_cols, wind_cols), with = FALSE]
    if (nrow(dt_h) > nrow(unique(dt_h[, ..group_cols]))) {
      dt_h <- unique(dt_h, by = group_cols)
    }

    new_names <- paste0(wind_cols, "_", height_suffix(3L))
    data.table::setnames(dt_h, wind_cols, new_names)

    out_wide <- merge(
      base_dt,
      dt_h,
      by = group_cols,
      all.x = TRUE,
      sort = FALSE
    )

    for (col in wind_cols) {
      cname <- paste0(col, "_", height_suffix(10L))
      if (!cname %in% names(out_wide)) {
        template <- .out[[col]]
        out_wide[, (cname) := na_like(template, .N)]
      }
    }

    return(out_wide)
  }

  wide_list <- vector(mode = "list", length = length(heights))

  for (i in seq_along(heights)) {
    h <- heights[i]
    dt_h <- .out[wind_height == h, c(group_cols, wind_cols), with = FALSE]

    # In case of duplicated station/period rows per height, keep one.
    if (nrow(dt_h) > nrow(unique(dt_h[, ..group_cols]))) {
      dt_h <- unique(dt_h, by = group_cols)
    }

    new_names <- paste0(wind_cols, "_", height_suffix(h))
    data.table::setnames(dt_h, wind_cols, new_names)
    wide_list[[i]] <- dt_h
  }

  out_wide <- base_dt
  for (dt_h in wide_list) {
    out_wide <- merge(
      out_wide,
      dt_h,
      by = group_cols,
      all.x = TRUE,
      sort = FALSE
    )
  }

  # Ensure that standard 3m and 10m columns exist even if a station only has
  # one of the heights.
  standard_heights <- c(3L, 10L)
  for (h in standard_heights) {
    suffix <- height_suffix(h)
    for (col in wind_cols) {
      cname <- paste0(col, "_", suffix)
      if (!cname %in% names(out_wide)) {
        template <- .out[[col]]
        out_wide[, (cname) := na_like(template, .N)]
      }
    }
  }

  return(out_wide)
}
