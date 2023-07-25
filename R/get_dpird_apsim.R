
#' Get DPIRD Summary Weather Data in the APSIM Format From the Weather 2.0 API
#'
#' Automates the retrieval and conversion of summary data from the
#'   \acronym{DPIRD} Weather 2.0 \acronym{API} to an \acronym{APSIM} .met file
#'   formatted weather data object.
#'
#' @param station_code A `character` string of the \acronym{DPIRD} station code
#'   for the station of interest.
#' @param start_date A `character` string or `Date` object representing the
#'   beginning of the range to query in the format \dQuote{yyyy-mm-dd}
#'   (ISO8601).  Data returned is inclusive of this date.
#' @param end_date A `character` string or `Date` object representing the end of
#'   the range query in the format  \dQuote{yyyy-mm-dd} (ISO8601).  Data
#'   returned is inclusive of this date.  Defaults to the current system date.
#' @param api_key A `character` string containing your \acronym{API} key from
#'   \acronym{DPIRD}, <https://www.agric.wa.gov.au/web-apis>, for the
#'   \acronym{DPIRD} Weather 2.0 \acronym{API}.
#' @param file A `character` string that provides the file or connection to
#'   write to.  Defaults to `NULL` with no file written, just an [apsimx] `met`
#'   class object in your \R session.
#'
#' @examples
#' \dontrun{
#' wd <- get_dpird_apsim(
#'   station_code = "BI",
#'   start_date = "20220401",
#'   end_date = "20221101",
#'   api_key = "your_api_key"
#' )
#' }
#'
#'
#' @author Adam H. Sparks, \email{adam.sparks@@dpird.wa.gov.au}
#'
#' @return An \CRANpkg{apsimx} object of class \sQuote{met} with attributes.
#'
#' @family DPIRD
#' @family data fetching
#' @family APSIM
#' @encoding UTF-8
#' @export

get_dpird_apsim <- function(station_code,
                            start_date,
                            end_date = Sys.Date(),
                            api_key,
                            file = NULL) {

  apsim <- get_dpird_summaries(
    station_code = station_code,
    start_date = start_date,
    end_date = end_date,
    interval = "daily",
    values = c(
      "airTemperatureMax",
      "airTemperatureMin",
      "rainfall",
      "relativeHumidityAvg",
      "solarExposure",
      "windAvgSpeed"
    ),
    api_key = api_key
  )

  apsim[, day := NULL]
  apsim[, day := lubridate::yday(apsim$date)]

  apsim <-
    apsim[, c(
      "year",
      "day",
      "solar_exposure",
      "air_temperature_max",
      "air_temperature_min",
      "rainfall",
      "relative_humidity_avg",
      "wind_avg"
    )]

  data.table::setnames(
    apsim,
    old = c(
      "solar_exposure",
      "air_temperature_max",
      "air_temperature_min",
      "rainfall",
      "relative_humidity_avg",
      "wind_avg"
    ),
    new = c("radn", "maxt", "mint", "rain", "rh", "windspeed")
  )

  data.table::setDF(apsim)

  units <- c("()", "()", "(MJ/m2/day)", "(oC)", "(oC)", "(mm)", "(%)", "(m/s)")
  comments <- sprintf("!data from DPIRD Weather 2.0 API. retrieved: %s",
                      Sys.time())

  attr(apsim, "filename") <- filename
  attr(apsim, "site") <-
    paste("site =", sub(".met", "", filename, fixed = TRUE))
  attr(apsim, "latitude") <- paste("latitude =", lonlat[2])
  attr(apsim, "longitude") <- paste("longitude =", lonlat[1])
  attr(apsim, "tav") <-
    paste("tav =", mean(colMeans(apsim[, c("maxt", "mint")],
                                 na.rm = TRUE), na.rm = TRUE))
  attr(apsim, "colnames") <- names(apsim)
  attr(apsim, "units") <- units
  attr(apsim, "comments") <- comments

  class(apsim) <- c("met", "data.frame")

  apsim <- amp_apsim_met(apsim)

  if (!isNULL(file)) {
    apsimx::write_apsim_met(apsim,
                            wrt.dir = dirname(file),
                            filename = basename(file))
  }
  return(invisible(apsim))
}

#' Calculates attribute amp for an object of class \sQuote{met}
#' This function rcalculates annual mean monthly amplitude for an object of
#'   class \sQuote{met} from \cranpkg{apsimx}
#'
#' @param met object of class \sQuote{met}
#' @return an object of class \sQuote{met} with a recalculation of annual
#'   amplitude in mean monthly temperature.
#' @author Fernando Miguez, \email{femiguez@@iastate.edu}
#' @noRd

amp_apsim_met <- function(met) {
  if (!inherits(met, "met"))
    stop("Object should be of class 'met", call. = FALSE)

  ## Step 1: create date
  date <-
    as.Date(paste(met$year, met$day, sep = "-"), format = "%Y-%j")
  ## Step 2: create month column
  mnth <- as.numeric(format(date, "%m"))

  met <-
    apsimx::add_column_apsim_met(
      met = met,
      value = mnth,
      name = "month",
      units = "()"
    )

  mtemp <- (met$maxt + met$mint) / 2
  met <-
    apsimx::add_column_apsim_met(
      met = met,
      value = mtemp,
      name = "mean.temp",
      units = "(oC)"
    )

  met.agg <- aggregate(mean.temp ~ mnth, data = met, FUN = mean)

  ans <- round(max(met.agg$mean.temp) - min(met.agg$mean.temp), 2)

  ## Clean up
  met <- apsimx::remove_column_apsim_met(met, "mean.temp")
  met <- apsimx::remove_column_apsim_met(met, "month")

  attr(met, "amp") <- paste("amp = ", ans)

  return(met)
}
