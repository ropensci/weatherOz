
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
#' @author Adam H. Sparks, \email{adam.sparks@@dpird.wa.gov.au}
#'
#' @return An \CRANpkg{apsimx} object of class \sQuote{met} with attributes.
#'
#' @family DPIRD
#' @family data fetching
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
      "evapotranspiration",
      "rainfall",
      "relativeHumidityAvg",
      "solarExposure",
      "windAvgSpeed"
    ),
    api_key = Sys.getenv("DPIRD_API_KEY")
  )

  pwr <- subset(as.data.frame(pwr), select = c("Yea", "DOY",
                                               "ALLSKY_SFC_SW_DWN",
                                               "T2M_MAX", "T2M_MIN",
                                               "PRECTOTCORR", "RH2M", "WS2M"))

  names(apsim) <-
    c("year", "day", "radn", "maxt", "mint", "rain", "rh", "windspeed")
  units <- c("()", "()", "(MJ/m2/day)", "(oC)", "(oC)", "(mm)", "(%)", "(m/s)")

  comments <- sprintf("!data from DPIRD Weather 2.0 API. retrieved: %s",
                      Sys.time())

  ## Calculating annual amplitude in mean monthly temperature

  attr(apsim, "filename") <- filename
  attr(apsim, "site") <- paste("site =", sub(".met", "", filename, fixed = TRUE))
  attr(apsim, "latitude") <- paste("latitude =", lonlat[2])
  attr(apsim, "longitude") <- paste("longitude =", lonlat[1])
  attr(apsim, "tav") <- paste("tav =", mean(colMeans(apsim[,c("maxt","mint")], na.rm=TRUE), na.rm=TRUE))
  attr(apsim, "colnames") <- names(apsim)
  attr(apsim, "units") <- units
  attr(apsim, "comments") <- comments
  ## No constants
  class(pwr) <- c("met", "data.frame")

  apsim <- apsimx::amp_apsim_met(apsim)

  if (filename != "noname.met") {
    apsimx::write_apsim_met(apsim, wrt.dir = wrt.dir, filename = filename)
  }
  return(invisible(apsim))
}
