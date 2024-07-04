
#' Find the Nearest Town With a BOM Forecast
#'
#' For a given `latitude` and `longitude`, find the nearest town that the
#'   \acronym{BOM} provides a forecast for.
#'
#' @param longitude A `numeric` value of longitude in decimal degree (DD)
#'   format.  By default, Canberra (approximately).
#' @param latitude A `numeric` value of latitude in decimal degree (DD)
#'   format.  By default, Canberra (approximately).
#' @param distance_km A `numeric` value of the distance in kilometres from the
#'   `latitude` and `longitude` point beyond which values will not be returned.
#'
#' @examplesIf interactive()
#'
#' # find forecast towns near Esperance, WA
#' find_forecast_towns(longitude = 121.8913, latitude = -33.8614)
#'
#' @return A [data.table::data.table()] of all forecast towns (in this package) sorted by
#'   distance from \var{latitude} and \var{longitude}, ascending.
#' @author Hugh Parsonage, \email{hugh.parsonage@@gmail.com}, and James Goldie,
#'   \email{me@@jamesgoldie.dev}, and Adam H. Sparks,
#'   \email{adamhsparks@@gmail.com}
#'
#' @family BOM
#' @family metadata
#' @autoglobal
#' @export

find_forecast_towns <-
  function(longitude = 149.2,
           latitude = -35.3,
           distance_km = 100) {
    lonlat <- .check_lonlat(longitude = longitude, latitude = latitude)

    user_longitude <- lonlat["longitude"]
    user_latitude <- lonlat["latitude"]

    file_dbf <- file.path(tempdir(), "AAC_codes.dbf")

    op <- options(timeout = 600L)
    on.exit(options(op))

    on.exit(unlink(file_dbf))
    utils::download.file(
      "ftp://ftp.bom.gov.au/anon/home/adfd/spatial/IDM00013.dbf",
      destfile = file_dbf,
      mode = "wb",
      quiet = TRUE
    )

    forecast_towns <-
      data.table::data.table(foreign::read.dbf(file_dbf, as.is = TRUE))
    data.table::setnames(forecast_towns, names(forecast_towns),
                         tolower(names(forecast_towns)))
    data.table::setcolorder(forecast_towns, c(2:3, 7:9))
    data.table::setnames(forecast_towns,
                         old = c("pt_name", "lat", "lon", "elevation"),
                         new = c("town", "latitude", "longitude", "elev_m"))
    forecast_towns <- forecast_towns[, c("aac",
                                         "town",
                                         "longitude",
                                         "latitude",
                                         "elev_m")]
    data.table::setkey(forecast_towns, "aac")

    forecast_towns[, "distance" := .haversine_distance(
      lat1 = latitude,
      lon1 = longitude,
      lat2 = user_latitude,
      lon2 = user_longitude
    )] |>
      data.table::setorderv("distance")

    forecast_towns[, distance := round(distance, 1)]
    out <- forecast_towns[distance %in%
                     forecast_towns[(distance <= distance_km)]$distance]
    data.table::setkey(out, "aac")
    return(out[])
  }
