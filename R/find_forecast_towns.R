
#' Find the Nearest Town with a BOM Forecast
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
#' @examples
#'
#' # find forecast towns near Esperance, WA
#' find_forecast_towns(longitude = 121.8913, latitude = -33.8614)
#'
#' @return A [data.table::data.table] of all forecast towns (in this package)
#'   sorted by distance from \var{latitude} and \var{longitude}, ascending.
#' @author Hugh Parsonage, \email{hugh.parsonage@@gmail.com} and James Goldie,
#'   \email{me@@jamesgoldie.dev} and Adam H. Sparks
#'   \email{adam.sparks@@dpird.wa.gov.au}
#'
#' @family BOM
#' @family metadata
#'
#' @export

find_forecast_towns <-
  function(longitude = 149.2,
           latitude = -35.3,
           distance_km = 100) {
    .check_lonlat(longitude = longitude, latitude = latitude)

    curl::curl_download(
      "ftp://ftp.bom.gov.au/anon/home/adfd/spatial/IDM00013.dbf",
      destfile = file.path(tempdir(), "AAC_codes.dbf"),
      mode = "wb",
      quiet = TRUE
    )

    forecast_towns <-
      data.table::data.table(foreign::read.dbf(file.path(tempdir(),
                                                         "AAC_codes.dbf"),
                                               as.is = TRUE))
    data.table::setnames(forecast_towns, names(forecast_towns),
                         tolower(names(forecast_towns)))
    data.table::setcolorder(forecast_towns, c(2:3, 7:9))
    data.table::setnames(forecast_towns, c(2, 5), c("town", "elev"))
    data.table::setkey(forecast_towns, "aac")

    forecast_towns[, "distance" := .haversine_distance(latitude,
                                                       longitude,
                                                       lat,
                                                       lon)] |>
      data.table::setorderv("distance")
    forecast_towns[distance %in%
                     forecast_towns[(distance <= distance_km)]$distance]
    data.table::setkey(forecast_towns, "aac")
    return(forecast_towns[, c("aac", "town", "lon", "lat", "elev", "distance")])
  }
