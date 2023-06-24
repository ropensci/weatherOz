
#' Find Nearest BOM Forecast Town
#'
#' For a given `latitude` and `longitude`, find the nearest town that the
#'   \acronym{BOM} provides a forecast for.
#'
#' @param latitude A `numeric` value of latitude in decimal degree (DD)
#'   format.  By default, Canberra (approximately).
#' @param longitude A `numeric` value of longitude in decimal degree (DD)
#'   format.  By default, Canberra (approximately).
#' @param distance_km A `numeric` value of the distance in kilometres from the
#'   `latitude` and `longitude` point beyond which values will not be returned.
#'
#' @examples
#'
#' # find forecast towns near Esperance, WA
#' find_forecast_towns(latitude = -33.8614, longitude = 121.8913)
#'
#' @return A [data.table::data.table] of all forecast towns (in this package)
#'   sorted by distance from \var{latlon}, ascending.
#' @author Hugh Parsonage, \email{hugh.parsonage@@gmail.com} and James Goldie,
#'   \email{me@@jamesgoldie.dev} and Adam H. Sparks
#'   \email{adam.sparks@@dpird.wa.gov.au}
#'
#' @family bomrang
#'
#' @export

find_forecast_towns <-
  function(latitude = -35.3,
           longitude = 149.2,
           distance_km = 100) {
    .check_lonlat(longitude = longitude, latitude = latitude)

    # Load JSON URL list
    load(system.file(
      "extdata",
      "AAC_codes.rda",
      package = "weatherOz",
      mustWork = TRUE
    ))

    forecast_towns <- data.table::copy(AAC_codes)
    forecast_towns[, "distance" := .haversine_distance(latitude,
                                                       longitude,
                                                       lat,
                                                       lon)] |>
      data.table::setorderv("distance")
    forecast_towns[distance %in%
                     forecast_towns[(distance <= distance_km)]$distance]
    return(forecast_towns[])
  }
