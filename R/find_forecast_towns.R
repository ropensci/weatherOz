
#' Find nearest BOM forecast towns for a given location by lat and lon
#'
#' @param latitude  A `numeric` value of latitude in decimal degree (DD)
#'  format. By default, Canberra (approximately).
#' @param longitude A `numeric` value of longitude in decimal degree (DD)
#'  format. By default, Canberra (approximately).
#' @param distance_km A `numeric` value of the distance in kilometres from the
#'  `latitude` and `longitude` point beyond which values will not be returned.
#'
#' @examples
#'
#' # find forecast towns near Esperance, WA
#' find_forecast_towns(lat = -33.8614, lon = 121.8913)
#'
#' @return A \code{\link{data.table}} of all forecast towns (in this package)
#' sorted by distance from \var{latlon}, ascending.
#' @author Hugh Parsonage, \email{hugh.parsonage@@gmail.com} and
#'  James Goldie, \email{me@@jamesgoldie.dev} and Adam Sparks,
#'  \email{adam.sparks@@dpird.wa.gov.au}
#'
#' @family bomrang-ported
#'
#' @export find_forecast_towns

find_forecast_towns <-
  function(latitude = -35.3,
           longitude = 149.2,
           distance_km = 100) {

  # CRAN NOTE avoidance:
  AAC_codes <- lat <- lon <- distance <- NULL # nocov

  .check_lonlat(longitude = longitude, latitude = latitude)

  # Load JSON URL list
  load(system.file(
    "extdata",
    "AAC_codes.rda",
    package = "wrapique",
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

# Distance over a great circle. Reasonable approximation.
.haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # to radians
  lat1 <- lat1 * 0.01745
  lat2 <- lat2 * 0.01745
  lon1 <- lon1 * 0.01745
  lon2 <- lon2 * 0.01745

  delta_lat <- abs(lat1 - lat2)
  delta_lon <- abs(lon1 - lon2)

  # radius of earth
  12742 * asin(sqrt(`+`(
    (sin(delta_lat / 2)) ^ 2,
    cos(lat1) * cos(lat2) * (sin(delta_lon / 2)) ^ 2
  )))
}
