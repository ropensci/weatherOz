
#' Find nearest BOM forecast towns for a given location
#'
#' @param latlon A length-2 `numeric` vector of latitude and longitude in
#' decimal degree (DD) format. By default, Canberra (approximately).
#'
#' @examples
#'
#' # sweep for forecast towns near Esperance, WA
#' sweep_for_forecast_towns(latlon = c(-33.8614, 121.8913))
#'
#' @return A \code{\link{data.table}} of all forecast towns (in this package)
#' sorted by distance from \var{latlon}, ascending.
#' @author Hugh Parsonage, \email{hugh.parsonage@@gmail.com} and
#'  James Goldie, \email{me@@jamesgoldie.dev}
#'
#' @family bomrang
#'
#' @export sweep_for_forecast_towns

sweep_for_forecast_towns <- function(latlon = c(-35.3, 149.2)) {
  Lat <- latlon[1]
  Lon <- latlon[2]

  # CRAN NOTE avoidance:
  AAC_codes <- lat <- lon <- NULL # nocov

  # Load JSON URL list
  load(system.file("extdata",
                   "AAC_codes.rda",
                   package = "bomrang"))

  forecast_towns <- data.table::copy(AAC_codes)
  forecast_towns[, "distance" := .haversine_distance(Lat, Lon, lat, lon)] |>
    data.table::setorderv("distance")
  return(forecast_towns[])
}
