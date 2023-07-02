

#' Find the Nearest Weather Stations to a Given Geographic Point or Known Station
#'
#' Find nearby weather stations given geographic coordinates or a station code
#'   for both of the \acronym{DPIRD} and \acronym{SILO} weather station
#'   networks.  Either a combination of \var{latitude} and \var{longitude} or
#'   \var{station_code} must be provided.  A \acronym{DPIRD} \acronym{API} key
#'   is only necessary to search for stations in the \acronym{DPIRD} network.
#'   If you are not interested in \acronym{DPIRD} stations in Western Australia,
#'   you may use this function to query only \acronym{SILO} stations for all of
#'   Australia without using a key.
#'
#' @param longitude A `numeric` value of longitude in decimal degree (DD)
#'   format.  Optional and defaults to `NULL`.  Required if `station_code` is
#'   not provided.
#' @param latitude A `numeric` value of latitude in decimal degree (DD)
#'   format.  Optional and defaults to `NULL`.  Required if `station_code` is
#'   not provided.
#' @param station_code A `string` with the station code for the station of
#'   interest.  Optional and defaults to `NULL`.  Required if `longitude` and
#'   `latitude` are not provided.
#' @param distance_km A `numeric` value for distance to limit the search from
#'   the station or location of interest.  Defaults to 100 km.
#' @param api_key A `string` value that is the user's \acronym{API} key from
#'   \acronym{DPIRD} (see <https://www.agric.wa.gov.au/web-apis>).  Only used
#'   when \var{which_api} is `DPIRD` or `all`.
#' @param which_api A `string` value that indicates which \acronym{API} to use.
#'   Defaults to `silo` only. Valid values are `all`, for both \acronym{SILO}
#'   (\acronym{BOM}) and \acronym{DPIRD} weather station networks; `silo` for
#'   only stations in the \acronym{SILO} network; or `dpird` for stations in the
#'   \acronym{DPIRD} network.
#' @param include_closed A `Boolean` value that indicates whether closed
#'   stations in the \acronym{DPIRD} network should be included in the results.
#'   Defaults to `FALSE` with closed stations not included.
#'
#' @return a [data.table::data.table] with `station_code`, `station_name`,
#'   `latitude`, `longitude`, `elev_m`, `state`, `owner`, and `distance`.
#'   Data are sorted by increasing distance from station or location of
#'   interest.
#'
#' @note You can request your own \acronym{API} key from \acronym{DPIRD} for
#'   free by filling out the form found at
#'   <https://www.agric.wa.gov.au/web-apis>.
#'
#' @examples \dontrun{
#'
#' # Note that queries to the DPIRD API require you to have your own API key.
#'
#' # Query WA only stations and return DPIRD's stations nearest to the
#'   Northam, WA station, "NO", returning stations with 50 km of this station
#'
#' wa_stn <- find_nearby_stations(
#'   station_code = "NO",
#'   distance_km = 50,
#'   api_key = "your_api_key",
#'   which_api = "dpird"
#' )
#'
#' # Query stations nearest DPIRD's Northam, WA station, "NO" and return both
#'   DPIRD and SILO/BOM stations within 50 km of this station.
#'
#' wa_stn <- find_nearby_stations(
#'   station_code = "NO",
#'   distance_km = 50,
#'   api_key = "your_api_key",
#'   which_api = "all"
#' )
#'
#' # Query Wagga Wagga BOM station finding stations within 200 km of it.
#'
#' wagga_stn <- find_nearby_stations(
#'   latitude = -35.1583,
#'   longitude = 147.4575,
#'   distance_km = 200,
#'   which_api = "silo"
#' )
#' }
#'
#' @author Rodrigo Pires, \email{rodrigo.pires@@dpird.wa.gov.au} and Adam H.
#'   Sparks \email{adam.sparks@@dpird.wa.gov.au}
#'
#' @family DPIRD
#' @family SILO
#' @family metadata
#'
#' @export

find_nearby_stations <- function(longitude = NULL,
                                 latitude = NULL,
                                 station_code = NULL,
                                 distance_km = 100,
                                 api_key = NULL,
                                 which_api = "silo",
                                 include_closed = FALSE) {
  which_api <- .check_which_api(which_api)

  .check_location_params(
    .longitude = longitude,
    .latitude = latitude,
    .station_code = station_code
  )

  # get DPIRD stations only ----
  if (which_api == "dpird") {
    return(
      .get_dpird_stations(
        .station_code = station_code,
        .distance_km = distance_km,
        .longitude = longitude,
        .latitude = latitude,
        .api_key = api_key,
        .include_closed = include_closed
      )
    )
  }

  # get SILO stations only ----
  if (which_api == "silo") {
    return(
      .get_silo_stations(
        .station_code = station_code,
        .distance_km = distance_km,
        .longitude = longitude,
        .latitude = latitude
      )
    )
  }

  # get both API's weather stations ----
  if (!is.null(station_code)) {
    if (isFALSE(grepl("^\\d+$", station_code))) {
      dpird_out <- .get_dpird_stations(
        .station_code = station_code,
        .distance_km = distance_km,
        .longitude = longitude,
        .latitude = latitude,
        .api_key = api_key,
        .include_closed = include_closed
      )
    } else {
      silo_out <- .get_silo_stations(
        .station_code = station_code,
        .distance_km = distance_km,
        .longitude = longitude,
        .latitude = latitude
      )
    } # check which dataset was queried and use it to supply lat/lon to query
    # the other data set for nearest stations using the closest lat/lon value
    # from the other data set. This may need to be rethought.
    if (exists("dpird_out")) {
      silo_out <- .get_silo_stations(
        .station_code = NULL,
        .distance_km = distance_km,
        .longitude = dpird_out$longitude[1],
        .latitude = dpird_out$latitude[1]
      )
    } else {
      dpird_out <- .get_dpird_stations(
        .distance_km = distance_km,
        .longitude = silo_out$longitude[1],
        .latitude = silo_out$latitude[1],
        .api_key = api_key,
        .include_closed = include_closed
      )
    }
    # get both datasets and rbind
  } else {
    dpird_out <- .get_dpird_stations(
      .distance_km = distance_km,
      .longitude = longitude,
      .latitude = latitude,
      .api_key = api_key,
      .include_closed = include_closed
    )
    silo_out <- .get_silo_stations(
      .distance_km = distance_km,
      .longitude = longitude,
      .latitude = latitude
    )
  }
  out <- rbind(dpird_out, silo_out)
  data.table::setorder(out, distance_km)
  return(out)
}

#' Query and Return a data.table of DPIRD Stations Within a Given Radius
#'
#' @param .station_code A string identifying a station in DPIRD's network, which
#'  should be used as the centre point to determine stations that fall within
#'   `.distance_km`.
#' @param .distance_km A `numeric` value for the distance in kilometres in which
#'  to search for nearby stations from a given geographic location or known
#'   `.station_code`.
#' @param .latitude A `numeric` value (Decimal Degrees) passed from another
#'   function.
#' @param .longitude A `numeric` value (Decimal Degrees) passed from another
#'   function.
#' @param .api_key A `string` value that is the user's \acronym{API} key from
#'   \acronym{DPIRD} (see <https://www.agric.wa.gov.au/web-apis>).
#' @param .include_closed A `Boolean` value indicating whether to include
#'   closed stations or not. Defaults to `FALSE`, not including closed stations.
#'
#' @noRd
.get_dpird_stations <- function(.station_code,
                                .distance_km,
                                .longitude,
                                .latitude,
                                .api_key,
                                .include_closed) {
  # Error if api key not provided
  if (missing(.api_key)) {
    stop(
      "A valid DPIRD API key must be provided, please visit\n",
      "<https://www.agric.wa.gov.au/web-apis> to request one.\n",
      call. = FALSE
    )
  }

  query_list <- list(api_key = .api_key,
                     api_group = "all",
                     include_closed = .include_closed)

  # if a station_code is provided, get the metadata for it which has lat/lon
  # then extract and pass that back to the API to find nearest stations to that
  # point
  if (is.null(.latitude) && is.null(.longitude)) {
    station_meta <- .query_dpird_api(.end_point = .station_code,
                                     .query_list = query_list,
                                     .limit = 1000)
    .latitude <-
      jsonlite::fromJSON(station_meta[[1]]$parse("UTF8"))$data$latitude
    .longitude <-
      jsonlite::fromJSON(station_meta[[1]]$parse("UTF8"))$data$longitude
  }

  # now either using a supplied lat/lon or the gathered values get the nearest
  # stations

  query_list <- c(
    query_list,
    longitude = .longitude,
    latitude = .latitude,
    radius = .distance_km
  )

  out <- .query_dpird_api(.end_point = "nearby",
                          .query_list = query_list,
                          .limit = 1000)

  out <-
    data.table::data.table(jsonlite::fromJSON(out[[1]]$parse("UTF8"))$collection)

  .set_snake_case_names(out)
  out[, station_code := as.factor(station_code)]
  data.table::setkey(out, "station_code")

  if (is.null(nrow(out)) && !is.null(.station_code)) {
    message(
      "No DPIRD stations found around a radius of < ",
      .distance_km,
      "\n",
      " km from station ",
      .station_code,
      "."
    )
  } else {
    # Warn user if there are no stations within the input radius and return data
    if (is.null(nrow(out))) {
      message(
        "No DPIRD stations found around a radius of <",
        .distance_km,
        " km\n",
        " from coordinates ",
        .latitude,
        " and ",
        .longitude,
        " (lat/lon).\n"
      )
    }
  }

  out[, state := "WA"]
  data.table::setnames(out,
                       old = c("altitude", "owner_code", "distance"),
                       new = c("elev_m", "owner", "distance_km"))
  out <-
    out[, c("station_code",
            "station_name",
            "longitude",
            "latitude",
            "state",
            "elev_m",
            "owner",
            "distance_km")]

  return(data.table::setorder(x = out, cols = distance_km))
}

#' Query and Return a data.table of Stations Available in SILO Within a Given Radius
#'
#' @param .station_code A string identifying a BOM station in SILO which should
#' be used as the centre point to determine stations that fall within
#'  `.distance_km`
#' @param .distance_km A `numeric` value for the distance in kilometres in which
#'  to search for nearby stations from a given geographic location or known
#'  `.station_code`.
#'
#' Uses SILO's cgi-bin web interface to query and download a text file of
#' BOM weather stations in SILO within 10000 km of Finke, NT (near the Lambert
#' centre of Australia). Stations are returned in order of distance from the
#' Finke Post Office.
#'
#' @noRd
.get_silo_stations <-
  function(.station_code,
           .distance_km,
           .longitude,
           .latitude) {
    if (is.null(.station_code)) {
      out <- .query_silo_api(
        .station_code = "015526",
        .radius = 10000,
        .format = "near",
        .dataset = "PatchedPoint"
      )

      out[, "distance_km" := .haversine_distance(
        lat1 = latitude,
        lon1 = longitude,
        lat2 = .latitude,
        lon2 = .longitude
      )] |>
        data.table::setorderv("distance_km")

      out <-
        out[distance_km %in% out[(distance_km <= .distance_km)]$distance_km]

      if (nrow(out) == 0L)
        message(
          "No SILO stations found around a radius of < ",
          .distance_km,
          " km\n for the given `station_code`",
          .station_code,
          "."
        )
    } else {
      out <-
        .query_silo_api(
          .station_code = .station_code,
          .radius = .distance_km,
          .format = "near",
          .sortby = "dist",
          .dataset = "PatchedPoint"
        )
      if (nrow(out) == 0L)
        message(
          "No SILO stations found around a radius of < ",
          .distance_km,
          " km\n for the given coordinates",
          .longitude,
          " and ",
          .latitude,
          "."
        )
    }
    return(data.table::setorder(x = out, cols = distance_km))
  }
