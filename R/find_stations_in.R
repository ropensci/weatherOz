
#' Find Stations Within a Geospatially Defined Geographic Area of Interest
#'
#' Given an \CRANpkg{sf} polygon or a bounding box as a vector with the minimum
#'   and maximum longitude and latitude values, find \acronym{DPIRD} or
#'   \acronym{BOM} stations in the \acronym{SILO} network that fall within that
#'   defined area or the station nearest the centroid of the area of interest.
#'
#' @param x One of two types of object:
#'   * A `Vector` A four-digit vector defining a bounding box of the area
#'   of interest in this order, \sQuote{xmin}, \sQuote{ymin}, \sQuote{xmax},
#'   \sQuote{ymax}, or
#'   * An object of class \CRANpkg{sf} defining the area of interest.
#' @param centroid `Boolean` A value of `TRUE` or `FALSE` indicating whether
#'   you want the centroid only to be used to find the nearest station to the
#'   centre of the area of interest.  If \dQuote{n} polygons are supplied,
#'   \dQuote{n} stations are returned.  Defaults to `FALSE` with all stations
#'   within the area of interest returned.
#' @param api_key A `character` string containing your \acronym{API} key from
#'   \acronym{DPIRD}, <https://www.dpird.wa.gov.au/online-tools/apis/>, for the
#'   \acronym{DPIRD} Weather 2.0 \acronym{API}.  If left as `NULL`, defaults to
#'   automatically detecting your key from your local .Renviron, .Rprofile or
#'   similar.  Alternatively, you may directly provide your key as a string
#'   here.  If nothing is provided, you will be prompted on how to set up your
#'   \R session so that it is auto-detected.  Only used  when \var{which_api} is
#'   `DPIRD` or `all`.
#' @param which_api A `string` value that indicates which \acronym{API} to use.
#'   Defaults to `silo` only.  Valid values are `all`, for both \acronym{SILO}
#'   (\acronym{BOM}) and \acronym{DPIRD} weather station networks; `silo` for
#'   only stations in the \acronym{SILO} network; or `dpird` for stations in the
#'   \acronym{DPIRD} network.
#' @param include_closed A `Boolean` value that indicates whether closed
#'   stations in the \acronym{DPIRD} network should be included in the results.
#'   Defaults to `FALSE` with closed stations not included.
#' @param crs A `string` value that provides the coordinate reference system,
#'   AKA, "projection" to be used for the point extraction.  Defaults to
#'   GDA 2020, EPSG:7844.  **NOTE** This will override any `crs` value that your
#'.  `polygon` provides unless you specify it again here, *e.g.*,
#'    `crs = sf::st_crs(polygon_object_name)`.
#'
#' @return a \CRANpkg{data.table} object of weather station(s) within the
#'   defined area of interest in an unprojected format, EPSG:4326, WGS 84 --
#'   WGS84 - World Geodetic System 1984, used in \acronym{GPS} format.
#'
#' @examplesIf interactive()
#'
#' # using a (generous) bounding box for Melbourne, Vic using only the SILO API
#' # for BOM stations, so no API key is needed.
#'
#' bbox <- find_stations_in(
#'   x = c(144.470215, -38.160476, 145.612793, -37.622934),
#'   which_api = "SILO",
#'   include_closed = TRUE
#' )
#' bbox
#'
#' # Use the same bounding box but only find a single station nearest
#'# the centroid using only the SILO API for BOM stations
#'
#' centroid <- find_stations_in(
#'   x = c(144.470215, -38.160476, 145.612793, -37.622934),
#'   which_api = "SILO",
#'   include_closed = TRUE,
#'   centroid = TRUE
#' )
#' centroid
#'
#' # Use the `south_west_agricultural_region` data to fetch stations only in the
#' # south-western portion of WA and plot it with {ggplot2} showing open/closed
#' # stations just to be sure they're inside the area of interest.
#'
#' # As this is in WA, we can use the DPIRD network, so we need our API key.
#' # Using the `south_west_agricultural_region` {sf} object provided.
#'
#' sw_wa <- find_stations_in(
#'   x = south_west_agricultural_region,
#'   api_key = "your_api_key",
#'   include_closed = TRUE
#' )
#'
#' sw_wa
#'
#' @family DPIRD
#' @family SILO
#' @family metadata
#'
#' @export

find_stations_in <- function(x,
                             centroid = FALSE,
                             api_key = NULL,
                             which_api = "all",
                             include_closed = FALSE,
                             crs = "EPSG:7844"
                             ) {

  .check_not_example_api_key(api_key)
  if (which_api == "all" || which_api == "dpird" && is.null(api_key)) {
    api_key <- get_key(service = "DPIRD")
    .is_valid_dpird_api_key(api_key)
  }

  if (missing(x)) {
    stop("You must provide an `sf` object or bounding box vector of 4 numbers.")
  }


  # convert bbox or named places to {sf} polygons
  if (is.numeric(x)) {
    if (x[[1]] > x[[3]] || x[[2]] > x[[4]]) {
      stop("Check that your lon lat values are in the proper order ",
           "and that you have provided four corners.")
    }

    # ensure values are in Australia
    .check_lonlat(longitude = x[[1]], latitude = x[[2]])
    .check_lonlat(longitude = x[[3]], latitude = x[[4]])

    x <- sf::st_as_sf(
      data.table::data.table("x" = x[c(1, 3)], "y" = x[c(2, 4)]),
      coords = c("x", "y"),
      crs = crs
    )
    x <- sf::st_as_sfc(sf::st_bbox(x), crs = crs)
  }

  # ensure that the CRS is uniform from here on
  area <- sf::st_transform(x = x, crs = crs)

  # fetch the station metadata, all the work is done locally for this fn
  stn_metadat <- get_stations_metadata(api_key = api_key,
                                       which_api = which_api,
                                       include_closed = include_closed)

  # ensure that there are no missing lon/lat values (can't find these stations
  # on a map anyway, so...)
  stn_metadat <- stn_metadat[!is.na(stn_metadat$longitude), ]
  stn_metadat <- stn_metadat[!is.na(stn_metadat$latitude), ]
  stn_metadat_sf <- sf::st_as_sf(stn_metadat,
                                 coords = c("longitude", "latitude"),
                                 crs = "EPSG:4326")
  stn_metadat_sf <- sf::st_transform(stn_metadat_sf, crs = sf::st_crs(area))

  sf::st_agr(stn_metadat_sf) <- "constant"
  if (inherits(area, "sf")) {
    sf::st_agr(area) <- "constant"
  }

  if (isFALSE(centroid)) {
    intersect <- sf::st_intersection(x = stn_metadat_sf, y = area)
  } else {
    intersect <- stn_metadat_sf[
      sf::st_nearest_feature(x = sf::st_as_sf(sf::st_centroid(area)),
                             y = stn_metadat_sf), ]

  }
  intersect <- sf::st_drop_geometry(intersect[, 1:9])
  return(stn_metadat[intersect, on = c("station_code",
                                       "station_name",
                                       "start",
                                       "end",
                                       "state",
                                       "elev_m",
                                       "source",
                                       "status",
                                       "wmo")])
}
