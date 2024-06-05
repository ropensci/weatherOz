
#' A List of DPIRD Minute Weather Data Values
#'
#' A [vector] object containing 12 items representing valid values to supply
#'   to `get_dpird_minute()`'s \var{values} argument taken from the
#'   documentation for the \acronym{DPIRD} Weather 2.0 \acronym{API}.
#'
#' @format A [vector] object of 12 items.
#'
#' @keywords datasets
#'
#' @family DPIRD
#' @family data
#'
#' @source <https://www.agric.wa.gov.au/weather-api-20>
#'
"dpird_minute_values"

#' A List of DPIRD Extreme Weather Data Values
#'
#' A [vector] object containing 57 items representing valid values to supply
#'   to `get_dpird_extremes()`'s \var{values} argument taken from the
#'   documentation for the \acronym{DPIRD} Weather 2.0 \acronym{API}.
#'
#' @format A [vector] object of 57 items.
#'
#' @keywords datasets
#'
#' @family DPIRD
#' @family data
#'
#' @source <https://www.agric.wa.gov.au/weather-api-20>
#'
"dpird_extreme_weather_values"


#' A List of DPIRD Summary Weather Data Values
#'
#' A [vector] object containing 75 items representing valid values to supply
#'   to `get_dpird_summary()`'s \var{values} argument taken from the
#'   documentation for the \acronym{DPIRD} Weather 2.0 \acronym{API}.
#'
#' @format A [vector] object of 75 items.
#'
#' @keywords datasets
#'
#' @family DPIRD
#' @family data
#'
#' @source <https://www.agric.wa.gov.au/weather-api-20>
#'
"dpird_summary_values"


#' A List of SILO Daily Weather Values
#'
#' A [vector] object containing 18 items representing valid values to supply
#'   to `get_patched_point()` and `get_data_drill()`'s \var{values} argument
#'   taken from the documentation for the \acronym{SILO} \acronym{API}.
#'
#' @format A [vector] object of 57 items.
#'
#' @keywords datasets
#'
#' @family SILO
#' @family data
#'
#' @source <https://www.longpaddock.qld.gov.au/silo/about/climate-variables/>
#'
"silo_daily_values"

#' Western Australia Southwest Agriculture Region Geospatial Polygon
#'
#' An \CRANpkg{sf} object of the the WA South West Agricultural Region.
#'
#' The zone managed for intensive agricultural activities in South-Western
#' Australia.  Also known as the South West Agricultural Area or Clearing Line.
#' This zone defines the easternmost extent of land cleared for agricultural
#' purposes.
#'
#' @section Base data sets:
#' Western Australian Land Information Authority -
#' Captured from photographic interpretation of best available orthophotography
#' at date of capture, dates range between 2007 and 2010.
#'
#' @section Scale of capture:
#' 1:20,000
#'
#' @section Coordinate Reference System:
#' EPSG:4326 - WGS 84 -- WGS84 - World Geodetic System 1984, used in GPS
#'   <https://epsg.io/4326>
#'
#' @name south_west_agriculture_region
#'
#' @docType data
#'
#' @format An [sf::sf()] polygon object
#'
#' @source \href{https://catalogue.data.wa.gov.au/dataset/south-west-agricultural-region-dpird-008}{Western Australia Department of Primary Industries and Regional
#'   Development}
#'   under a \href{https://creativecommons.org/licenses/by/4.0/}{Creative Commons Attribution 4.0 Licence}
#'
"south_west_agricultural_region"
