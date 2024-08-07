% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_nearby_stations.R
\name{find_nearby_stations}
\alias{find_nearby_stations}
\title{Find the Nearest Weather Stations to a Given Geographic Point or Known Station}
\usage{
find_nearby_stations(
  longitude = NULL,
  latitude = NULL,
  station_code = NULL,
  distance_km = 100,
  api_key = NULL,
  which_api = "silo",
  include_closed = FALSE
)
}
\arguments{
\item{longitude}{A \code{numeric} value of longitude in decimal degree (DD)
format.  Optional and defaults to \code{NULL}.  Required if \code{station_code} is
not provided.}

\item{latitude}{A \code{numeric} value of latitude in decimal degree (DD)
format.  Optional and defaults to \code{NULL}.  Required if \code{station_code} is
not provided.}

\item{station_code}{A \code{string} with the station code for the station of
interest.  Optional and defaults to \code{NULL}.  Required if \code{longitude} and
\code{latitude} are not provided.}

\item{distance_km}{A \code{numeric} value for distance to limit the search from
the station or location of interest.  Defaults to 100 km.}

\item{api_key}{A \code{character} string containing your \acronym{API} key from
\acronym{DPIRD}, \url{https://www.agric.wa.gov.au/web-apis}, for the
\acronym{DPIRD} Weather 2.0 \acronym{API}.  If left as \code{NULL}, defaults to
automatically detecting your key from your local .Renviron, .Rprofile or
similar.  Alternatively, you may directly provide your key as a string
here.  If nothing is provided, you will be prompted on how to set up your
\R session so that it is auto-detected.  Only used  when \var{which_api} is
\code{DPIRD} or \code{all}.}

\item{which_api}{A \code{string} value that indicates which \acronym{API} to use.
Defaults to \code{silo} only.  Valid values are \code{all}, for both \acronym{SILO}
(\acronym{BOM}) and \acronym{DPIRD} weather station networks; \code{silo} for
only stations in the \acronym{SILO} network; or \code{dpird} for stations in the
\acronym{DPIRD} network.}

\item{include_closed}{A \code{Boolean} value that indicates whether closed
stations in the \acronym{DPIRD} network should be included in the results.
Defaults to \code{FALSE} with closed stations not included.}
}
\value{
A \code{\link[data.table:data.table]{data.table::data.table()}} with \code{station_code}, \code{station_name},
\code{latitude}, \code{longitude}, \code{elev_m}, \code{state}, \code{owner}, and \code{distance}.
Data are sorted by increasing distance from station or location of
interest.
}
\description{
Find nearby weather stations given geographic coordinates or a station code
for both of the \acronym{DPIRD} and \acronym{SILO} weather station
networks.  Either a combination of \var{latitude} and \var{longitude} or
\var{station_code} must be provided.  A \acronym{DPIRD} \acronym{API} key
is only necessary to search for stations in the \acronym{DPIRD} network.
If you are not interested in \acronym{DPIRD} stations in Western Australia,
you may use this function to query only \acronym{SILO} stations for all of
Australia without using a key.
}
\note{
You can request your own \acronym{API} key from \acronym{DPIRD} for
free by filling out the form found at
\url{https://www.agric.wa.gov.au/web-apis}.
}
\examples{
\dontrun{

# Note that queries to the DPIRD API require you to have your own API key.

# Query WA only stations and return DPIRD's stations nearest to the
# Northam, WA station, "NO", returning stations with 50 km of this station

wa_stn <- find_nearby_stations(
  station_code = "NO",
  distance_km = 50,
  api_key = "your_api_key",
  which_api = "dpird"
)

# Query stations nearest DPIRD's Northam, WA station, "NO" and return both
# DPIRD and SILO/BOM stations within 50 km of this station.

wa_stn <- find_nearby_stations(
  station_code = "NO",
  distance_km = 50,
  api_key = "your_api_key",
  which_api = "all"
)

# Query Wagga Wagga BOM station finding stations within 200 km of it, note
# that it is not necessary to provide an `api_key` for SILO queries of
# nearby stations.

wagga_stn <- find_nearby_stations(
  latitude = -35.1583,
  longitude = 147.4575,
  distance_km = 200,
  which_api = "silo"
)
}

}
\seealso{
Other DPIRD: 
\code{\link{dpird_extreme_weather_values}},
\code{\link{dpird_minute_values}},
\code{\link{dpird_summary_values}},
\code{\link{find_stations_in}()},
\code{\link{get_dpird_apsim}()},
\code{\link{get_dpird_availability}()},
\code{\link{get_dpird_extremes}()},
\code{\link{get_dpird_minute}()},
\code{\link{get_dpird_summaries}()},
\code{\link{get_stations_metadata}()}

Other SILO: 
\code{\link{find_stations_in}()},
\code{\link{get_data_drill}()},
\code{\link{get_data_drill_apsim}()},
\code{\link{get_patched_point}()},
\code{\link{get_patched_point_apsim}()},
\code{\link{get_stations_metadata}()},
\code{\link{silo_daily_values}}

Other metadata: 
\code{\link{find_forecast_towns}()},
\code{\link{find_stations_in}()},
\code{\link{get_available_imagery}()},
\code{\link{get_available_radar}()},
\code{\link{get_dpird_availability}()},
\code{\link{get_stations_metadata}()}
}
\author{
Rodrigo Pires, \email{rodrigo.pires@dpird.wa.gov.au}, and Adam H.
Sparks, \email{adamhsparks@gmail.com}
}
\concept{DPIRD}
\concept{SILO}
\concept{metadata}
