% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_stations_metadata.R
\name{get_stations_metadata}
\alias{get_stations_metadata}
\title{Get Weather Station Metadata for Both DPIRD and SILO Weather Stations}
\usage{
get_stations_metadata(
  station_code = NULL,
  station_name = NULL,
  which_api = "all",
  api_key = NULL,
  include_closed = FALSE,
  rich = FALSE
)
}
\arguments{
\item{station_code}{An optional value that should be provided as a single
\code{string} value or character \code{vector} of station codes for which to return
metadata.  If this or \code{station_name} are not provided, all station metadata
is returned by default.  If this and \code{station_name} are both provided, this
takes precedence and values corresponding to this input will be returned.}

\item{station_name}{An optional value that should be provided as either a
single \code{string} or character \code{vector} of station names for which to return
metadata.  Fuzzy matching is used, \emph{e.g.}, using
\code{c("brisbane", "melbourne")} will return rows for \dQuote{Brisbane},
\dQuote{Brisbane Aero}, \dQuote{Mt Brisbane},
\dQuote{City of Melbourne Bay}, \dQuote{Selbourne Kirnbrae},
\dQuote{Maroondah Weir Melbourne Water}, \dQuote{Melbourne Airport} and
\dQuote{Melbourne Botanical Gardens} \code{station_name} values.  If this or
\code{station_code} are not provided, all station metadata is returned by
default.  Using \code{station_code} will always override this argument if both
are provided.}

\item{which_api}{A \code{string} value that indicates which \acronym{API} to use.
Valid values are \code{all}, for both \acronym{SILO} (\acronym{BOM} data) and
\acronym{DPIRD} \acronym{API}s; \code{silo} for only stations from the
\acronym{SILO} \acronym{API} (\acronym{BOM} data); or \code{dpird} for stations
from the \acronym{DPIRD} Weather 2.0 \acronym{API}.  Defaults to \code{all}.}

\item{api_key}{A \code{character} string containing your \acronym{API} key from
\acronym{DPIRD}, \url{https://www.agric.wa.gov.au/web-apis}, for the
\acronym{DPIRD} Weather 2.0 \acronym{API}.  If left as \code{NULL}, defaults to
automatically detecting your key from your local .Renviron, .Rprofile or
similar.  Alternatively, you may directly provide your key as a string
here.  If nothing is provided, you will be prompted on how to set up your
\R session so that it is auto-detected.  Only used  when \var{which_api} is
\code{DPIRD} or \code{all}.}

\item{include_closed}{A \code{Boolean} string indicating whether to include closed
stations' metadata.  Use \code{TRUE} to include these.  Defaults to \code{FALSE}.}

\item{rich}{A \code{Boolean} string indicating whether to return rich information
about \acronym{DPIRD}'s weather station(s), this does not affect the
\acronym{SILO} stations' metadata, the variables for these observations
will be \code{NA}.  Defaults to \code{FALSE}.}
}
\value{
A \code{\link[data.table:data.table]{data.table::data.table()}} of \acronym{BOM} weather stations'
metadata for stations available from \acronym{SILO} and weather stations'
metadata for stations available from \acronym{DPIRD}'s Weather 2.0
\acronym{API} with the following columns sorted by \code{state} and
\code{station_name}.
\tabular{rl}{
\strong{station_code}:\tab Unique station code. \code{factor}\cr
\strong{station_name}:\tab Unique station name. \code{character}\cr
\strong{start}:\tab Date observations start. \code{date}\cr
\strong{end}:\tab Date observations end. \code{date}\cr
\strong{latitude}:\tab Latitude in decimal degrees. \code{numeric}\cr
\strong{longitude}:\tab Longitude in decimal degrees. \code{numeric}\cr
\strong{state}:\tab State in which the station is located. \code{character}\cr
\strong{elev_m}:\tab Station elevation in metres. \code{numeric}\cr
\strong{source}:\tab Organisation responsible for the data or station
maintenance. \code{character}\cr
\strong{include_closed}:\tab Station include_closed, one of \sQuote{open} or
\sQuote{closed}. \code{character}\cr
\strong{wmo}:\tab World Meteorological Organisation, (\acronym{WMO}), number
if applicable. \code{numeric}\cr
\strong{\code{rich} values}\tab\cr
\strong{capabilities}:\tab a list of the station's capabilities (data that it
records). \code{character}\cr
\strong{probe_height}:\tab temperature probe height in metres. \code{double}\cr
\strong{rain_gauge_height}\tab rain gauge height in metres. \code{double}\cr
\strong{wind_probe_heights}:\tab wind probe heights always 3 metres, although
some have 10 metre probes. \code{integer}\cr
}
}
\description{
Download the latest station locations and metadata for stations in the
\acronym{SILO} and \acronym{DPIRD} networks.  For \acronym{BOM} stations
that exist in \acronym{SILO}, but lack metadata from \acronym{BOM}, the
rows will exist to indicate that the station is in the \acronym{SILO} data
set, but there is no corresponding \acronym{BOM} metadata available.
}
\note{
For stations in the \acronym{SILO} \acronym{API}, \acronym{BOM} does
not report the exact date on which stations opened or closed, only the
year.  Therefore the \code{start} and \code{end} columns will indicate January 1 of
the year that a station opened or closed, whereas stations in the
\acronym{DPIRD} network have the date to the day.  For \acronym{BOM}
stations that are closed for the current year, this indicates that the
station closed sometime during the current year prior to the request being
made. \code{NA} in the current year indicates a station is still open.

There are discrepancies between the \acronym{BOM}'s official station
metadata, \emph{e.g.} longitude and latitude values and \acronym{SILO} metadata.
In these cases, the \acronym{BOM} metadata is used as it is considered to
be the authority on the stations' locations.

The station names are returned by both \acronym{API}s in full caps.
For purposes of cleaner graphs and maps where these data may be sued, this
function converts them to proper name formats/title case with the first
letter of every word capitalised excepting words like \dQuote{at} or
\dQuote{on} and keeps acronyms like \dQuote{AWS} or \dQuote{PIRSA} or state
abbreviations in the station names as all caps.
}
\examples{
\dontrun{
# fetch metadata for all stations available in {weatherOz}
get_stations_metadata(api_key = "your_api_key")
}

}
\references{
Station location and other metadata are sourced from the Australian Bureau of
Meteorology (\acronym{BOM}) webpage, Bureau of Meteorology Site Numbers:\cr
\url{http://www.bom.gov.au/climate/cdo/about/site-num.shtml} and
\url{http://www.bom.gov.au/climate/data/lists_by_element/stations.txt} and the
\acronym{DPIRD} Weather 2.0 \acronym{API}.
}
\seealso{
Other DPIRD: 
\code{\link{dpird_extreme_weather_values}},
\code{\link{dpird_minute_values}},
\code{\link{dpird_summary_values}},
\code{\link{find_nearby_stations}()},
\code{\link{find_stations_in}()},
\code{\link{get_dpird_apsim}()},
\code{\link{get_dpird_availability}()},
\code{\link{get_dpird_extremes}()},
\code{\link{get_dpird_minute}()},
\code{\link{get_dpird_summaries}()}

Other SILO: 
\code{\link{find_nearby_stations}()},
\code{\link{find_stations_in}()},
\code{\link{get_data_drill}()},
\code{\link{get_data_drill_apsim}()},
\code{\link{get_patched_point}()},
\code{\link{get_patched_point_apsim}()},
\code{\link{silo_daily_values}}

Other metadata: 
\code{\link{find_forecast_towns}()},
\code{\link{find_nearby_stations}()},
\code{\link{find_stations_in}()},
\code{\link{get_available_imagery}()},
\code{\link{get_available_radar}()},
\code{\link{get_dpird_availability}()}
}
\author{
Adam H. Sparks, \email{adamhsparks@gmail.com}
}
\concept{DPIRD}
\concept{SILO}
\concept{metadata}
