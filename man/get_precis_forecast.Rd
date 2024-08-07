% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_precis_forecast.R
\name{get_precis_forecast}
\alias{get_precis_forecast}
\title{Get a BOM Daily Précis Forecast}
\usage{
get_precis_forecast(state = "AUS")
}
\arguments{
\item{state}{Australian state or territory as full name or postal code.
Fuzzy string matching via \code{\link[base:agrep]{base::agrep()}} is done.  Defaults to \code{AUS}
returning all state bulletins, see Details for more.}
}
\value{
A \code{\link[data.table:data.table]{data.table::data.table()}} of an Australia \acronym{BOM} précis seven day
forecasts for \acronym{BOM} selected towns.
}
\description{
Fetch nicely formatted daily précis forecast from the \acronym{BOM}, which
contains seven-day town forecasts for a specified state or territory.
Ported from \pkg{bomrang}.
}
\details{
Allowed state and territory postal codes, only one state per request
or all using 'AUS'.
\describe{
\item{AUS}{Australia, returns forecast for all states, NT and ACT}
\item{ACT}{Australian Capital Territory (will return NSW)}
\item{NSW}{New South Wales}
\item{NT}{Northern Territory}
\item{QLD}{Queensland}
\item{SA}{South Australia}
\item{TAS}{Tasmania}
\item{VIC}{Victoria}
\item{WA}{Western Australia}
}
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}

# get the short forecast for Western Australia
get_precis_forecast(state = "WA")
\dontshow{\}) # examplesIf}
}
\references{
Forecast data come from Australian Bureau of Meteorology (\acronym{BOM})
Weather Data Services \cr
\url{http://www.bom.gov.au/catalogue/data-feeds.shtml}

Location data and other metadata for towns come from the \acronym{BOM}
anonymous \acronym{FTP} server with spatial data \cr
\url{ftp://ftp.bom.gov.au/anon/home/adfd/spatial/}, specifically the
\acronym{DBF} file portion of a shapefile, \cr
\url{ftp://ftp.bom.gov.au/anon/home/adfd/spatial/IDM00013.dbf}.
}
\seealso{
\link{parse_precis_forecast}

Other BOM: 
\code{\link{find_forecast_towns}()},
\code{\link{get_ag_bulletin}()},
\code{\link{get_available_imagery}()},
\code{\link{get_available_radar}()},
\code{\link{get_coastal_forecast}()},
\code{\link{get_radar_imagery}()},
\code{\link{get_satellite_imagery}()},
\code{\link{parse_ag_bulletin}()},
\code{\link{parse_coastal_forecast}()},
\code{\link{parse_precis_forecast}()}

Other data fetching: 
\code{\link{get_ag_bulletin}()},
\code{\link{get_coastal_forecast}()},
\code{\link{get_data_drill}()},
\code{\link{get_data_drill_apsim}()},
\code{\link{get_dpird_apsim}()},
\code{\link{get_dpird_extremes}()},
\code{\link{get_dpird_minute}()},
\code{\link{get_dpird_summaries}()},
\code{\link{get_patched_point}()},
\code{\link{get_patched_point_apsim}()},
\code{\link{get_radar_imagery}()},
\code{\link{get_satellite_imagery}()}
}
\author{
Adam H. Sparks, \email{adamhsparks@gmail.com}, Keith
Pembleton, \email{keith.pembleton@usq.edu.au}, and Paul Melloy,
\email{paul@melloy.com.au}
}
\concept{BOM}
\concept{data fetching}
