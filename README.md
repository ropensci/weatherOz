
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {weatherOz}: An API Client for Australian Weather and Climate Data Resources <img src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![DOI](https://zenodo.org/badge/613750527.svg)](https://zenodo.org/badge/latestdoi/613750527)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/598_status.svg)](https://github.com/ropensci/software-review/issues/598)
[![R-CMD-check](https://github.com/DPIRD-FSI/weatherOz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DPIRD-FSI/weatherOz/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/DPIRD-FSI/weatherOz/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/DPIRD-FSI/weatherOz/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

{weatherOz} aims to facilitate access and download weather and climate
data for Australia from Australian data sources. Data are sourced from
from the Western Australian Department of Primary Industries and
Regional Development (DPIRD) and the Scientific Information for Land
Owners (SILO) API endpoints and the Australian Government Bureau of
Meteorology’s (BOM) FTP server.

The package queries the APIs or an FTP server and returns data as a data
frame or radar and satellite imagery in your R session. Observation data
from DPIRD’s weather station network are available via the [Weather
2.0](https://www.agric.wa.gov.au/weather-api-20) Open API initiative.
SILO data is available from Queensland’s Long Paddock initiative
(Jeffery *et al.* 2001) and are spatially and temporally complete,
covering all Australia and few nearby islands (112 to 154 degrees
longitude, -10 to -44 degrees latitude), with resolution 0.05° longitude
by 0.05° latitude (approximately 5 km × 5 km). Visit the [SILO
website](https://siloapi.longpaddock.qld.gov.au/silo/) for more details
about how the data is prepared and which climate data are available.
Agriculture bulletins, radar imagery, satellite imagery and seven-day
forecasts are available from the Bureau of Meteorology (BOM) via an
anonymous FTP server.

Access to DPIRD API requires an API key. Apply for an API key by
submitting the [DPIRD API registration
form](https://www.agric.wa.gov.au/form/dpird-api-registration). Access
to the SILO API is conditioned to supplying a valid email address with
the user query. Follow the API Terms and Conditions for the
[DPIRD](https://www.agric.wa.gov.au/apis/api-terms-and-conditions) and
[SILO](https://siloapi.longpaddock.qld.gov.au/silo/about/access-data/)
APIs.

Observation data from the DPIRD’s weather station network is also
available via a [web interface](https://weather.agric.wa.gov.au). The
data available is a mirror of the DPIRD Weather 2.0 API endpoints.
Rainfall estimates are also available at virtual stations (*i.e.*, where
no observational data is present) and is sourced from the Doppler radar
service provided by the Bureau of Meteorology under license.

## Installation instructions

You can install the development version of {weatherOz} like so:

``` r
install.packages("weatherOz", repos = "https://ropensci.r-universe.dev")
```

## A Note on API Keys

The examples in this README assume that you have stored your API key in
your .Renviron file. See [Chapter
8](https://rstats.wtf/r-startup.html#renviron) in “What They Forgot to
Teach You About R” by Bryan *et al.* for more on storing details in your
.Renviron if you are unfamiliar.

## Example 1

Source wind and erosion conditions for daily time interval from the
DPIRD Weather 2.0 API.

``` r
library(weatherOz)
#> 
#> Attaching package: 'weatherOz'
#> The following object is masked from 'package:graphics':
#> 
#>     plot
#> The following object is masked from 'package:base':
#> 
#>     plot

wd <- get_dpird_summaries(
  station_code = "BI",
  start_date = "20220501",
  end_date = "20220502",
  api_key = Sys.getenv("DPIRD_API_KEY"),
  interval = "daily",
  values = c(
    "wind",
    "erosionCondition",
    "erosionConditionMinutes",
    "erosionConditionStartTime"
  )
)

wd
#> Key: <station_code>
#>    station_code station_name longitude latitude  year month   day       date
#>          <fctr>       <char>     <num>    <num> <int> <int> <int>     <Date>
#> 1:           BI        Binnu  114.6958  -28.051  2022     5     1 2022-05-01
#> 2:           BI        Binnu  114.6958  -28.051  2022     5     1 2022-05-01
#> 3:           BI        Binnu  114.6958  -28.051  2022     5     2 2022-05-02
#> 4:           BI        Binnu  114.6958  -28.051  2022     5     2 2022-05-02
#>    erosion_condition_minutes erosion_condition_start_time wind_avg_speed
#>                        <int>                       <POSc>          <num>
#> 1:                         0                         <NA>          10.85
#> 2:                         0                         <NA>          15.57
#> 3:                         7          2022-05-02 15:01:00          13.06
#> 4:                         7          2022-05-02 15:01:00          17.70
#>    wind_height wind_max_direction_compass_point wind_max_direction_degrees
#>          <int>                           <char>                      <int>
#> 1:           3                              SSW                        200
#> 2:          10                              SSW                        194
#> 3:           3                              SSW                        205
#> 4:          10                              SSW                        193
#>    wind_max_speed       wind_max_time
#>             <num>              <POSc>
#> 1:          31.82 2022-05-01 17:28:00
#> 2:          34.88 2022-05-01 17:34:00
#> 3:          38.52 2022-05-02 16:07:00
#> 4:          40.10 2022-05-02 16:31:00
```

## Example 2

Source data from latitude and longitude coordinates (gridded data - SILO
API) Southwood, QLD for max and min temperature and rainfall.

``` r
library(weatherOz)

wd <- get_data_drill(
  latitude = -27.85,
  longitude = 150.05,
  start_date = "20221001",
  end_date = "20221201",
  values = c(
    "max_temp",
    "min_temp",
    "rain"
  ),
  api_key = Sys.getenv("SILO_API_KEY")
)

head(wd)
#>    longitude latitude  year month   day       date air_tmax air_tmax_source
#>        <num>    <num> <num> <num> <int>     <Date>    <num>           <int>
#> 1:    150.05   -27.85  2022    10     1 2022-10-01     25.1              25
#> 2:    150.05   -27.85  2022    10     2 2022-10-02     22.6              25
#> 3:    150.05   -27.85  2022    10     3 2022-10-03     24.0              25
#> 4:    150.05   -27.85  2022    10     4 2022-10-04     25.7              25
#> 5:    150.05   -27.85  2022    10     5 2022-10-05     22.3              25
#> 6:    150.05   -27.85  2022    10     6 2022-10-06     24.4              25
#>    air_tmin air_tmin_source  elev_m  extracted rainfall rainfall_source
#>       <num>           <int>  <char>     <Date>    <num>           <int>
#> 1:      9.8              25 254.5 m 2024-03-09      0.9              25
#> 2:     11.7              25 254.5 m 2024-03-09      0.0              25
#> 3:      7.8              25 254.5 m 2024-03-09      0.0              25
#> 4:     10.6              25 254.5 m 2024-03-09      0.0              25
#> 5:     13.3              25 254.5 m 2024-03-09      0.0              25
#> 6:     14.7              25 254.5 m 2024-03-09      1.8              25
```

## Notes on Data and API Endpoints

Note that most of the data are not static and may be replaced with
improved data. Also please note that SILO may be unavailable between
11am and 1pm (Brisbane time) each Wednesday and Thursday to allow for
essential system maintenance.

Please also note that not all exposed endpoints of the DPIRD APIs have
associated functions. Development is ongoing. While we are responsive to
user requests, we don’t make any commitments about speed of delivery.

## References

Jeffrey, S.J., Carter, J.O., Moodie, K.B. and Beswick, A.R. (2001).
Using spatial interpolation to construct a comprehensive archive of
Australian climate data, *Environmental Modelling and Software*, Vol
16/4, pp 309-330. <https://doi.org/10.1016/S1364-8152(01)00008-1>.

## Code of Conduct

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.
