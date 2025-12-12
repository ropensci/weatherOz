
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {weatherOz}: An API Client for Australian Weather and Climate Data Resources <img src="man/figures/logo.png" align="right"/>

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![DOI](https://zenodo.org/badge/613750527.svg)](https://zenodo.org/badge/latestdoi/613750527)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/598_status.svg)](https://github.com/ropensci/software-review/issues/598)
[![status](https://joss.theoj.org/papers/10.21105/joss.06717/status.svg)](https://joss.theoj.org/papers/10.21105/joss.06717)
[![R-CMD-check](https://github.com/ropensci/weatherOz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci/weatherOz/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ropensci/weatherOz/graph/badge.svg)](https://app.codecov.io/gh/ropensci/weatherOz)
[![CRAN
status](https://www.r-pkg.org/badges/version/weatherOz)](https://CRAN.R-project.org/package=weatherOz)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/weatherOz)](https://CRAN.R-project.org/package=weatherOz)
<!-- badges: end -->

{weatherOz} facilitates access to and download of weather and climate
data for Australia from Australian and international data sources. Data
are sourced from the [Western Australia Department of Primary Industries
and Regional Development
(DPIRD)](https://www.dpird.wa.gov.au/online-tools/apis/), the
[Scientific Information for Land Owners (SILO)
API](https://www.longpaddock.qld.gov.au/silo/), the [Norwegian
Meteorological Institute](https://www.met.no/) and the Australian
Government Bureau of Meteorology’s (BOM) [FTP
server](http://www.bom.gov.au/catalogue/anon-ftp.shtml).

The package queries the APIs or an FTP server and returns data as a data
frame or radar and satellite imagery in your R session. Observation data
from DPIRD’s weather station network are available via the [Weather
2.0](https://www.dpird.wa.gov.au/online-tools/apis/) Open API
initiative. SILO data is available from Queensland’s Long Paddock
initiative (Jeffery *et al.* 2001) and are spatially and temporally
complete, covering all Australia and few nearby islands (112 to 154
degrees longitude, -10 to -44 degrees latitude), with resolution 0.05°
longitude by 0.05° latitude (approximately 5 km × 5 km). Visit the [SILO
website](https://www.longpaddock.qld.gov.au/silo/) for more details
about how the data is prepared and which climate data are available.
Forecast data with up to 9 days ahead are available from the [MET
Weather API Locationforecast
endpoint](https://api.met.no/weatherapi/locationforecast/2.0/documentation)
for any location in Australia. Agriculture bulletins, radar imagery,
satellite imagery and seven-day forecasts are available from the Bureau
of Meteorology (BOM) via an anonymous FTP server.

Access to DPIRD API requires an API key. Apply for an API key by
submitting the [DPIRD API registration
form](https://www.dpird.wa.gov.au/forms/dpird-api-registration/). Access
to the SILO API is conditioned to supplying a valid email address with
the user query. Follow the API Terms and Conditions for the
[DPIRD](https://www.dpird.wa.gov.au/online-tools/apis/api-terms-and-conditions/),
[SILO](https://www.longpaddock.qld.gov.au/silo/api-documentation/) and
[METNO](https://api.met.no/doc/TermsOfService) APIs.

Observation data from the DPIRD’s weather station network is also
available via a [web interface](https://weather.agric.wa.gov.au). The
data available is a mirror of the DPIRD Weather 2.0 API endpoints.
Rainfall estimates are also available at virtual stations (*i.e.*, where
no observational data is present) and is sourced from the Doppler radar
service provided by the Australian Government Bureau of Meteorology
(BOM) under license.

## Installation instructions

You can install the stable version of {weatherOz} from
[CRAN](https://cran.r-project.org/) like so:

``` r
install.packages("weatherOz")
```

You can install the development version of {weatherOz} like so:

``` r
install.packages("weatherOz", repos = "https://ropensci.r-universe.dev")
```

## A Note on API Keys

The examples in this README assume that you have stored your API key in
your .Renviron file. {weatherOz} will prompt you to set up your API keys
automatically if you haven’t. For more information on the preferred
method for setting up your API keys, see [Chapter
8](https://rstats.wtf/r-startup.html#renviron) in “What They Forgot to
Teach You About R” by Bryan *et al.* for more on storing details in your
.Renviron if you are unfamiliar.

To get a DPIRD API key, you can use `get_key()` and it will direct you
to the form to request a key and provides instructions for setting it up
so that it’s available in your R session and {weatherOz} will
automatically find it. If you have already set up an API key, this will
return that value for you.

``` r
get_key(service = "DPIRD")
```

You only need to provide an e-mail address for the SILO and MET Weather
APIs. Using `get_key()` will provide you with instructions on what
format to use in your .Renviron so that {weatherOz} will auto-recognise
it and if you have already set up an API key, this will return that
value for you.

``` r
get_key(service = "SILO")
get_key(service = "METNO")
```

Note that you do not need to do this separately, any function requiring
an API key will prompt you if you don’t have one set.

## Example 1

Source wind and erosion conditions for daily time interval from the
DPIRD Weather 2.0 API.

``` r
library(weatherOz)

wd <- get_dpird_summaries(
  station_code = "BI",
  start_date = "20220501",
  end_date = "20220502",
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
#> 2:                         0                         <NA>          13.06
#> 3:                         7          2022-05-02 15:01:00          15.57
#> 4:                         7          2022-05-02 15:01:00          17.70
#>    wind_height wind_max_direction_compass_point wind_max_direction_degrees
#>          <int>                           <char>                      <int>
#> 1:           3                              SSW                        200
#> 2:           3                              SSW                        205
#> 3:          10                              SSW                        194
#> 4:          10                              SSW                        193
#>    wind_max_speed       wind_max_time
#>             <num>              <POSc>
#> 1:          31.82 2022-05-01 17:28:00
#> 2:          38.52 2022-05-02 16:07:00
#> 3:          34.88 2022-05-01 17:34:00
#> 4:          40.10 2022-05-02 16:31:00
```

## Example 2

Source data from latitude and longitude coordinates anywhere in
Australia (interpolated/gridded data - SILO API) for Southwood, QLD for
max and min temperature and rainfall.

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
  )
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
#> 1:      9.8              25 254.5 m 2025-11-05      0.9              25
#> 2:     11.7              25 254.5 m 2025-11-05      0.0              25
#> 3:      7.8              25 254.5 m 2025-11-05      0.0              25
#> 4:     10.6              25 254.5 m 2025-11-05      0.0              25
#> 5:     13.3              25 254.5 m 2025-11-05      0.0              25
#> 6:     14.7              25 254.5 m 2025-11-05      1.7              25
```

## Example 3

Source 9-day forecast data from the Norwegian Meteorological Institute
Weather API for Perth, WA.

``` r
library(weatherOz)

perth_forecast <- get_metno_forecast(
  latitude = -31.95,
  longitude = 115.86,
  format = "complete"
)

# Inspect the structure
names(perth_forecast)
#> [1] "data"     "raw"      "metadata"

# View the first few rows of hourly data
head(perth_forecast$data)
#>                   time air_temperature relative_humidity wind_speed
#>                 <POSc>           <num>             <num>      <num>
#> 1: 2025-11-05 17:00:00            22.0              51.0        2.6
#> 2: 2025-11-05 18:00:00            23.4              49.0        1.3
#> 3: 2025-11-05 19:00:00            23.8              48.9        1.9
#> 4: 2025-11-05 20:00:00            23.9              47.8        2.7
#> 5: 2025-11-05 21:00:00            24.7              47.1        3.8
#> 6: 2025-11-05 22:00:00            22.4              54.6        4.2
#>    wind_from_direction cloud_area_fraction air_pressure_at_sea_level
#>                  <num>               <num>                     <num>
#> 1:                63.1                10.9                    1004.8
#> 2:               347.0                38.3                    1004.4
#> 3:               305.7                26.6                    1003.8
#> 4:               267.8                19.5                    1003.1
#> 5:               251.8                63.3                    1002.6
#> 6:               183.4               100.0                    1003.0
#>    precipitation_amount      symbol_code
#>                   <num>           <char>
#> 1:                  0.0     clearsky_day
#> 2:                  0.0 partlycloudy_day
#> 3:                  0.0         fair_day
#> 4:                  0.0         fair_day
#> 5:                  0.4  rainshowers_day
#> 6:                  0.0           cloudy

# Check metadata
perth_forecast$metadata$request$longitude
#> [1] 115.86

perth_forecast$metadata$request$latitude
#> [1] -31.95

perth_forecast$metadata$request$format
#> [1] "complete"

perth_forecast$metadata$status_code
#> [1] 200

perth_forecast$metadata$expires
#> [1] "2025-11-05 09:46:08 AWST"
```

## Notes on Data and API Endpoints

Note that most of the data are not static and may be replaced with
improved data. Also please note that SILO may be unavailable between
11am and 1pm (Brisbane time) each Wednesday and Thursday to allow for
essential system maintenance.

The MET Weather API is an interface to a selection of data produced by
MET Norway. The data are freely available for use under a Creative
Commons license, including commercial use. See also [separate conditions
for use of the service](https://api.met.no/doc/TermsOfService).

Please also note that not all exposed endpoints of the DPIRD APIs have
associated functions. Development is ongoing.While we are responsive to
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
