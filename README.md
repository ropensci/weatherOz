  <!-- badges: start -->
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
  <!-- badges: end -->

# {wrapique}: An API Client for Australian Weather and Climate Data Resources

{wrapique} aims to facilitate access and download of a range of weather and climate data for Australia.
Data are sourced from from the Western Australian Department of Primary Industries and Regional Development (DPIRD) and the Scientific Information for Land Owners (SILO) API endpoints and the Australian Government Bureau of Meteorology (BOM).
The package queries the APIs and returns data as a data frame or radar and satellite imagery in your R session.
Observation data from the DPIRD's weather station network are available via the [Science](https://www.agric.wa.gov.au/science-api-20) and [Weather](https://www.agric.wa.gov.au/weather-api-20) APIs.
SILO data is obtained from the Long Paddock initiative (Jeffery et al. 2001) and are spatially and temporally complete, covering all Australia and few nearby islands (112°E to 154°E, 10°S to 44°S), with resolution 0.05° longitude by 0.05° latitude (approximately 5 km × 5 km).
Visit the [SILO website](https://siloapi.longpaddock.qld.gov.au/silo/) for more details about how the data is prepared and which climate data are available.

Access to DPIRD API requires an API key.
Apply for an API key by submitting the [DPIRD API registration form](https://www.agric.wa.gov.au/form/dpird-api-registration).
Access to the SILO API is conditioned to supplying a valid email address with the user query.
Follow the API Terms and Conditions for the [DPIRD](https://www.agric.wa.gov.au/apis/api-terms-and-conditions) and [SILO](https://siloapi.longpaddock.qld.gov.au/silo/about/access-data/) APIs.

Observation data from the DPIRD's weather station network is also available via a [web interface](https://weather.agric.wa.gov.au).
The data available is a mirror of the DPIRD Weather API endpoints.
Rainfall estimates are also available at virtual stations (_i.e._, where no observational data is present) and is sourced from the Doppler radar service provided by the Bureau of Meteorology under license.

## Installation instructions

You can install the development version of {wrapique} like so:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
}
remotes::install_git("https://git.agric.wa.gov.au/r/wrapique.git")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```

## Notes on Data and API Endpoints

Note that most of the data are not static and may be replaced with improved data.
Also please note that SILO may be unavailable between 11am and 1pm (Brisbane time) each Wednesday and Thursday to allow for essential system maintenance.

Please also note that not all exposed endpoints of the DPIRD APIs have associated functions.
Development is ongoing.
While we are responsive to user requests, we don't make any commitments about speed of delivery.

## References

Jeffrey, S.J., Carter, J.O., Moodie, K.B. and Beswick, A.R. (2001). Using spatial interpolation to construct a comprehensive archive of Australian climate data, _Environmental Modelling and Software_, Vol 16/4, pp 309-330. <https://doi.org/10.1016/S1364-8152(01)00008-1)>.
