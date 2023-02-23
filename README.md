# wrapique: a DPIRD and SILO API Client

*wrapique* aims to facilitate access and download of a range of climate data
from the Western Australian Department of Primary Industries and Regional 
Development (DPIRD) and the Scientific Information for Land Owners (SILO) API
endpoints. The package queries the APIs and returns data in your R session as a 
data frame. Observation data from the DPIRD's weather station network are
available via the [Science](https://www.agric.wa.gov.au/science-api-20) and [Weather](https://www.agric.wa.gov.au/weather-api-20) APIs. SILO data is 
obtained from the Long Paddock initiative (QLD Government) and are spatially  
and temporally complete, covering all Australia and few nearby islands 
(112°E to 154°E, 10°S to 44°S), with resolution 0.05° longitude by 0.05° 
latitude (approximately 5 km × 5 km). Visit the [SILO website](https://siloapi.longpaddock.qld.gov.au/silo/) 
for more details about how the data is prepared and which climate data are available.

Access to DPIRD API requires an API key. Apply for an API key by submitting the 
[DPIRD API registration form](https://www.agric.wa.gov.au/form/dpird-api-registration).
Access to the SILO API is conditioned to supplying a valid email address with
the user query. Follow the API Terms and Conditions for the [DPIRD](https://www.agric.wa.gov.au/apis/api-terms-and-conditions) and 
[SILO](https://siloapi.longpaddock.qld.gov.au/silo/about/access-data/) APIs.

Observation data from the DPIRD's weather station network is also available via
a [web interface](https://weather.agric.wa.gov.au). The data available is a 
mirror of the DPIRD Weather API endpoints. Rainfall estimates are also available
at virtual stations (i.e. where no observational data is present) and is sourced 
from the Doppler Radar service provided by the Bureau of Meteorology under license.

**Note that the data are not static and may be replaced with improved data. Also please note that SILO may be unavailable between 11am and 1pm (Brisbane time) each Wednesday and Thursday to allow for essential system maintenance.**
  
Please note that not all exposed endpoints of the DPIRD APIs have associated functions. 
Development is ongoing. While we are responsive to user requests, 
we don't make any commitments about speed of delivery.

## References
Stone, G., Dalla Pozza, R., Carter J., & McKeon, G. (2019). Long Paddock: 
climate risk and grazing information for Australian rangelands and grazing 
communities . The Rangeland Journal, 41, 225–232

## Installation instructions

You can install the development version of wrapique like so:

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
