  <!-- badges: start -->
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
  [![tic](https://github.com/DPIRD-FSI/weatherOz/workflows/tic/badge.svg?branch=main)](https://github.com/DPIRD-FSI/weatherOz/actions)
  [![Codecov test coverage](https://codecov.io/gh/DPIRD-FSI/weatherOz/branch/main/graph/badge.svg)](https://app.codecov.io/gh/DPIRD-FSI/weatherOz?branch=main)
  <!-- badges: end -->

# {weatherOz}: An API Client for Australian Weather and Climate Data Resources

{weatherOz} aims to facilitate access and download of a range of weather and climate data for Australia.
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

You can install the development version of {weatherOz} like so:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
}
remotes::install_github("DPIRD-FSI/weatherOz", build_vignettes = TRUE)
```

## Example 1

Source wind and erosion conditions for daily time interval from the DPIRD Weather API.

```r
# define start and end date
start_date <- "2022-05-01"
end_date <- "2022-05-02"

output <- get_dpird_summaries(
            site = "BI",
            first = start_date,
            last = end_date,
            api_key = mykey,
            interval = "hourly",
            which_vars = c("wind", "erosion"))
```

## Example 2

Source data from latitude and longitude coordinates (gridded data - SILO API) Southwood, QLD in the 'apsim' format.

```r
wd <- get_silo(latitude = -27.85,
               longitude = 150.05,
               first = "20221001",
               last = "20221201",
               data_format = "apsim",
               email = "your@email")
```

## Notes on Data and API Endpoints

Note that most of the data are not static and may be replaced with improved data.
Also please note that SILO may be unavailable between 11am and 1pm (Brisbane time) each Wednesday and Thursday to allow for essential system maintenance.

Please also note that not all exposed endpoints of the DPIRD APIs have associated functions.
Development is ongoing.
While we are responsive to user requests, we don't make any commitments about speed of delivery.

## References

Jeffrey, S.J., Carter, J.O., Moodie, K.B. and Beswick, A.R. (2001). Using spatial interpolation to construct a comprehensive archive of Australian climate data, _Environmental Modelling and Software_, Vol 16/4, pp 309-330. <https://doi.org/10.1016/S1364-8152(01)00008-1)>.

## Code of Conduct
 
Please note that the weatherOz project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
