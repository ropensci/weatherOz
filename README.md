# wrapique

WA weather API query package -- for getting and parsing weather data relevant to DPIRD projects from relevant APIs. 

As of April 2021, this package has been spun off from the Western Australian Department of Primary Industries and Regional development (DPIRD) developed 'FoliarDisease' package so as to separate out the two distinct sets of functionality.

This package allows the user to query one of three DPIRD provided APIs (weather, science, radar) or the SILO API provided by the Queensland government, as well as to parse that data in ways commonly of interest to Agricultural scientists. It is not intended to be comprehensive -- for example, not all exposed endpoints of the APIs have associated functions. Development is ongoing. While we are responsive to user requests, we don't make any commitments about speed of delivery. 

As well as summaries, some basic mapping functionality is included. This produces maps in the style seen at https://www.agric.wa.gov.au/dry-seasons-and-drought/seasonal-climate-information in the soil water and potential yield tabs. 

## The APIs

More detailed information about the APIs is available at: 

* DPIRD Science - https://www.agric.wa.gov.au/science-api-20
* DPIRD Weather - https://www.agric.wa.gov.au/weather-api-20
* DPIRD Radar - https://www.agric.wa.gov.au/radar-api-20
* SILO - https://www.longpaddock.qld.gov.au/silo/api-documentation/guide/

Note  that the first three of these links are to the old `agric` domain; the 
actual APIs point at the `dpird` domain, but the documentation is at `agric.wa.gov.au`. 

DPIRD APIs require an API key. These can be requested via the form at
https://www.agric.wa.gov.au/form/dpird-api-registration. The SILO API does not
require an API key, but does require that the user provide a valid email address
as part of their request.

It is also of relevance here that the database behind DPIRD Science includes a
mirror of some of the SILO data. Prior to 2021, this was predominantly for
locations in the South West Land Division. Going forward, weather data will also
be available for rangelands locations. As with the SILO data set, this contains
interpolated data for periods where stations were not operating.
