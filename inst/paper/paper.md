---
title:
  'weatherOz: An API Client for Australian Weather and Climate Data Resources in R'
authors:
- affiliation: 1
  name: Rodrigo Pires
  orcid: 0000-0001-7384-6849
- affiliation: 1
  name: Anna Hepworth
  orcid: 0000-0003-0204-6347
- affiliation: 1
  name: Rebecca O'Leary
- affiliation: 2
  name: Jonathan Carroll
  orcid: 0000-0002-1404-5264
- affiliation: 3
  name: James Goldie
  orcid: 0000-0002-5024-6207
- affiliation: 4
  name: Dean Marchiori
  orcid: 0000-0002-3430-7225
- affiliation: 5
  name: Paul Melloy
  orcid: 0000-0003-4253-7167
- affiliation: 6
  name: Mark Padgam
  orcid: 0000-0003-2172-5265
- affiliation: 7
  name: Hugh Parsonage
  orcid: 0000-0003-4055-0835
- affiliation: 8
  name: Keith Pembleton
  orcid: 0000-0002-1896-4516
- affiliation: "1, 9, 10"
  name: Adam H. Sparks
  orcid: 0000-0002-0061-8359
date: "XX XXX 2024"
output:
  html_document: default
  pdf_document: default
bibliography: paper.bib
tags:
- Australia
- weather forecast
- meteorology
- climatology
- weather data
- R
- API client
- xml
- json
affiliations:
- index: 1
  name: Department of Primary Industries and Regional Development of Western Australia, Perth, Western Australia 6000, Australia
- index: 2
  name:
- index: 3
  name:
- index: 4
  name: Wave Data Labs, Wollongong, New South Wales, 2500, Australia
- index: 5
  name: The University of Queensland, School of Agriculture and Food Sustainability, Gatton, Queensland 4343, Australia
- index: 6
  name: University of Salzburg, Inter-Faculty Department of Geoinformatics, 5020 Salzburg, Austria
- index: 7
  name: Grattan Institute, Carlton, Victoria 3053, Australia
- index: 8
  name: University of Southern Queensland, School of Agricultural, Computational and Environmental Sciences, Toowoomba, Queensland 4350, Australia
- index: 9
  name: University of Southern Queensland, Centre for Crop Health, Toowoomba, Queensland 4350, Australia
- index: 10
  name: Curtin University, Centre for Crop and Disease Management, Curtin Biometry and Agricultural Data Analytics, Bentley, Western Australia 6102, Australia

---

# Summary

Researchers and policymakers use weather data in a variety of ways. 
Agriculture applications of the data are used in several types of models and decision support tools, to estimate leaf wetness, crop yield, crop growth stage, physiological stress or forecasting crop disease epidemics or insect pest population levels [@Venaelaeinen2002; @DeWolf2003; Sparks2017].
Other areas of use include mapping potential renewable energy, _e.g._, wind or solar potential for exploration purposes [@Ramachandra2007].
The data can also be used by decision makers for municipalities to help plan for extreme weather events, energy needs and other infrastructure [@Svensson2002; @Alcoforado2009].

{weatherOz} offers R users with a single interface to access Australian climate and weather data sources providing a standardised way of easily querying and retrieving Australian climate and weather data.

# Statement of need

Australian weather data availability is fragmented and difficult to easily access repeatedly or programmatically for use in research activities.
Previously the R package, {bomrang} [@Sparks2017], provided easy access to data available from the Australian Bureau of Meteorology (BoM) in R.
However, difficulties with accessing the data and restrictive polices caused us to archive this package.
Therefore, we created {weatherOz} to replace and at the same time, improve upon {bomrang} and provide R users with a package that allowed for easy of access to multiple sources of Australian climate and weather data while providing a unified approach to working with different data sources.
There are two sources of Australia-wide climate and weather station observations and data, BoM and Queensland Government's Scientific Information for Landowners (SILO) [@Jeffery2001] database, hosted by the Queensland Department of the Environment, Science and Innovation (DESI) and one source for Western Australia only weather station data, the Department of Primary Industries and Regional Development of Western Australia's (DPIRD) database.
BoM provides files through their public anonymous FTP server [@BoM2024] and data from both the SILO and DPIRD databases are available through APIs.

{weatherOz} provides access to data from the SILO database are made available under a Creative Commons Attribution 4.0 International (CC BY 4.0) licence including the Patched Point data available "from 1889 to yesterday" for approximately 8000 weather station locations located at post-offices, airports, police stations, national parks and private properties [@SILO2024], and Data Drill data, which are spatially interpolated data covering Australian land surfaces.
The DPIRD weather station network includes approximately 200 weather stations with data available from the year 2000 in Western Australia with the majority being located in the southwestern part of the state and are available in time-steps from minute to annual summaries with the default being daily values and is made available under a Creative Commons Attribution Licence 3.0 (CC BY 3.0 AU), though users must register for a free API key to use the resource. BoM data that are supported include forecasts, both précis and coastal, agriculture bulletins and satellite and radar imagery and are available under a Creative Commons Attribution (CC BY) licence.

# Features

The package is fully documented with a quick-start vignette that provides details about how to set up the API keys and go about frequently conducted tasks, _e.g._, fetching daily summary weather data from DPIRD and SILO as well as maps of station locations in the DPIRD and SILO station networks.
Additionally, each data source has a dedicated vignette that details how to fetch data from that source and includes greater detail about the functionality of the package.
Most users will likely use `get_patched_point()` (station data), `get_data_drill()` (spatially interpolated gridded data) or `get_dpird_summaries()` to retrieve summarised weather data values.
Most functions return a `data.table` [@Barrett2024], with the exception of some specialised functions discussed later.

Weather station metadata available through {weatherOz} is richer than what is available from SILO alone and includes geographic location as longitude, latitude and state, elevation, dates available, open or closed status and in the case of DPIRD stations, more detailed information including uptime and hardware details about the stations themselves is available through `get_stations_metadata()`.

Two specialised functions for data available from BoM, `get_radar_imagery()` and `get_satellite_imagery()`, return either {terra} [@Hijmans2024] or {stars} [@Pebesma2023] native objects or a {magick} object [@Oooms2024], respectively. 
Three other specialised functions are available for APSIM users providing {apsimx} ".met" objects of weather data from both DPIRD and SILO [@miguez2024] in an R session, _e.g._, `get_dpird_apsim()`, `get_data_drill_apsim()` and `get_patched_point_apsim()`.

{weatherOz} strives to unify the users experience, this includes the function arguments across the API requests as much as possible and also renaming the return object columns such that they follow a common naming scheme no matter which data source.
This does mean that the column names do not match the data source, but make working with data from the different APIs more manageable.

# Acknowledgements

Work on this package was funded as salary for the time of individuals involved in the writing of this software in part by the Department of Primary Industries and Regional Development of Western Australia, Adam H. Sparks and Rebecca O'Leary; Grains Research and Development Corporation research projects GRI2007-001RTX, Rodrigo Pires; and DAW2112-002RTX, Anna Hepworth; and Curtin University, Adam H. Sparks.

# References

