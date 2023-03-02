Create Databases of BOM Station Locations
================

<STYLE type='text/css' scoped>
PRE.fansi SPAN {padding-top: .25em; padding-bottom: .25em};
</STYLE>
<PRE class="fansi fansi-message"><CODE>## ── <span style='font-weight: bold;'>Attaching core tidyverse packages</span> ──────────────────────── tidyverse 2.0.0 ──
## <span style='color: #00BB00;'>✔</span> <span style='color: #0000BB;'>dplyr    </span> 1.1.0     <span style='color: #00BB00;'>✔</span> <span style='color: #0000BB;'>readr    </span> 2.1.4
## <span style='color: #00BB00;'>✔</span> <span style='color: #0000BB;'>forcats  </span> 1.0.0     <span style='color: #00BB00;'>✔</span> <span style='color: #0000BB;'>stringr  </span> 1.5.0
## <span style='color: #00BB00;'>✔</span> <span style='color: #0000BB;'>ggplot2  </span> 3.4.1     <span style='color: #00BB00;'>✔</span> <span style='color: #0000BB;'>tibble   </span> 3.1.8
## <span style='color: #00BB00;'>✔</span> <span style='color: #0000BB;'>lubridate</span> 1.9.2     <span style='color: #00BB00;'>✔</span> <span style='color: #0000BB;'>tidyr    </span> 1.3.0
## <span style='color: #00BB00;'>✔</span> <span style='color: #0000BB;'>purrr    </span> 1.0.1     
## ── <span style='font-weight: bold;'>Conflicts</span> ────────────────────────────────────────── tidyverse_conflicts() ──
## <span style='color: #BB0000;'>✖</span> <span style='color: #0000BB;'>dplyr</span>::<span style='color: #00BB00;'>filter()</span> masks <span style='color: #0000BB;'>stats</span>::filter()
## <span style='color: #BB0000;'>✖</span> <span style='color: #0000BB;'>dplyr</span>::<span style='color: #00BB00;'>lag()</span>    masks <span style='color: #0000BB;'>stats</span>::lag()
## <span style='color: #00BBBB;'>ℹ</span> Use the <a href='http://conflicted.r-lib.org/'>conflicted package</a> to force all conflicts to become errors
</CODE></PRE>

This document provides details on methods used to create the database of
BOM JSON files for stations and corresponding metadata, *e.g.*,
latitude, longitude (which are more detailed than what is in the JSON
file), start, end, elevation, etc.

Refer to this BOM page for more reference,
<http://reg.bom.gov.au/catalogue/anon-ftp.shtml>.

\## Product code definitions

\### States

- IDD - NT

- IDN - NSW/ACT

- IDQ - Qld

- IDS - SA

- IDT - Tas/Antarctica (distinguished by the product number)

- IDV - Vic

- IDW - WA

### Product code numbers

- 60701 - coastal observations (duplicated in 60801)

- 60801 - State weather observations excluding Canberra

- 60803 - Antarctica weather observations

- 60901 - capital city weather observations (duplicated in 60801)

- 60903 - Canberra area weather observations

## Get station metadata

The station metadata are downloaded from a zip file linked from the
“[Bureau of Meteorology Site
Numbers](http://www.bom.gov.au/climate/cdo/about/site-num.shtml)”
website. The zip file may be directly downloaded, [file of site
details](ftp://ftp.bom.gov.au/anon2/home/ncc/metadata/sitelists/stations.zip).

``` r
# This file is a pseudo-fixed width file. Line five contains the headers at
# fixed widths which are coded in the read_table() call.
# The last seven lines contain other information that we don't want.
# For some reason, reading it directly from the BOM website does not work, so
# we use curl to fetch it first and then import it from the R tempdir(), trim
# the extra off the ends and write it back out to import using `read_fwf()`.

curl::curl_download(
  url = "ftp://ftp.bom.gov.au/anon2/home/ncc/metadata/sitelists/stations.zip",
  destfile = file.path(tempdir(), "stations.zip"),
  mode = "wb",
  quiet = TRUE
)

bom_stations_lines <- read_lines(unzip(file.path(tempdir(), "stations.zip")))
keep <- length(bom_stations_lines) - 7
bom_stations_lines <- bom_stations_lines[1:keep]
write_lines(x = bom_stations_lines, file = file.path(tempdir(), "stations.txt"))

bom_stations_raw <-
  read_fwf(
    file = file.path(tempdir(), "stations.txt"),
    col_positions = fwf_empty(
      file = file.path(tempdir(), "stations.txt"),
      skip = 4,
      n = 1000,
      col_names = c(
        "site",
        "dist",
        "name",
        "start",
        "end",
        "lat",
        "lon",
        "NULL1",
        "state",
        "elev",
        "bar_ht",
        "wmo"
      )
    ),
    skip = 4,
    col_types = c(
      site = "character",
      dist = "character",
      name = "character",
      start = readr::col_integer(),
      end = readr::col_integer(),
      lat = readr::col_double(),
      lon = readr::col_double(),
      NULL1 = "character",
      state = "character",
      elev = readr::col_double(),
      bar_ht = readr::col_double(),
      wmo = readr::col_integer()
    )
  )

bom_stations_raw[bom_stations_raw == "...."] <- NA
bom_stations_raw[bom_stations_raw == ".."] <- NA

# remove extra columns for source of location
bom_stations_raw <- bom_stations_raw[, -8]

# add current year to stations that are still active
bom_stations_raw$end <- as.numeric(bom_stations_raw$end)

bom_stations_raw["end"][is.na(bom_stations_raw["end"])] <-
  as.integer(format(Sys.Date(), "%Y"))
```

## Check station locations

Occasionally the stations are listed in the wrong location, *e.g.*,
Alice Springs Airport in SA. Perform quality check to ensure that the
station locations are accurate based on the lat/lon values.

``` r
`%notin%`  <- function(x, table) {
  # Same as !(x %in% table)
  match(x, table, nomatch = 0L) == 0L
}

data.table::setDT(bom_stations_raw)
latlon2state <- function(lat, lon) {
  ASGS.foyer::latlon2SA(lat,
                        lon,
                        to = "STE",
                        yr = "2016",
                        return = "v")
}

bom_stations_raw %>%
  .[lon > -50, state_from_latlon := latlon2state(lat, lon)] %>%
  .[state_from_latlon == "New South Wales", actual_state := "NSW"] %>%
  .[state_from_latlon == "Victoria", actual_state := "VIC"] %>%
  .[state_from_latlon == "Queensland", actual_state := "QLD"] %>%
  .[state_from_latlon == "South Australia", actual_state := "SA"] %>%
  .[state_from_latlon == "Western Australia", actual_state := "WA"] %>%
  .[state_from_latlon == "Tasmania", actual_state := "TAS"] %>%
  .[state_from_latlon == "Australian Capital Territory",
    actual_state := "ACT"] %>%
  .[state_from_latlon == "Northern Territory", actual_state := "NT"] %>%
  .[actual_state != state & state %notin% c("ANT", "ISL"),
    state := actual_state] %>%
  .[, actual_state := NULL]
```

    ## Loading required package: sp

``` r
data.table::setDF(bom_stations_raw)
```

## Create state codes

Use the state values extracted from `ASGS.foyer` to set state codes from
BOM rather than the sometimes incorrect `state` column from BOM.

BOM state codes are as follows:

- IDD - NT,

- IDN - NSW/ACT,

- IDQ - Qld,

- IDS - SA,

- IDT - Tas/Antarctica,

- IDV - Vic, and

- IDW - WA

``` r
bom_stations_raw$state_code <- NA
bom_stations_raw$state_code[bom_stations_raw$state == "WA"] <- "W"
bom_stations_raw$state_code[bom_stations_raw$state == "QLD"] <- "Q"
bom_stations_raw$state_code[bom_stations_raw$state == "VIC"] <- "V"
bom_stations_raw$state_code[bom_stations_raw$state == "NT"] <- "D"
bom_stations_raw$state_code[bom_stations_raw$state == "TAS" |
                              bom_stations_raw$state == "ANT"] <-
  "T"
bom_stations_raw$state_code[bom_stations_raw$state == "NSW"] <- "N"
bom_stations_raw$state_code[bom_stations_raw$state == "SA"] <- "S"
```

## Save data

### Station location database for get_ag_bulletin()

First, rename columns and drop a few that aren’t necessary for the ag
bulletin information. Filter for only stations currently reporting
values. Then pad the `site` field with 0 to match the data in the XML
file that holds the ag bulletin information. Lastly, create the
databases for use in {wrapique}.

``` r
new_stations_site_list <-
  bom_stations_raw %>%
  dplyr::filter(end == lubridate::year(Sys.Date())) %>%
  dplyr::mutate(end = as.integer(end))

new_stations_site_list$site <-
  gsub("^0{1,2}", "", new_stations_site_list$site)

data.table::setDT(new_stations_site_list)
data.table::setkey(new_stations_site_list, "site")
```

#### Changes in stations_site_list

``` r
load(system.file("extdata", "stations_site_list.rda", package = "wrapique"))

(
  stations_site_list_changes <-
    diffobj::diffPrint(new_stations_site_list, stations_site_list)
)
```

#### Save stations_site_list Data and Changes

``` r
if (!dir.exists("../inst/extdata")) {
  dir.create("../inst/extdata", recursive = TRUE)
}

stations_site_list <- new_stations_site_list

save(stations_site_list,
     file = "../inst/extdata/stations_site_list.rda",
     compress = "bzip2")

# save(stations_site_list_changes,
#      file = "../inst/extdata/stations_site_list_changes.rda",
#      compress = "bzip2")
```

## Session Info

<PRE class="fansi fansi-output"><CODE>## <span style='color: #00BBBB; font-weight: bold;'>─ Session info ───────────────────────────────────────────────────────────────</span>
##  <span style='color: #555555; font-style: italic;'>setting </span> <span style='color: #555555; font-style: italic;'>value</span>
##  version  R version 4.2.2 (2022-10-31)
##  os       macOS Ventura 13.2.1
##  system   aarch64, darwin20
##  ui       X11
##  language (EN)
##  collate  en_US.UTF-8
##  ctype    en_US.UTF-8
##  tz       Australia/Perth
##  date     2023-03-02
##  pandoc   3.1 @ /opt/homebrew/bin/ (via rmarkdown)
## 
## <span style='color: #00BBBB; font-weight: bold;'>─ Packages ───────────────────────────────────────────────────────────────────</span>
##  <span style='color: #555555; font-style: italic;'>package    </span> <span style='color: #555555; font-style: italic;'>*</span> <span style='color: #555555; font-style: italic;'>version</span> <span style='color: #555555; font-style: italic;'>date (UTC)</span> <span style='color: #555555; font-style: italic;'>lib</span> <span style='color: #555555; font-style: italic;'>source</span>
##  archive       1.1.5   <span style='color: #555555;'>2022-05-06</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  ASGS.foyer    0.3.1   <span style='color: #555555;'>2021-03-21</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  askpass       1.1     <span style='color: #555555;'>2019-01-13</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  bit           4.0.5   <span style='color: #555555;'>2022-11-15</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  bit64         4.0.5   <span style='color: #555555;'>2020-08-30</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  cli           3.6.0   <span style='color: #555555;'>2023-01-09</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  colorout      1.2-2   <span style='color: #555555;'>2023-01-03</span> <span style='color: #555555;'>[1]</span> <span style='color: #BB00BB; font-weight: bold;'>Github (jalvesaq/colorout@79931fd)</span>
##  colorspace    2.1-0   <span style='color: #555555;'>2023-01-23</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  crayon        1.5.2   <span style='color: #555555;'>2022-09-29</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  credentials   1.3.2   <span style='color: #555555;'>2021-11-29</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  curl          5.0.0   <span style='color: #555555;'>2023-01-12</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  data.table    1.14.8  <span style='color: #555555;'>2023-02-17</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  digest        0.6.31  <span style='color: #555555;'>2022-12-11</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  dplyr       * 1.1.0   <span style='color: #555555;'>2023-01-29</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.2)</span>
##  ellipsis      0.3.2   <span style='color: #555555;'>2021-04-29</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  evaluate      0.20    <span style='color: #555555;'>2023-01-17</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  fansi         1.0.4   <span style='color: #555555;'>2023-01-22</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.2)</span>
##  fastmap       1.1.1   <span style='color: #555555;'>2023-02-24</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  forcats     * 1.0.0   <span style='color: #555555;'>2023-01-29</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.2)</span>
##  generics      0.1.3   <span style='color: #555555;'>2022-07-05</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  ggplot2     * 3.4.1   <span style='color: #555555;'>2023-02-10</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  glue          1.6.2   <span style='color: #555555;'>2022-02-24</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  gtable        0.3.1   <span style='color: #555555;'>2022-09-01</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  hms           1.1.2   <span style='color: #555555;'>2022-08-19</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  htmltools     0.5.4   <span style='color: #555555;'>2022-12-07</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  knitr         1.42    <span style='color: #555555;'>2023-01-25</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  lattice       0.20-45 <span style='color: #555555;'>2021-09-22</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  lifecycle     1.0.3   <span style='color: #555555;'>2022-10-07</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  lubridate   * 1.9.2   <span style='color: #555555;'>2023-02-10</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  magrittr      2.0.3   <span style='color: #555555;'>2022-03-30</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  munsell       0.5.0   <span style='color: #555555;'>2018-06-12</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  openssl       2.0.5   <span style='color: #555555;'>2022-12-06</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  pillar        1.8.1   <span style='color: #555555;'>2022-08-19</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  pkgconfig     2.0.3   <span style='color: #555555;'>2019-09-22</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  purrr       * 1.0.1   <span style='color: #555555;'>2023-01-10</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  R6            2.5.1   <span style='color: #555555;'>2021-08-19</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  readr       * 2.1.4   <span style='color: #555555;'>2023-02-10</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  rlang         1.0.6   <span style='color: #555555;'>2022-09-24</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  rmarkdown     2.20    <span style='color: #555555;'>2023-01-19</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.2)</span>
##  rstudioapi    0.14    <span style='color: #555555;'>2022-08-22</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  scales        1.2.1   <span style='color: #555555;'>2022-08-20</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  sessioninfo   1.2.2   <span style='color: #555555;'>2021-12-06</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  sp          * 1.6-0   <span style='color: #555555;'>2023-01-19</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.2)</span>
##  stringi       1.7.12  <span style='color: #555555;'>2023-01-11</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  stringr     * 1.5.0   <span style='color: #555555;'>2022-12-02</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  sys           3.4.1   <span style='color: #555555;'>2022-10-18</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  tibble      * 3.1.8   <span style='color: #555555;'>2022-07-22</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  tidyr       * 1.3.0   <span style='color: #555555;'>2023-01-24</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  tidyselect    1.2.0   <span style='color: #555555;'>2022-10-10</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  tidyverse   * 2.0.0   <span style='color: #555555;'>2023-02-22</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  timechange    0.2.0   <span style='color: #555555;'>2023-01-11</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  tzdb          0.3.0   <span style='color: #555555;'>2022-03-28</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  utf8          1.2.3   <span style='color: #555555;'>2023-01-31</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  vctrs         0.5.2   <span style='color: #555555;'>2023-01-23</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  vroom       * 1.6.1   <span style='color: #555555;'>2023-01-22</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  withr         2.5.0   <span style='color: #555555;'>2022-03-03</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  xfun          0.37    <span style='color: #555555;'>2023-01-31</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  yaml          2.3.7   <span style='color: #555555;'>2023-01-23</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.2)</span>
## 
## <span style='color: #555555;'> [1] /Users/adamsparks/Library/R/arm64/4.2/library</span>
## <span style='color: #555555;'> [2] /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/site-library</span>
## <span style='color: #555555;'> [3] /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/library</span>
## 
## <span style='color: #00BBBB; font-weight: bold;'>──────────────────────────────────────────────────────────────────────────────</span>
</CODE></PRE>
