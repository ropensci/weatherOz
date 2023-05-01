Create Databases of BOM Station Locations
================

<STYLE type='text/css' scoped>
PRE.fansi SPAN {padding-top: .25em; padding-bottom: .25em};
</STYLE>
<PRE class="fansi fansi-message"><CODE>## ── <span style='font-weight: bold;'>Attaching core tidyverse packages</span> ──────────────────────── tidyverse 2.0.0 ──
## <span style='color: #00BB00;'>✔</span> <span style='color: #0000BB;'>dplyr    </span> 1.1.2     <span style='color: #00BB00;'>✔</span> <span style='color: #0000BB;'>readr    </span> 2.1.4
## <span style='color: #00BB00;'>✔</span> <span style='color: #0000BB;'>forcats  </span> 1.0.0     <span style='color: #00BB00;'>✔</span> <span style='color: #0000BB;'>stringr  </span> 1.5.0
## <span style='color: #00BB00;'>✔</span> <span style='color: #0000BB;'>ggplot2  </span> 3.4.2     <span style='color: #00BB00;'>✔</span> <span style='color: #0000BB;'>tibble   </span> 3.2.1
## <span style='color: #00BB00;'>✔</span> <span style='color: #0000BB;'>lubridate</span> 1.9.2     <span style='color: #00BB00;'>✔</span> <span style='color: #0000BB;'>tidyr    </span> 1.3.0
## <span style='color: #00BB00;'>✔</span> <span style='color: #0000BB;'>purrr    </span> 1.0.1     
## ── <span style='font-weight: bold;'>Conflicts</span> ────────────────────────────────────────── tidyverse_conflicts() ──
## <span style='color: #BB0000;'>✖</span> <span style='color: #0000BB;'>dplyr</span>::<span style='color: #00BB00;'>between()</span>     masks <span style='color: #0000BB;'>data.table</span>::between()
## <span style='color: #BB0000;'>✖</span> <span style='color: #0000BB;'>dplyr</span>::<span style='color: #00BB00;'>filter()</span>      masks <span style='color: #0000BB;'>stats</span>::filter()
## <span style='color: #BB0000;'>✖</span> <span style='color: #0000BB;'>dplyr</span>::<span style='color: #00BB00;'>first()</span>       masks <span style='color: #0000BB;'>data.table</span>::first()
## <span style='color: #BB0000;'>✖</span> <span style='color: #0000BB;'>lubridate</span>::<span style='color: #00BB00;'>hour()</span>    masks <span style='color: #0000BB;'>data.table</span>::hour()
## <span style='color: #BB0000;'>✖</span> <span style='color: #0000BB;'>lubridate</span>::<span style='color: #00BB00;'>isoweek()</span> masks <span style='color: #0000BB;'>data.table</span>::isoweek()
## <span style='color: #BB0000;'>✖</span> <span style='color: #0000BB;'>dplyr</span>::<span style='color: #00BB00;'>lag()</span>         masks <span style='color: #0000BB;'>stats</span>::lag()
## <span style='color: #BB0000;'>✖</span> <span style='color: #0000BB;'>dplyr</span>::<span style='color: #00BB00;'>last()</span>        masks <span style='color: #0000BB;'>data.table</span>::last()
## <span style='color: #BB0000;'>✖</span> <span style='color: #0000BB;'>lubridate</span>::<span style='color: #00BB00;'>mday()</span>    masks <span style='color: #0000BB;'>data.table</span>::mday()
## <span style='color: #BB0000;'>✖</span> <span style='color: #0000BB;'>lubridate</span>::<span style='color: #00BB00;'>minute()</span>  masks <span style='color: #0000BB;'>data.table</span>::minute()
## <span style='color: #BB0000;'>✖</span> <span style='color: #0000BB;'>lubridate</span>::<span style='color: #00BB00;'>month()</span>   masks <span style='color: #0000BB;'>data.table</span>::month()
## <span style='color: #BB0000;'>✖</span> <span style='color: #0000BB;'>lubridate</span>::<span style='color: #00BB00;'>quarter()</span> masks <span style='color: #0000BB;'>data.table</span>::quarter()
## <span style='color: #BB0000;'>✖</span> <span style='color: #0000BB;'>lubridate</span>::<span style='color: #00BB00;'>second()</span>  masks <span style='color: #0000BB;'>data.table</span>::second()
## <span style='color: #BB0000;'>✖</span> <span style='color: #0000BB;'>purrr</span>::<span style='color: #00BB00;'>transpose()</span>   masks <span style='color: #0000BB;'>data.table</span>::transpose()
## <span style='color: #BB0000;'>✖</span> <span style='color: #0000BB;'>lubridate</span>::<span style='color: #00BB00;'>wday()</span>    masks <span style='color: #0000BB;'>data.table</span>::wday()
## <span style='color: #BB0000;'>✖</span> <span style='color: #0000BB;'>lubridate</span>::<span style='color: #00BB00;'>week()</span>    masks <span style='color: #0000BB;'>data.table</span>::week()
## <span style='color: #BB0000;'>✖</span> <span style='color: #0000BB;'>lubridate</span>::<span style='color: #00BB00;'>yday()</span>    masks <span style='color: #0000BB;'>data.table</span>::yday()
## <span style='color: #BB0000;'>✖</span> <span style='color: #0000BB;'>lubridate</span>::<span style='color: #00BB00;'>year()</span>    masks <span style='color: #0000BB;'>data.table</span>::year()
## <span style='color: #00BBBB;'>ℹ</span> Use the conflicted package (<span style='color: #0000BB; font-style: italic;'>&lt;http://conflicted.r-lib.org/&gt;</span>) to force all conflicts to become errors
</CODE></PRE>

This document provides details on methods used to create the database of
BOM JSON files for stations and corresponding metadata, *e.g.*,
latitude, longitude (which are more detailed than what is in the JSON
file), start, end, elevation, etc.

Refer to this BOM page for more reference,
<http://reg.bom.gov.au/catalogue/anon-ftp.shtml>.

## Product code definitions

### States

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
# The last eight lines contain other information that we don't want.
# For some reason, reading it directly from the BOM website does not work, so
# we use curl to fetch it first and then import it from the R tempdir().

curl::curl_download(
  url = "ftp://ftp.bom.gov.au/anon2/home/ncc/metadata/sitelists/stations.zip",
  destfile = file.path(tempdir(), "stations.zip"),
  mode = "wb",
  quiet = TRUE
)
 utils::unzip(file.path(tempdir(), "stations.zip"), exdir = tempdir())
  file_in <- file.path(tempdir(), "stations.txt")

  bom_stations <-
    data.table::setDT(
      readr::read_fwf(
        file = file_in,
        na = c("..", ".....", " "),
        skip = 4,
        col_positions = readr::fwf_cols(
          "station_code" = c(1, 8),
          "dist" = c(9, 14),
          "station_name" = c(15, 55),
          "start" = c(56, 63),
          "end" = c(64, 71),
          "lat" = c(72, 80),
          "lon" = c(81, 90),
          "source" = c(91, 105),
          "state" = c(106, 109),
          "elev.m" = c(110, 120),
          "bar_height.m" = c(121, 129),
          "wmo" = c(130, 136)
        ),
        col_types = c(
          station_code = readr::col_character(),
          dist = readr::col_character(),
          site_name = readr::col_character(),
          start = readr::col_integer(),
          end = readr::col_integer(),
          lat = readr::col_double(),
          lon = readr::col_double(),
          source = readr::col_character(),
          state = readr::col_character(),
          elev.m = readr::col_double(),
          bar_height.m = readr::col_double(),
          wmo = readr::col_integer()
        ),
        n_max = length(utils::count.fields(file_in)) - 6,
        # drop last six rows
      )
    )

  bom_stations[, station_code := as.factor(station_code)]
  bom_stations[, station_name := DescTools::StrCap(x = station_name,
                                                   method = "word")]
  bom_stations[, start := as.integer(start)]
  bom_stations[, end := as.integer(end)]
  bom_stations[, status := ifelse(!is.na(end), "Closed", "Open")]
  bom_stations[is.na(end), end := as.integer(format(Sys.Date(), "%Y"))]
```

## Save data

### Station location database for get_ag_bulletin()

First, rename columns and drop a few that aren’t necessary for the ag
bulletin information. Then pad the `site` field with 0 to match the data
in the XML file that holds the ag bulletin information. Lastly, create
the data file for use in {weatherOz}.

``` r
new_stations_site_list <- data.table::data.table(bom_stations)

new_stations_site_list[, site :=
                         gsub("^0{1,2}", "", new_stations_site_list$site)]

data.table::setDT(new_stations_site_list)
data.table::setkey(new_stations_site_list, "site")
```

#### Changes in “stations_site_list”

``` r
load(system.file("extdata", "stations_site_list.rda", package = "weatherOz"))

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
##  version  R version 4.3.0 (2023-04-21)
##  os       macOS Ventura 13.3.1
##  system   aarch64, darwin20
##  ui       X11
##  language (EN)
##  collate  en_US.UTF-8
##  ctype    en_US.UTF-8
##  tz       Australia/Perth
##  date     2023-05-01
##  pandoc   3.1.2 @ /opt/homebrew/bin/ (via rmarkdown)
## 
## <span style='color: #00BBBB; font-weight: bold;'>─ Packages ───────────────────────────────────────────────────────────────────</span>
##  <span style='color: #555555; font-style: italic;'>package    </span> <span style='color: #555555; font-style: italic;'>*</span> <span style='color: #555555; font-style: italic;'>version </span> <span style='color: #555555; font-style: italic;'>date (UTC)</span> <span style='color: #555555; font-style: italic;'>lib</span> <span style='color: #555555; font-style: italic;'>source</span>
##  askpass       1.1      <span style='color: #555555;'>2019-01-13</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  bit           4.0.5    <span style='color: #555555;'>2022-11-15</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  bit64         4.0.5    <span style='color: #555555;'>2020-08-30</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  boot          1.3-28.1 <span style='color: #555555;'>2022-11-22</span> <span style='color: #555555;'>[2]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  cellranger    1.1.0    <span style='color: #555555;'>2016-07-27</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  class         7.3-21   <span style='color: #555555;'>2023-01-23</span> <span style='color: #555555;'>[2]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  cli           3.6.1    <span style='color: #555555;'>2023-03-23</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  colorspace    2.1-0    <span style='color: #555555;'>2023-01-23</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  crayon        1.5.2    <span style='color: #555555;'>2022-09-29</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  credentials   1.3.2    <span style='color: #555555;'>2021-11-29</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  curl          5.0.0    <span style='color: #555555;'>2023-01-12</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  data.table  * 1.14.8   <span style='color: #555555;'>2023-02-17</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  DescTools     0.99.48  <span style='color: #555555;'>2023-02-19</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  digest        0.6.31   <span style='color: #555555;'>2022-12-11</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  dplyr       * 1.1.2    <span style='color: #555555;'>2023-04-20</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  e1071         1.7-13   <span style='color: #555555;'>2023-02-01</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  evaluate      0.20     <span style='color: #555555;'>2023-01-17</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  Exact         3.2      <span style='color: #555555;'>2022-09-25</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  expm          0.999-7  <span style='color: #555555;'>2023-01-09</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  fansi         1.0.4    <span style='color: #555555;'>2023-01-22</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  fastmap       1.1.1    <span style='color: #555555;'>2023-02-24</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  forcats     * 1.0.0    <span style='color: #555555;'>2023-01-29</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  generics      0.1.3    <span style='color: #555555;'>2022-07-05</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  ggplot2     * 3.4.2    <span style='color: #555555;'>2023-04-03</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  gld           2.6.6    <span style='color: #555555;'>2022-10-23</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  glue          1.6.2    <span style='color: #555555;'>2022-02-24</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  gtable        0.3.3    <span style='color: #555555;'>2023-03-21</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  hms           1.1.3    <span style='color: #555555;'>2023-03-21</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  htmltools     0.5.5    <span style='color: #555555;'>2023-03-23</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  httr          1.4.5    <span style='color: #555555;'>2023-02-24</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  knitr         1.42     <span style='color: #555555;'>2023-01-25</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  lattice       0.21-8   <span style='color: #555555;'>2023-04-05</span> <span style='color: #555555;'>[2]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  lifecycle     1.0.3    <span style='color: #555555;'>2022-10-07</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  lmom          2.9      <span style='color: #555555;'>2022-05-29</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  lubridate   * 1.9.2    <span style='color: #555555;'>2023-02-10</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  magrittr      2.0.3    <span style='color: #555555;'>2022-03-30</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  MASS          7.3-58.4 <span style='color: #555555;'>2023-03-07</span> <span style='color: #555555;'>[2]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  Matrix        1.5-4    <span style='color: #555555;'>2023-04-04</span> <span style='color: #555555;'>[2]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  munsell       0.5.0    <span style='color: #555555;'>2018-06-12</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  mvtnorm       1.1-3    <span style='color: #555555;'>2021-10-08</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  openssl       2.0.6    <span style='color: #555555;'>2023-03-09</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  paint         0.1.7    <span style='color: #555555;'>2023-04-23</span> <span style='color: #555555;'>[1]</span> <span style='color: #BB00BB; font-weight: bold;'>Github (MilesMcBain/paint@a7e97dd)</span>
##  pillar        1.9.0    <span style='color: #555555;'>2023-03-22</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  pkgconfig     2.0.3    <span style='color: #555555;'>2019-09-22</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  proxy         0.4-27   <span style='color: #555555;'>2022-06-09</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  purrr       * 1.0.1    <span style='color: #555555;'>2023-01-10</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  R6            2.5.1    <span style='color: #555555;'>2021-08-19</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  Rcpp          1.0.10   <span style='color: #555555;'>2023-01-22</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  readr       * 2.1.4    <span style='color: #555555;'>2023-02-10</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  readxl        1.4.2    <span style='color: #555555;'>2023-02-09</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  rlang         1.1.0    <span style='color: #555555;'>2023-03-14</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  rmarkdown     2.21     <span style='color: #555555;'>2023-03-26</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  rootSolve     1.8.2.3  <span style='color: #555555;'>2021-09-29</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  rstudioapi    0.14     <span style='color: #555555;'>2022-08-22</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  scales        1.2.1    <span style='color: #555555;'>2022-08-20</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  sessioninfo   1.2.2    <span style='color: #555555;'>2021-12-06</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  stringi       1.7.12   <span style='color: #555555;'>2023-01-11</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  stringr     * 1.5.0    <span style='color: #555555;'>2022-12-02</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  sys           3.4.1    <span style='color: #555555;'>2022-10-18</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  tibble      * 3.2.1    <span style='color: #555555;'>2023-03-20</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  tidyr       * 1.3.0    <span style='color: #555555;'>2023-01-24</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  tidyselect    1.2.0    <span style='color: #555555;'>2022-10-10</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  tidyverse   * 2.0.0    <span style='color: #555555;'>2023-02-22</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  timechange    0.2.0    <span style='color: #555555;'>2023-01-11</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  tzdb          0.3.0    <span style='color: #555555;'>2022-03-28</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  utf8          1.2.3    <span style='color: #555555;'>2023-01-31</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  vctrs         0.6.2    <span style='color: #555555;'>2023-04-19</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  vroom         1.6.1    <span style='color: #555555;'>2023-01-22</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  withr         2.5.0    <span style='color: #555555;'>2022-03-03</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  xfun          0.39     <span style='color: #555555;'>2023-04-20</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
##  yaml          2.3.7    <span style='color: #555555;'>2023-01-23</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.3.0)</span>
## 
## <span style='color: #555555;'> [1] /Users/adamsparks/Library/R/arm64/4.3/library</span>
## <span style='color: #555555;'> [2] /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/library</span>
## 
## <span style='color: #00BBBB; font-weight: bold;'>──────────────────────────────────────────────────────────────────────────────</span>
</CODE></PRE>
