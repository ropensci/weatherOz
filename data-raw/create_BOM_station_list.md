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

bom_stations_lines <- read_lines(file.path(tempdir(), "stations.zip"))
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

## Save data

### Station location database for get_ag_bulletin()

First, rename columns and drop a few that aren’t necessary for the ag
bulletin information. Then pad the `site` field with 0 to match the data
in the XML file that holds the ag bulletin information. Lastly, create
the data file for use in {wrapique}.

``` r
new_stations_site_list <- data.table::data.table(bom_stations_raw)

new_stations_site_list[, site :=
                         gsub("^0{1,2}", "", new_stations_site_list$site)]

data.table::setDT(new_stations_site_list)
data.table::setkey(new_stations_site_list, "site")
```

#### Changes in “stations_site_list”

``` r
load(system.file("extdata", "stations_site_list.rda", package = "wrapique"))

(
  stations_site_list_changes <-
    diffobj::diffPrint(new_stations_site_list, stations_site_list)
)
```

<PRE class="fansi fansi-output"><CODE>## <span style='color: #555555;'>No visible differences between objects, but objects are *not* `all.equal`:</span>
## <span style='color: #555555;'>- Column &#039;lat&#039;: Mean relative difference: 0.000002582665</span>
## <span style='color: #BBBB00;'>&lt;</span> <span style='color: #BBBB00;'>new_stations_site_list</span>                                                       
## <span style='color: #0000BB;'>&gt;</span> <span style='color: #0000BB;'>stations_site_list</span>                                                           
## <span style='color: #00BBBB;'>@@ 1,24 / 1,24 @@                                                              </span>
##           site dist               name start  end      lat      lon state  elev
##       1:  1000   01           KARUNJIE  1940 1983 -16.2919 127.1956    WA 320.0
##       2: 10000   10        AMERY ACRES  1934 2023 -31.1683 117.0736    WA 340.0
##       3: 10001   10            BAANDEE  1905 1976 -31.6000 117.9667    WA  &lt;NA&gt;
##       4: 10002   10      BAANDEE NORTH  1911 2023 -31.3667 117.9031    WA 270.0
##       5: 10003   10          BALKULING  1913 1955 -31.9833 117.1000    WA  &lt;NA&gt;
##      ---                                                                       
##   19416:  9995  09A       LAKE SHASTER  2003 2010 -33.8044 120.6342    WA  81.0
##   19417:  9996  09A  LATITUDE 33 WINES  1993 2009 -33.8294 115.1903    WA 120.0
##   19418:  9997  09A RAVENSCLIFFE ALERT  2003 2023 -33.7647 115.8431    WA 200.0
##   19419:  9998  09A      NORTH WALPOLE  2004 2023 -34.9469 116.7222    WA  73.0
##   19420:  9999  09A     ALBANY AIRPORT  2012 2023 -34.9411 117.8158    WA  68.4
##          bar_ht   wmo                                                          
##       1:   &lt;NA&gt;  &lt;NA&gt;                                                          
##       2:   &lt;NA&gt;  &lt;NA&gt;                                                          
##       3:   &lt;NA&gt;  &lt;NA&gt;                                                          
##       4:   &lt;NA&gt;  &lt;NA&gt;                                                          
##       5:   &lt;NA&gt;  &lt;NA&gt;                                                          
##      ---                                                                       
##   19416:   &lt;NA&gt;  &lt;NA&gt;                                                          
##   19417:   &lt;NA&gt;  &lt;NA&gt;                                                          
##   19418:   &lt;NA&gt;  &lt;NA&gt;                                                          
##   19419:   73.5 95647                                                          
##   19420:   70.0 94802
</CODE></PRE>

#### Save stations_site_list Data and Changes

``` r
if (!dir.exists("../inst/extdata")) {
  dir.create("../inst/extdata", recursive = TRUE)
}

stations_site_list <- new_stations_site_list

save(stations_site_list,
     file = "../inst/extdata/stations_site_list.rda",
     compress = "bzip2")

save(stations_site_list_changes,
     file = "../inst/extdata/stations_site_list_changes.rda",
     compress = "bzip2")
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
##  date     2023-03-09
##  pandoc   3.1.1 @ /opt/homebrew/bin/ (via rmarkdown)
## 
## <span style='color: #00BBBB; font-weight: bold;'>─ Packages ───────────────────────────────────────────────────────────────────</span>
##  <span style='color: #555555; font-style: italic;'>package    </span> <span style='color: #555555; font-style: italic;'>*</span> <span style='color: #555555; font-style: italic;'>version</span> <span style='color: #555555; font-style: italic;'>date (UTC)</span> <span style='color: #555555; font-style: italic;'>lib</span> <span style='color: #555555; font-style: italic;'>source</span>
##  archive       1.1.5   <span style='color: #555555;'>2022-05-06</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  askpass       1.1     <span style='color: #555555;'>2019-01-13</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  bit           4.0.5   <span style='color: #555555;'>2022-11-15</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  bit64         4.0.5   <span style='color: #555555;'>2020-08-30</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  cli           3.6.0   <span style='color: #555555;'>2023-01-09</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  colorout      1.2-2   <span style='color: #555555;'>2023-01-03</span> <span style='color: #555555;'>[1]</span> <span style='color: #BB00BB; font-weight: bold;'>Github (jalvesaq/colorout@79931fd)</span>
##  colorspace    2.1-0   <span style='color: #555555;'>2023-01-23</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  crayon        1.5.2   <span style='color: #555555;'>2022-09-29</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  credentials   1.3.2   <span style='color: #555555;'>2021-11-29</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  curl          5.0.0   <span style='color: #555555;'>2023-01-12</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  data.table  * 1.14.8  <span style='color: #555555;'>2023-02-17</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  diffobj       0.3.5   <span style='color: #555555;'>2021-10-05</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
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
