Create BOM Radar Location Database
================

<STYLE type='text/css' scoped>
PRE.fansi SPAN {padding-top: .25em; padding-bottom: .25em};
</STYLE>

## Note

This functionality is ported from {bomrang},
<https://github.com/ropensci-archive/bomrang/blob/main/data-raw/create_BOM_radar_locations.md>.

## Get BOM Radar Locations

BOM maintains a shapefile of radar site names and their geographic
locations. For ease, we’ll just use the .dbf file part of the shapefile
to extract the product codes and radar locations. The file is available
from BOM’s anonymous FTP server with spatial data
<ftp://ftp.bom.gov.au/anon/home/adfd/spatial/>, specifically the DBF
file portion of a shapefile,
<ftp://ftp.bom.gov.au/anon/home/adfd/spatial/IDR00007.dbf>.

``` r
curl::curl_download(
  "ftp://ftp.bom.gov.au/anon/home/adfd/spatial/IDR00007.dbf",
  destfile = paste0(tempdir(), "radar_locations.dbf"),
  mode = "wb",
  quiet = TRUE
)

new_radar_locations <-
  foreign::read.dbf(paste0(tempdir(), "radar_locations.dbf"), as.is = TRUE)

new_radar_locations$LocationID <-
  ifelse(
    test = nchar(new_radar_locations$LocationID) == 1,
    yes = paste0("0", new_radar_locations$LocationID),
    no = new_radar_locations$LocationID
  )

data.table::setDT(new_radar_locations)
data.table::setkey(new_radar_locations, "Name")

str(new_radar_locations)
```

    ## Classes 'data.table' and 'data.frame':   69 obs. of  13 variables:
    ##  $ Name      : chr  "Adelaide" "Albany" "Alice Springs" "Bairnsdale" ...
    ##  $ Longitude : num  138 118 134 148 148 ...
    ##  $ Latitude  : num  -34.6 -34.9 -23.8 -37.9 -19.9 ...
    ##  $ Radar_id  : int  64 31 25 68 24 93 66 105 1 17 ...
    ##  $ Full_Name : chr  "Adelaide (Buckland Park)" "Albany" "Alice Springs" "Bairnsdale" ...
    ##  $ IDRnn0name: chr  "BuckPk" "Albany" "AliceSp" "Bnsdale" ...
    ##  $ IDRnn1name: chr  "BucklandPk" "Albany" "AliceSprings" "Bairnsdale" ...
    ##  $ State     : chr  "SA" "WA" "NT" "VIC" ...
    ##  $ Type      : chr  "Doppler" "Doppler" "Standard weather watch" "Doppler" ...
    ##  $ Group_    : chr  "Yes" "Yes" "Yes" "Yes" ...
    ##  $ Status    : chr  "Public" "Public" "Public" "Public" ...
    ##  $ Archive   : chr  "BuckPk" "Albany" "AliceSp" "Bnsdale" ...
    ##  $ LocationID: chr  "64" "31" "25" "68" ...
    ##  - attr(*, "data_types")= chr [1:13] "C" "F" "F" "N" ...
    ##  - attr(*, ".internal.selfref")=<externalptr> 
    ##  - attr(*, "sorted")= chr "Name"

## Show Changes from Last Release

To ensure that the data being compared is from the most recent release,
reinstall {wrapique} from CRAN.

``` r
install.packages("bomrang", repos = "http://cran.us.r-project.org")

load(system.file("extdata", "radar_locations.rda", package = "wrapique"))

(
  radar_location_changes <-
    diffobj::diffPrint(new_radar_locations, radar_locations)
)
```

# Save Radar Stations Data and Changes

Save the radar stations’ metadata and changes to disk for use in
{wrapique}.

``` r
if (!dir.exists("../inst/extdata")) {
  dir.create("../inst/extdata", recursive = TRUE)
}

radar_locations <- new_radar_locations

save(radar_locations,
     file = "../inst/extdata/radar_locations.rda",
     compress = "bzip2")

# save(radar_location_changes,
#      file = "../inst/extdata/radar_location_changes.rda",
#      compress = "bzip2")
```

## Session Info

``` r
sessioninfo::session_info()
```

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
##  date     2023-02-28
##  pandoc   3.1 @ /opt/homebrew/bin/ (via rmarkdown)
## 
## <span style='color: #00BBBB; font-weight: bold;'>─ Packages ───────────────────────────────────────────────────────────────────</span>
##  <span style='color: #555555; font-style: italic;'>package    </span> <span style='color: #555555; font-style: italic;'>*</span> <span style='color: #555555; font-style: italic;'>version</span> <span style='color: #555555; font-style: italic;'>date (UTC)</span> <span style='color: #555555; font-style: italic;'>lib</span> <span style='color: #555555; font-style: italic;'>source</span>
##  askpass       1.1     <span style='color: #555555;'>2019-01-13</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  cli           3.6.0   <span style='color: #555555;'>2023-01-09</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  colorout      1.2-2   <span style='color: #555555;'>2023-01-03</span> <span style='color: #555555;'>[1]</span> <span style='color: #BB00BB; font-weight: bold;'>Github (jalvesaq/colorout@79931fd)</span>
##  credentials   1.3.2   <span style='color: #555555;'>2021-11-29</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  curl          5.0.0   <span style='color: #555555;'>2023-01-12</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  data.table    1.14.8  <span style='color: #555555;'>2023-02-17</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  digest        0.6.31  <span style='color: #555555;'>2022-12-11</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  evaluate      0.20    <span style='color: #555555;'>2023-01-17</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  fansi         1.0.4   <span style='color: #555555;'>2023-01-22</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.2)</span>
##  fastmap       1.1.1   <span style='color: #555555;'>2023-02-24</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  foreign       0.8-84  <span style='color: #555555;'>2022-12-06</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  htmltools     0.5.4   <span style='color: #555555;'>2022-12-07</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  knitr         1.42    <span style='color: #555555;'>2023-01-25</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  openssl       2.0.5   <span style='color: #555555;'>2022-12-06</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  rlang         1.0.6   <span style='color: #555555;'>2022-09-24</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  rmarkdown     2.20    <span style='color: #555555;'>2023-01-19</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.2)</span>
##  rstudioapi    0.14    <span style='color: #555555;'>2022-08-22</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  sessioninfo   1.2.2   <span style='color: #555555;'>2021-12-06</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  sys           3.4.1   <span style='color: #555555;'>2022-10-18</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  xfun          0.37    <span style='color: #555555;'>2023-01-31</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.0)</span>
##  yaml          2.3.7   <span style='color: #555555;'>2023-01-23</span> <span style='color: #555555;'>[1]</span> <span style='color: #555555;'>CRAN (R 4.2.2)</span>
## 
## <span style='color: #555555;'> [1] /Users/adamsparks/Library/R/arm64/4.2/library</span>
## <span style='color: #555555;'> [2] /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/site-library</span>
## <span style='color: #555555;'> [3] /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/library</span>
## 
## <span style='color: #00BBBB; font-weight: bold;'>──────────────────────────────────────────────────────────────────────────────</span>
</CODE></PRE>
