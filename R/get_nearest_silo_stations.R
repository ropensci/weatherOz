

#' Create a data.table object of all stations available in SILO
#'
#' Uses SILO's cgi-bin web interface to query and download a text file of
#' BOM weather stations in SILO within 2500 km of Finke, NT (near the Lambert
#' centre of Australia). Stations are returned in order of distance from the
#' Finke Post Office.
#'
#' @noRd
.get_silo_stations <- function() {
  curl::curl_download(url = "https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?format=near&station=015526&radius=2500",
                      destfile = file.path(tempdir(), "stations.txt"))
  r <- data.table::fread(file.path(tempdir(), "stations.txt"))[, -7]
  data.table::setnames(r, names(r), c("station_id",
                                      "station_name",
                                      "latitude",
                                      "longitude",
                                      "state",
                                      "elevation"))
  return(r)
}
