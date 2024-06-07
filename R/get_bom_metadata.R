#' Fetch and Clean BOM Metadata for SILO Stations
#'
#' This function caches the file during a user's session for faster responses.
#' Starting a new \R session will delete this file.
#'
#' @return a \CRANpkg{data.table} of BOM station metadata.
#' @keywords Internal
#' @autoglobal
#' @noRd

.get_bom_metadata <- function() {
  op <- options(timeout = 600L)
  on.exit(options(op))

  file_in <- file.path(tempdir(), "stations.txt")
  if (!file.exists(file_in)) {
    tryCatch({
      zip_file <- file.path(tempdir(), "stations.zip")
      utils::download.file(
        url =
          "ftp://ftp.bom.gov.au/anon2/home/ncc/metadata/sitelists/stations.zip",
        destfile = zip_file,
        mode = "wb",
        quiet = TRUE
      )
    }, error = function(x)
      stop(
        "The BOM server with the station location information is not ",
        "responding. Please retry again later.\n",
        call. = FALSE
      ))

    utils::unzip(zip_file, exdir = tempdir())
    file.remove(zip_file)
  }

  bom_stations <-
    data.table::setDT(
      utils::read.fwf(
        file = file_in,
        widths = c(8, 6, 40, 8, 8, 10, 10, 15, 3, 11, 9, 7),
        header = FALSE,
        skip = 4,
        col.names = c(
          "Site",
          "Dist",
          "Site name",
          "Start",
          "End",
          "Lat",
          "Lon",
          "Source",
          "STA",
          "Height (m)",
          "Bar_ht",
          "WMO"
        ),
        nrows = length(utils::count.fields(file_in)) - 6,
        comment.char = "",
        allowEscapes = TRUE,
        strip.white = TRUE,
        colClasses = "character"
      )
    )
  data.table::setnames(
    x = bom_stations,
    new = c(
      "station_code",
      "dist",
      "station_name",
      "start",
      "end",
      "latitude",
      "longitude",
      "source",
      "state",
      "elev_m",
      "bar_height.m",
      "wmo"
    ),
    old = c(
      "Site",
      "Dist",
      "Site.name",
      "Start",
      "End",
      "Lat",
      "Lon",
      "Source",
      "STA",
      "Height..m.",
      "Bar_ht",
      "WMO"
    )
  )

  # replace ".." and "....." with NA
  bom_stations <-
    bom_stations[, lapply(.SD, function(x)
      replace(x, which(x == ".."), NA))]
  bom_stations <-
    bom_stations[, lapply(.SD, function(x)
      replace(x, which(x == "....."), NA))]
  bom_stations[, station_code := as.factor(station_code)]
  data.table::setkey(x = bom_stations, station_code)
  bom_stations[, station_name := .strcap(x = station_name)]
  bom_stations[, start := as.integer(start)]
  bom_stations[, end := as.integer(end)]
  bom_stations[, latitude := as.numeric(latitude)]
  bom_stations[, longitude := as.numeric(longitude)]
  bom_stations[, status := ifelse(!is.na(end), "closed", "open")]
  bom_stations[, elev_m := as.numeric(elev_m)]
  bom_stations[, wmo := as.numeric(wmo)]
  bom_stations[, dist := NULL]
  bom_stations[, source := NULL]
  bom_stations[, bar_height.m := NULL]
  bom_stations[, source := "Bureau of Meteorology"]

  data.table::setcolorder(
    bom_stations,
    neworder = c(
      "station_code",
      "station_name",
      "start",
      "end",
      "latitude",
      "longitude",
      "state",
      "elev_m",
      "source",
      "status",
      "wmo"
    )
  )

  return(bom_stations[])
}
