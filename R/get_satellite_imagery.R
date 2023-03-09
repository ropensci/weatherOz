
#' Get a listing of available BOM satellite GeoTIFF imagery
#'
#' Fetch a listing of \acronym{BOM} 'GeoTIFF' satellite imagery from
#' \url{ftp://ftp.bom.gov.au/anon/gen/gms/} to determine which files are
#' currently available for download.  Files are available at ten minute update
#' frequency with a 24 hour delete time.  Useful to know the most recent files
#' available and then specify in the \code{\link{get_satellite_imagery}}
#' function.
#'
#' @param product_id Character.  \acronym{BOM} product ID of interest for which
#' a list of available images will be returned.  Defaults to all images
#' currently available.
#'
#' @details Valid \acronym{BOM} satellite Product IDs for 'GeoTIFF' files
#'  include:
#' \describe{
#' \item{IDE00420}{AHI cloud cover only 2km FD GEOS GIS}
#' \item{IDE00421}{AHI IR (Ch13) greyscale 2km FD GEOS GIS}
#' \item{IDE00422}{AHI VIS (Ch3) greyscale 2km FD GEOS GIS}
#' \item{IDE00423}{AHI IR (Ch13) Zehr 2km FD GEOS GIS}
#' \item{IDE00425}{AHI VIS (true colour) / IR (Ch13 greyscale) composite 1km FD
#' GEOS GIS}
#' \item{IDE00426}{AHI VIS (true colour) / IR (Ch13 greyscale) composite 2km FD
#' GEOS GIS}
#' \item{IDE00427}{AHI WV (Ch8) 2km FD GEOS GIS}
#' \item{IDE00430}{AHI cloud cover only 2km AUS equirect. GIS}
#' \item{IDE00431}{AHI IR (Ch13) greyscale 2km AUS equirect. GIS}
#' \item{IDE00432}{AHI VIS (Ch3) greyscale 2km AUS equirect. GIS}
#' \item{IDE00433}{AHI IR (Ch13) Zehr 2km AUS equirect. GIS}
#' \item{IDE00435}{AHI VIS (true colour) / IR (Ch13 greyscale) composite 1km AUS
#' equirect. GIS}
#' \item{IDE00436}{AHI VIS (true colour) / IR (Ch13 greyscale) composite 2km AUS
#' equirect. GIS}
#' \item{IDE00437}{AHI WV (Ch8) 2km AUS equirect. GIS}
#' \item{IDE00439}{AHI VIS (Ch3) greyscale 0.5km AUS equirect. GIS}
#' }
#'
#' @return
#' A vector of all available files for the requested Product ID(s).
#'
#' @references
#' Australian Bureau of Meteorology (\acronym{BOM}) high-definition satellite
#' images \url{http://www.bom.gov.au/australia/satellite/index.shtml}
#'
#' @examplesIf interactive()
#' # Check availability of AHI VIS (true colour) / IR (Ch13 greyscale) composite
#' # 1km FD GEOS GIS images
#' imagery <- get_available_imagery(product_id = "IDE00425")
#'
#' @family bomrang-ported
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @export get_available_imagery

get_available_imagery <- function(product_id = "all") {
  ftp_base <- "ftp://ftp.bom.gov.au/anon/gen/gms/"
  .check_IDs(product_id)
  message("\nThe following files are currently available for download:\n")
  tif_list <- .ftp_images(product_id, bom_server = ftp_base)
  write(tif_list, file = file.path(tempdir(), "tif_list"))
  print(tif_list)
}

#' Get \acronym{BOM} Satellite GeoTIFF Imagery
#'
#' Fetch \acronym{BOM} satellite GeoTIFF imagery from
#' \url{ftp://ftp.bom.gov.au/anon/gen/gms/} and return a raster
#' \code{\link[terra]{SpatRaster}} object of 'GeoTIFF' files. Files are
#'  available at ten minutes update frequency with a 24 hour delete time.
#'  It is suggested to check file availability first by using
#'  \code{\link{get_available_imagery}}.
#'
#' @param product_id Character. \acronym{BOM} product ID to download in
#' 'GeoTIFF' format and import as a \code{\link[terra]{SpatRaster}} object.  A
#' vector of values from \code{\link{get_available_imagery}} may be used here.
#' Value is required.
#' @param scans Numeric. Number of scans to download, starting with most recent
#' and progressing backwards, \emph{e.g.}, 1 - the most recent single scan
#' available , 6 - the most recent hour available, 12 - the most recent 2 hours
#' available, etc.  Negating will return the oldest files first.  Defaults to 1.
#' Value is optional.
#'
#' @details Valid \acronym{BOM} satellite Product IDs for use with
#' \var{product_id} include:
#' \describe{
#' \item{IDE00420}{AHI cloud cover only 2km FD GEOS GIS}
#' \item{IDE00421}{AHI IR (Ch13) greyscale 2km FD GEOS GIS}
#' \item{IDE00422}{AHI VIS (Ch3) greyscale 2km FD GEOS GIS}
#' \item{IDE00423}{AHI IR (Ch13) Zehr 2km FD GEOS GIS}
#' \item{IDE00425}{AHI VIS (true colour) / IR (Ch13 greyscale) composite 1km FD
#'  GEOS GIS}
#' \item{IDE00426}{AHI VIS (true colour) / IR (Ch13 greyscale) composite 2km FD
#'  GEOS GIS}
#' \item{IDE00427}{AHI WV (Ch8) 2km FD GEOS GIS}
#' \item{IDE00430}{AHI cloud cover only 2km AUS equirect. GIS}
#' \item{IDE00431}{AHI IR (Ch13) greyscale 2km AUS equirect. GIS}
#' \item{IDE00432}{AHI VIS (Ch3) greyscale 2km AUS equirect. GIS}
#' \item{IDE00433}{AHI IR (Ch13) Zehr 2km AUS equirect. GIS}
#' \item{IDE00435}{AHI VIS (true colour) / IR (Ch13 greyscale) composite 1km AUS
#'  equirect. GIS}
#' \item{IDE00436}{AHI VIS (true colour) / IR (Ch13 greyscale) composite 2km AUS
#'  equirect. GIS}
#' \item{IDE00437}{AHI WV (Ch8) 2km AUS equirect. GIS}
#' \item{IDE00439}{AHI VIS (Ch3) greyscale 0.5km AUS equirect. GIS}
#' }
#'
#' @family bomrang-ported
#' @seealso
#' \code{\link{get_available_imagery}}
#'
#' @return
#' A \code{SpatRaster} object of GeoTIFF images with layers named by
#'  \acronym{BOM} Product ID, timestamp and band.
#'
#' @note The original \pkg{bomrang} version of this function supported local
#' file caching using \CRANpkg{hoardr}. This version does not support this
#' functionality any longer.
#'
#' @references
#' Australian Bureau of Meteorology (BOM) high-definition satellite images \cr
#' \url{http://www.bom.gov.au/australia/satellite/index.shtml}
#'
#' @examplesIf interactive()
#' # Fetch AHI VIS (true colour) / IR (Ch13 greyscale) composite 1km FD
#' # GEOS GIS \code{SpatRaster} object for most recent single scan available
#'
#' imagery <- get_satellite_imagery(product_id = "IDE00425", scans = 1)
#'
#' # Get a list of available image files and use that to specify files for
#' # download, downloading the two most recent files available
#'
#' avail <- get_available_imagery(product_id = "IDE00425")
#' imagery <- get_satellite_imagery(product_id = avail, scans = 2)
#'
#' @author Adam H. Sparks, \email{adam.sparks@@dpird.wa.gov.au}
#' @rdname get_satellite_imagery
#' @export get_satellite_imagery

get_satellite_imagery <- get_satellite <-
  function(product_id,
           scans = 1) {
    if (length(unique(substr(product_id, 1, 8))) != 1) {
      stop("\nwrapique only supports working with one Product ID at a time\n")
    }

    ftp_base <- "ftp://ftp.bom.gov.au/anon/gen/gms/"

    # if we're feeding output from get_available_imagery(), use those values
    if (substr(
      product_id[1],
      nchar(product_id[1]) - 3, nchar(product_id[1])
    ) == ".tif") {
      tif_files <- utils::tail(product_id, scans)
    } else {
      # otherwise check the user entered product_id values
      .check_IDs(product_id)

      if (any(grepl("tif_files", list.files(tempdir())))) {
        # read files already checked using available_images()
        tif_files <- readLines(file.path(tempdir(), "tif_files"))
      } else {
        # check what's on the server
        tif_files <- .ftp_images(product_id, bom_server = ftp_base)
      }

      # filter by number of scans requested
      tif_files <- utils::tail(tif_files, scans)
    }

    tif_files <- paste0(ftp_base, tif_files)

    # download files from server

    h <- curl::new_handle()
    curl::handle_setopt(handle = h,
                        FTP_RESPONSE_TIMEOUT = 200000,
                        CONNECTTIMEOUT = 90,
                        ftp_use_epsv = TRUE
    )

    # TODO: this needs to be commented to be more clear
    tryCatch({
      Map(
        function(urls, destination)
          curl::curl_download(urls,
                              destination,
                              mode = "wb",
                              quiet = TRUE,
                              handle = h
          ),
        tif_files,
        file.path(tempdir(), basename(tif_files))
      )
    },
    error = function() {
      return(magick::image_read(
        system.file("error_images",
                    "image_error_message.png",
                    package = "wrapique")
      ))
    }
    )
    # create SpatRaster object of the GeoTIFF files
    files <-
      list.files(tempdir(), pattern = "\\.tif$", full.names = TRUE)
    files <- basename(files)[basename(files) %in% basename(tif_files)]
    files <- file.path(tempdir(), files)
    if (all(substr(files, nchar(files) - 3, nchar(files)) == ".tif")) {
      read_tif <- terra::rast(files)
    } else {
      stop(paste0(
        "\nCannot create a `SpatRaster` object of ", files, ".\n",
        "\nPerhaps the file download corrupted?\n",
        "\nYou might also check your cache directory for the files.\n"
      ))
    }
    return(read_tif)
  }

# Local internal functions
#' @noRd
.check_IDs <- function(product_id) {
  IDs <- c(
    "IDE00420",
    "IDE00421",
    "IDE00422",
    "IDE00423",
    "IDE00425",
    "IDE00426",
    "IDE00427",
    "IDE00430",
    "IDE00431",
    "IDE00432",
    "IDE00433",
    "IDE00435",
    "IDE00436",
    "IDE00437",
    "IDE00439"
  )

  if (product_id == "all") {
    product_id <- IDs
  } else if (product_id %in% IDs) {
    product_id <- product_id
  } else {
    stop(
      "\nA product ID matching what you entered, ",
      product_id,
      "\nwas not\n",
      "\nfound. Please check and try again.\n"
    )
  }
}

#' @noRd
.ftp_images <- function(product_id, bom_server) {
  # define custom useragent and handle for communicating with BOM servers
  USERAGENT <- paste0("{wrapique} R package (",
                      utils::packageVersion("wrapique"))
  # set a custom user-agent, restore original settings on exit
  # required for #130 - BOM returns 403 for RStudio
  op <- options()
  on.exit(options(op))
  options(HTTPUserAgent = USERAGENT)

  # BOM's FTP server can timeout too quickly
  # Also, BOM's http server sometimes sends a http response of 200, "all good",
  # but then will not actually serve the requested file, so we want to set a max
  # time limit for the complete process to complete as well.
  list_files <- curl::new_handle()
  curl::handle_setopt(handle = list_files,
                      FTP_RESPONSE_TIMEOUT = 60L,
                      CONNECTTIMEOUT = 60L,
                      TIMEOUT = 120L,
                      USERAGENT = USERAGENT,
                      ftp_use_epsv = TRUE,
                      dirlistonly = TRUE
  )

  # get file list from FTP server
  con <- curl::curl(
    url = bom_server,
    "r",
    handle = list_files
  )
  tif_files <- readLines(con)
  close(con)

  # filter only the GeoTIFF files
  tif_files <- tif_files[grepl("^.*\\.tif", tif_files)]

  # select the Product ID requested from list of files
  if (product_id != "all") {
    tif_files <- switch(
      product_id,
      "IDE00420" = {
        tif_files[grepl(
          "IDE00420",
          tif_files
        )]
      },
      "IDE00421" = {
        tif_files[grepl(
          "IDE00421",
          tif_files
        )]
      },
      "IDE00422" = {
        tif_files[grepl(
          "IDE00422",
          tif_files
        )]
      },
      "IDE00423" = {
        tif_files[grepl(
          "IDE00423",
          tif_files
        )]
      },
      "IDE00425" = {
        tif_files[grepl(
          "IDE00425",
          tif_files
        )]
      },
      "IDE00426" = {
        tif_files[grepl(
          "IDE00426",
          tif_files
        )]
      },
      "IDE00427" = {
        tif_files[grepl(
          "IDE00427",
          tif_files
        )]
      },
      "IDE00430" = {
        tif_files[grepl(
          "IDE00430",
          tif_files
        )]
      },
      "IDE00431" = {
        tif_files[grepl(
          "IDE00431",
          tif_files
        )]
      },
      "IDE00432" = {
        tif_files[grepl(
          "IDE00432",
          tif_files
        )]
      },
      "IDE00433" = {
        tif_files[grepl(
          "IDE00433",
          tif_files
        )]
      },
      "IDE00435" = {
        tif_files[grepl(
          "IDE00435",
          tif_files
        )]
      },
      "IDE00436" = {
        tif_files[grepl(
          "IDE00436",
          tif_files
        )]
      },
      "IDE00437" = {
        tif_files[grepl(
          "IDE00437",
          tif_files
        )]
      },
      tif_files[grepl(
        "IDE00439",
        tif_files
      )]
    )
    paste0(bom_server, tif_files)
  } else {
    tif_files
  }

  # check if the Product ID requested provides any files on server
  if (length(tif_files) == 0 |
      tif_files[1] == "ftp://ftp.bom.gov.au/anon/gen/gms/") {
    stop(paste0("\nSorry, no files are currently available for ", product_id))
  }
  return(tif_files)
}


# Export terra plot functionality to plot radar imagery
#' @importFrom terra plot
#' @export
terra::plot