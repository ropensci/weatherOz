# code to read in the dpird logo (which should have a copy in this folder)
# and then create a data object for the package.

# ---- Logo ----
#' Read logo from file
#'
#' Reads PNG format logo and returns as class 'raster'.
#'
#' @param filename Name of PNG file. If this is not provided, it is assumed that
#'   the desired file is "DPIRD_black.png", and that this is in the working
#'   directory.
#'
#' @keywords hplot
#' @export

read.logo <- function(
  filename = "DPIRD_black.png") {

  tmp <- png::readPNG(filename)
  grDevices::as.raster(tmp[, , 1:3])

}

