# code to read in the dpird logo (which should have a copy in this folder)
# and then create a data object for the package.

# the 'read.logo' function has been copied 'as is' from the SSF package
# and used to read in the current dpird logo; it is not currently being included
# as a logo in the package, so the documentation has been commented out.

# ---- Logo ----
# Read logo from file
#
# Reads PNG format logo and returns as class 'raster'.
#
# @param filename Name of PNG file. If this is not provided, it is assumed that
  # the desired file is "DPIRD_black.png", and that this is in the working
  # directory.
  #
# @keywords hplot
# @export

read.logo <- function(
  filename = "DPIRD_black.png") {

  tmp <- png::readPNG(filename)
  grDevices::as.raster(tmp[, , 1:3])

}

dpird_logo <- read.logo(
  filename = "raw/DPIRD_black.png"
  )

# create the file as data file
usethis::use_data(
  dpird_logo,
  overwrite = TRUE)
