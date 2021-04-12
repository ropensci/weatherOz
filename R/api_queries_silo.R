# functions to query the SILO API to get weather data
# SILO   - https://silo.longpaddock.qld.gov.au

#' @describeIn weather.data Daily weather data from BOM owned weather stations,
#'   \code{bom.data.full} returns all of the provided data from the SILO data
#'   set. \code{Date} is returned in date format, \code{Date2} is returned as
#'   character, and the remaining variables are returned as numeric.
#'
#' @export

# * need to look at https://www.longpaddock.qld.gov.au/silo/gridded-data/
#   to see whether any of those are simpler options for getting gridded data.
# 2019-017-22 - username and password removed. To add them back, the original function
# is in "R > deprecated > get.data.deprecated.R"
# 2020-12-14 - added the monthly option; extracted out internal functionality to keep
# this simpler

bom.data.full <- function(
  site,
  first,
  last = Sys.Date(),
  email = NULL,
  vars.string = NULL,
  interval = "daily") {

  # ** at present, vars.string doesn't do anything

  if (missing(site)) stop("Site ID or lat/lon required")
  if (missing(first)) stop("Need to supply a start date")
  if (!length(site) %in% c(1,2)) stop("site not correctly specified")
  if (is.null(email)) stop("no email address")
  m.int <- try(match.arg(interval,
                         c("daily", "monthly"),
                         several.ok = FALSE),
               silent = TRUE)
  data.format <- switch(m.int,
                        "daily" = "alldata",
                        "monthly" = "monthly")


  # set up environment - allows for correct download of BOM data
  httr::set_config(httr::config(ssl_verifypeer = 0L))

  # site is given as ID
  if (length(site) == 1) {
    api <- "https://longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php"
    #generate the source URI for the data
    g <- httr::GET(
      api,
      query = list(
        start    = format(first, "%Y%m%d"),
        finish   = format(last, "%Y%m%d"),
        station  = site,
        format   = data.format,
        username = email
      )
    )
  }

  # ** is this better as an else statement on the previous?
  # site is given as lat/long
  if (length(site) == 2) {
    api <- "https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php"
    #generate the source URI for the data
    g <- httr::GET(
      api,
      query = list(
        format   = "alldata",
        lat = site[1],
        lon = site[2],
        username = email,
        password = "silo",
        start    = format(first, "%Y%m%d"),
        finish   = format(last, "%Y%m%d")
      )
    )
  }

  weather.data <- parsedata_silo_api(
    api.data = g,
    interval = m.int,
    first = first)

  # # select the data from the source
  # r <- httr::content(g, "text")
  # s <- unlist(strsplit(r, "\n"))                  # split text file into lines
  # n.names <- grep("Date", s)                      # select the row with the names
  # n.first <- grep(format(first, "%Y%m%d"), s)     # identify row with first date of interest
  # nms     <- unlist(strsplit(s[n.names], "\\s+")) # get column names from row with names
  # s       <- s[n.first:length(s)]                 # remove header and get data (must happen after names are extracted)

  # # convert s into dataframe, splitting each line at the spaces
  # # initialise empty data frame. Because the data is all character at this point,
  # # use of matrix isn't losing any type information
  # temp <- stats::setNames(
  #   object = data.frame(
  #     matrix(nrow = length(s),
  #            ncol = length(nms)
  #     )),
  #   nm = nms)
  #
  # # split each row of s into component parts; place in columns of df note that
  # # while some version of 'apply' would be better, at least this works
  # for (j in 1:length(s)) {
  #   temp[j,] <- unlist(strsplit(s[j], "\\s+"))
  # }

  # # convert variables to useful data types
  # # Dates are hand converted; assumes that these will always be here.
  # temp$Date <- as.Date(temp$Date, "%Y%m%d")
  # temp$Date2 <- as.Date(temp$Date2, "%d-%m-%Y")

  # # use the inbuilt function to convert everything that should be to numeric
  # temp <- make.cols.numeric(temp)
  #
  # # subset vars.string
  # # ** currently not being done

  return(weather.data)
}



