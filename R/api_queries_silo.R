# functions to query the SILO API to get weather data
# SILO   - https://silo.longpaddock.qld.gov.au

#' @describeIn weather.data Daily weather data from BOM owned weather stations,
#'   \code{bom.data.full} returns all of the provided data from the SILO data
#'   set. \code{Date} is returned in date format, \code{Date2} is returned as
#'   character, and the remaining variables are returned as numeric.
#'
#' @export

bom_data_full <- function(site,
                          first,
                          last = Sys.Date(),
                          email = NULL,
                          vars_string = NULL,
                          interval = "daily") {

  # parameter checking and cleaning
  if (missing(site)) stop("Site ID or lat/lon required")
  if (missing(first)) stop("Need to supply a start date")
  if (!length(site) %in% c(1, 2)) stop("site not correctly specified")
  if (is.null(email)) stop("no email address")
  m_int <- try(
    match.arg(
      interval,
      c("daily", "monthly"),
      several.ok = FALSE
    ),
    silent = TRUE
  )
  data_format <- switch(m_int,
    "daily"   = "alldata",
    "monthly" = "monthly"
  )

  # set up environment - allows for correct download of BOM data
  httr::set_config(
    httr::config(
      ssl_verifypeer = 0L
    )
  )

  # site is given as ID
  if (length(site) == 1) {
    api <- "https://longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php"
    # generate the source URI for the data
    g <- httr::GET(
      api,
      query = list(
        start = format(
          first,
          "%Y%m%d"
        ),
        finish = format(
          last,
          "%Y%m%d"
        ),
        station = site,
        format = data_format,
        username = email
      )
    )
  }

  # site is given as lat/long
  if (length(site) == 2) {
    api <- "https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php"
    # generate the source URI for the data
    g <- httr::GET(
      api,
      query = list(
        format = "alldata",
        lat = site[1],
        lon = site[2],
        username = email,
        password = "silo",
        start = format(
          first,
          "%Y%m%d"
        ),
        finish = format(
          last,
          "%Y%m%d"
        )
      )
    )
  }

  weather_data <- parsedata_silo_api(
    api.data = g,
    interval = m_int,
    first = first
  )

  # subset vars.string to happen here

  return(weather_data)
}
