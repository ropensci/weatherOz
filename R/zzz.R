
.onLoad <-
  function(libname = find.package("weatherOz"),
           pkgname = "weatherOz") {
    options(weatherOz.connection = stdin())

    # Check the operating system
    os <- Sys.info()[["sysname"]]

    # Define the query URLs
    if (os == "Windows") {
      base_dpird_url <- "https://api.agric.wa.gov.au/v2/"
    } else {
      base_dpird_url <- "https://api.dpird.wa.gov.au/v2/"
    }
  }

utils::globalVariables(".")

# set up a user agent string for API queries
user_agent <- paste0("weatherOz",
                     gsub(
                       pattern = "\\.",
                       replacement = "",
                       x = getNamespaceVersion("weatherOz")
                     ))
