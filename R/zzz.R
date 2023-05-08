
.onLoad <-
  function(libname = find.package("weatherOz"),
           pkgname = "weatherOz") {
    options(weatherOz.connection = stdin())
  }

utils::globalVariables(".")

# set up a user agent string for API queries
user_agent <- paste0("weatherOz",
                     gsub(
                       pattern = "\\.",
                       replacement = "",
                       x = getNamespaceVersion("weatherOz")
                     ))
