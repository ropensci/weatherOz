
.onLoad <-
  function(libname = find.package("weatherOz"),
           pkgname = "weatherOz") {
    options(weatherOz.connection = stdin())
  }

utils::globalVariables(".")
