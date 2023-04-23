
.onLoad <-
  function(libname = find.package("weatherOz"),
           pkgname = "weatherOz") {
    options(bomrang.connection = stdin())
  }
utils::globalVariables(".")
