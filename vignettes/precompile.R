# vignettes that depend on Internet access need to be precompiled and take a
# while to run
library(knitr)
knit(input = "vignettes/weatherOz.Rmd.orig",
     output = "vignettes/weatherOz.Rmd")

knit(input = "vignettes/weatherOz_for_BOM.Rmd.orig",
     output = "vignettes/weatherOz_for_BOM.Rmd")

knit(input = "vignettes/weatherOz_for_DPIRD.Rmd.orig",
     output = "vignettes/weatherOz_for_DPIRD.Rmd")

knit(input = "vignettes/use_case.Rmd.orig",
     output = "vignettes/use_case.Rmd")

# remove file path such that vignettes will build with figures
replace <- readLines("vignettes/weatherOz.Rmd")
replace <- gsub("<img src=\"vignettes/", "<img src=\"", replace)
# this replaces the .gif with .png extension, the radar .gif image is converted
# when knitting
replace <- gsub(".gif", ".png", replace)
fileConn <- file("vignettes/weatherOz.Rmd")
writeLines(replace, fileConn)
close(fileConn)

# remove file path such that vignettes will build with figures
replace <- readLines("vignettes/weatherOz_for_BOM.Rmd")
replace <- gsub("<img src=\"vignettes/", "<img src=\"", replace)
# this replaces the .gif with .png extension, the radar .gif image is converted
# when knitting
replace <- gsub(".gif", ".png", replace)
fileConn <- file("vignettes/weatherOz_for_BOM.Rmd")
writeLines(replace, fileConn)
close(fileConn)

# build vignettes
library(devtools)
build_vignettes()

# move resource files to /doc
resources <-
  list.files("vignettes/", pattern = ".png$", full.names = TRUE)
file.copy(from = resources,
          to = "doc",
          overwrite =  TRUE)
