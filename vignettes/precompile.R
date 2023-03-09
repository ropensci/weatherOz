# vignettes that depend on Internet access need to be precompiled and take a
# while to run
library(knitr)
knit(input = "vignettes/wrapique.Rmd.orig",
     output = "vignettes/wrapique.Rmd")

knit(input = "vignettes/use_case.Rmd.orig",
     output = "vignettes/use_case.Rmd")

# remove file path such that vignettes will build with figures
replace <- readLines("vignettes/wrapique.Rmd")
replace <- gsub("<img src=\"vignettes/", "<img src=\"", replace)
# this replaces the .gif with .png extension, the radar .gif image is converted
# when knitting
replace <- gsub(".gif", ".png", replace)
fileConn <- file("vignettes/wrapique.Rmd")
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
