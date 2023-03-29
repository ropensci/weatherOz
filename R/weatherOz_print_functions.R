#' Print a weatherOz_tbl object
#'
#' Custom [print()] method for `weatherOz_tbl` objects.
#'
#' @param x a Defaults to `weatherOz_tbl` object.
#' @param ... ignored
#'
#' @export
#' @noRd

print.weatherOz_tbl <- function(x,
                                quote = FALSE,
                                ...) {
  .weatherOz_header(x)
  print(
    data.table::as.data.table(x),
    topn = getOption("datatable.print.topn"),
    nrows = getOption("datatable.print.nrows"),
    class = getOption("datatable.print.class"),
    row.names = getOption("datatable.print.rownames")
  )
  invisible(x)
}

.weatherOz_header <- function(x) {
  state <- c(attributes(x)$state)
  product_id <- c(attributes(x)$product_id)

  ## ag bulletin header ----
  if ("ag_bulletin" %in% attributes(x)) {
    .stylecat("  ", strrep("-", 11),
              "  Australian Bureau of Meteorology (BOM) Ag Bulletin. ",
              strrep("-", 11), "\n")
    .stylecat("  Please note information at the foot of:\n")
    for (i in product_id) {
      .stylecat("  <http://www.bom.gov.au/cgi-bin/wrap_fwo.pl?",
                i,
                ".html>\n")
    }
    .stylecat("  the HTML version of Agricultural Observations Bulletin for \n",
              "  ", knitr::combine_words(unlist(state)), ".",
              "\n")
    .stylecat(
      "  Also see: \n",
      "  <https://www.bom.gov.au/catalogue/observations/about-agricultural.shtml>",
      ".\n"
    )
    .stylecat("  ",
              strrep("-",
                     76),
              "  \n")
  }

  if ("coastal_forecast" %in% attributes(x)) {
    .stylecat("  ", strrep("-", 11),
              "  Australian Bureau of Meteorology (BOM) Coastal Waters Forecast. ",
              strrep("-", 11), "\n")
    .stylecat("  Please note information at the foot of:\n")
    for (i in product_id) {
      .stylecat("  <http://www.bom.gov.au/cgi-bin/wrap_fwo.pl?",
                i,
                ".html>\n")
    }
    .stylecat("  the HTML version of Coastal Waters Forecast for \n",
              "  ", knitr::combine_words(unlist(state)), ".",
              "\n")
    .stylecat(
      "  Also see \n",
      "  <http://www.bom.gov.au/catalogue/observations/about-coastal-observations.shtml>",
      ".\n"
    )
    .stylecat("  ",
              strrep("-",
                     89),
              "  \n")
  }

  if ("precis_forecast" %in% attributes(x)) {
    .stylecat("  ", strrep("-", 11),
              "  Australian Bureau of Meteorology (BOM) précis Forecast. ",
              strrep("-", 11), "\n")
    .stylecat("  The HTML version of Short Form (précis) Forecast for\n",
              "  ", knitr::combine_words(unlist(state)), " can be found at:\n")
    for (s in state) {
      .stylecat("  <www.bom.gov.au/",
                tolower(s),
                "/forecasts/state.shtml>\n")
    }
    .stylecat("  Please note information at the page \n",
              "  <http://www.bom.gov.au/catalogue/data-feeds.shtml#precis>",
              "\n")
    .stylecat("  ",
              strrep("-",
                     80),
              "  \n")
  }
}


#' Style header meta-information for BOM products
#'
#' Formats meta-information for users that BOM includes in the bulletins
#' using italics and DPIRD's dark red colour style
#'
#' @noRd
.stylecat <- function(...) {
  dpird_medium_red <-
    crayon::make_style(grDevices::rgb(0.58, 0.20, 0.13), bg = FALSE)
  cat(dpird_medium_red(crayon::italic(paste0(...))))
}
