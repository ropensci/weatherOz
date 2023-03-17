#' Print a weatherOz.ag.bulletin object
#'
#' Custom [print()] method for `weatherOz.ag.bulletin` objects.
#'
#' @param x a Defaults to `weatherOz.ag.bulletin` object.
#' @param ... ignored
#'
#' @export
#' @noRd

print.weatherOz_ag_bulletin_tbl <- function(x,
                                        quote = FALSE,
                                        ...) {
  .weatherOz_ag_header(x)
  print(
    data.table::as.data.table(x),
    topn = getOption("datatable.print.topn"),
    nrows = getOption("datatable.print.nrows"),
    class = getOption("datatable.print.class"),
    row.names = getOption("datatable.print.rownames")
  )
  invisible(x)
}

.weatherOz_ag_header <- function(x) {
  state <- attr(x, "state") %||% "UNKNOWN"
  product_id = attr(x, "product_id") %||% "UNKNOWN"
  .stylecat("  --- Australian Bureau of Meteorology (BOM) Ag Bulletin ---\n")
  .stylecat("  State:\t\t", state, "\n")
  .stylecat("  Please note information at the foot of,\n")
  .stylecat("  <http://www.bom.gov.au/cgi-bin/wrap_fwo.pl?",
            product_id,
            ".html>,\n"
  )
  .stylecat(". the HTML version of Agricultural Observations Bulletin for ",
            state,
            ".\n")
  .stylecat("  ", strrep("-", 63), "  \n")
}

.stylecat <- function(...) {
  cat(crayon::cyan(crayon::italic(paste0(...))))
}
