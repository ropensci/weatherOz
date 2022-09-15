#' Check dates for sequentialness and completeness
#'
#' Invisibly returns a `NULL` value if no errors are found, else stops with
#' an informative message providing the user with missing date values in the
#' sequence.
#'
#' @param x A vector of dates
#' @param y A vector of values with location name
#' @return A NULL value
#' @examples
#'
#' x <- seq(as.Date("2021-01-01"), as.Date("2021-01-31"), by = "1 day")
#' y <- "BA"
#' date_seq_check(x = x, y = y)
#' @author Adam H. Sparks, \email{adam.sparks@@dpird.wa.gov.au}
#' @export

date_seq_check <- function(x, y) {
  location <- y[[1]]
  date_range <- seq(min(x), max(x), by = "1 day")
  if (any(date_range %notin% x) || any(date_range != x)) {
    warning(
      call. = FALSE,
      "The weather data do not contain sequential dates, and/or date(s), ",
      paste(date_range[date_range %notin% x], collapse = ", "),
      ", is/are missing for ", location, ".\n"
    )
  }
  return(invisible(NULL))
}
