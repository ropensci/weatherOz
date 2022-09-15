
#' Negate %in% for easy comparisons
#'
#' Check if values in `x` are also in `y`
#'
#' @param x a vector of values
#' @param y a vector of values of the same length as `x` for comparison
#' @example x %notin% y
#' @keywords internal
#' @return A vector of Boolean values the same length as `x` and `y`
#' @author Adam Sparks, \email{adam.sparks@@dpird.wa.gov.au}
#' @noRd
`%notin%` <- function(x, y) {
  match(x, y, nomatch = 0L) == 0L
}

#' Check user-provided dates for validity
#'
#' @param x User entered date value
#' @return Validated date string as a `POSIXct` object.
#' @note This was taken from \CRANpkg{nasapower}.
#' @example .check_date(x)
#' @author Adam Sparks, \email{adam.sparks@@dpird.wa.gov.au}
#' @keywords internal
#' @noRd
.check_date <- function(x) {
  tryCatch(
    x <- lubridate::parse_date_time(x,
                                    c(
                                      "Ymd",
                                      "dmY",
                                      "mdY",
                                      "BdY",
                                      "Bdy",
                                      "bdY",
                                      "bdy"
                                    )),
    warning = function(c) {
      stop(call. = FALSE,
           "\n",
           x,
           " is not in a valid date format. Please enter a valid date format.",
           "\n")
    }
  )
  return(x)
}
