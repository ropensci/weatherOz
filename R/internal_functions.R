
#' Negate %in% for easy comparisons
#'
#' Check if values in `x` are also in `y`
#'
#' @param x a vector of values
#' @param y a vector of values of the same length as `x` for comparison
#' @example x %notin% y
#' @keywords internal
#' @return A vector of Boolean values the same length as `x` and `y`
#' @author Adam Sparks, adam.sparks@@dpird.wa.gov.au
#' @noRd
`%notin%` <- function(x, y) {
  match(x, y, nomatch = 0L) == 0L
}
