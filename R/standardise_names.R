
#' Standardise Column Names and Row Bind DPIRD and SILO Data
#'
#' Takes _daily_ \acronym{DPIRD} or \acronym{SILO} weather data returned by
#'   `get_dpird_summary()`, `get_silo_data_drill()` or
#'   `get_silo_patched_point()` and standardises the column names and if
#'   multiple stations are provided, performs `rbind()` returning a unified
#'   \CRANpkg{data.table} object.  All columns of the original data are
#'   retained if they are not present between the APIs, _i.e._, only those data
#'   that have corresponding values in each \acronym{API} are renamed, all
#'   others are kept as-is or filled with `NA`.
#'
#' @param x A [data.table::data.table] object containing daily weather data
#'   returned from `get_dpird_summary()`, `get_silo_data_drill()` or
#'   `get_silo_patched_point()`.
#'
standardise_names <- function(x) {

}
