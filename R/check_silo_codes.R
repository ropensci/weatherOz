
#' Check SILO data codes
#'
#' Checks if any SILO data codes for interpolated data are present in the
#'   requested station observation data. If any such codes are found, a message
#'   will be reported with a suggestion to check the data source columns
#'   and `get_silo()` documentation for further details on codes and references.
#'
#' @param dt A `data.table`, defaults to the SILO API query result object from
#'   `.query_silo()`.
#'
#' @return An `invisible(NULL)`. This function returns no value, only a friendly
#'   message. It is used for checking and reporting the presence of interpolated
#'   data codes in the station observation data (for API queries performed using
#'   a station_code/code).
#'
#' @noRd
#' @keywords internal

.check_silo_codes <- function(dt) {

  # these are the only cols that we need to be concerned about being
  # interpolated
  primary_cols <- c(
    "daily_rain_source",
    "max_temp_source",
    "min_temp_source",
    "vp_source",
    "evap_pan_source"
  )

  dt <- dt[, ..primary_cols]

  if (ncol(dt) > 0) {
    if (any(dt[, lapply(
      X = .SD,
      FUN = function(col)
        all(col == 0)
    )])) {
      # Report message
      message(
        "You have requested station observation data but some rows in this\n",
        "dataset have data codes for interpolated data.\n",
        "Check the 'data_source' columns and `get_patched_point()` or\n",
        "`get_data_drill()` documentation for further details on codes and\n",
        "references.\n"
      )
    }
  }
  return(invisible(NULL))
}
