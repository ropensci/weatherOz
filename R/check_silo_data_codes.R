
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
.check_silo_codes <- function(dt) {
  # TODO: grep for "source" in column names and check with ones need non-zeros
  # counted

  grep("source", names(dt))
  if (.this_format == "alldata") {
    code_cols <- c("Smx",
                   "Smn",
                   "Srn",
                   "Sev",
                   "Ssl",
                   "Svp",
                   "Ssp",
                   "Ses",
                   "Sp")

    # Count the number of non-zero rows for each new column
    non_zero_counts <-
      dt[, lapply(.SD, function(col)
        sum(col != 0)),
        .SDcols = code_cols]

  } else {
    # Split the 'code' column into separate columns for each quality code
    code_cols <- c("Ssl", "Smx", "Smn", "Srn", "Sev", "Svp")
    x <- data.table::copy(dt)
    x[, (code_cols) := data.table::tstrsplit(as.character(code),
                                             "",
                                             fixed = TRUE,
                                             type.convert = TRUE)]

    # Count the number of non-zero rows for each new column
    non_zero_counts <- x[, lapply(.SD, function(col)
      sum(col != 0)),
      .SDcols = code_cols]
  }

  if (any(non_zero_counts > 0)) {
    # Report message
    message(
      "You have requested station observation data but rows in this dataset",
      "have data codes of interpolated data. Check the data source columns and",
      "`get_patched_point()` or `get_data_drill()` documentation for further",
      "details on codes and references.\n"
    )
  }
  return(invisible(NULL))
}
