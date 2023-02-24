
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

#' @title SILO API query parser
#' @description This function takes results from an API query to the SILO
#' database and formats it to a flat data.frame or a list of data.frames if
#' querying multiple sites. The function also converts character columns to date
#' and numerical classes, according to the data represented in the column.
#' Note that for the 'monthly' `data_format` the function also renames the
#' columns from "Yr.Mth", "Avg TMax (oC)", "Avg TMin (oC)", "Tot Rain (mm)"
#' "Tot Evap (mm)", "Avg Rad (MJ/m2)", "Avg VP (hPa)" to "year_month", "
#' tmax_avg", "tmin_avg", "total_rainfall", "total_evap", "radiation_avg" and
#' "vapour_pressure_avg", respectively.
#' @param query_response data returned by the SILO API, usually a list of
#' character data.
#' @param this_format A string, user defined by the query details. One of
#' 'alldata', 'apsim' and 'monthly'. Internally inherited from
#' `get_silo_points()`.
#' @param this_date A string, user defined by the query details and represents
#' the start date of the query. Internally inherited from `get_silo_points()`.
#' @return A data.frame with date class column(s) and numeric class columns for
#' the weather variables.
#' @keywords internal
#'
#' @noRd

parse_silo <- function(query_response,
                       this_format,
                       this_date) {
  # apsim data
  if (toupper(this_format) == "APSIM") {
    # code snippet from {cropgrowdays}
    df <- strsplit(as.character(query_response), "\nyear")
    df <- as.character(df[[1]][2])
    df <- unlist(strsplit(df, "\n"))[-2]
    df <- gsub("\\s+", " ", df)
    out <- read.delim(textConnection(df), sep = " ")
    names(out)[1] <- "year"
    out$date <-
      as.Date(out[, "day"] - 1, paste0(out[, "year"], "-01-01"))
  }

  # monthly data
  if (this_format == 'monthly') {
    df <- unlist(strsplit(query_response, "\n"))
    n_first <-
      grep(format(lubridate::as_date(this_date), "%Y%m"), df)
    this_names <- c(
      "year_month",
      "tmax_avg",
      "tmin_avg",
      "total_rainfall",
      "total_evap",
      "radiation_avg",
      "vapour_pressure_avg"
    )

    # Create df and give names
    df <- df[n_first:length(df)]
    out <- stats::setNames(data.frame(matrix(
      nrow = length(df),
      ncol = length(this_names)
    )),
    nm = this_names)
    # Add data
    for (j in 1:length(df)) {
      out[j,] <- unlist(strsplit(df[j], "\\s+"))
    }

    # All columns were parsed as char, fix it
    # Select columns that have only numbers as obs
    num_cols <- unlist(lapply(names(out),
                              function(x)
                                all(grepl(
                                  "^[-0-9.]+$", out[[x]]
                                ))))
    # grab the column with dates
    date_col <- unlist(lapply(out,
                              function(x)
                                any(
                                  class(out) %in% c("POSIXct",
                                                    "POSIXt",
                                                    "Date")
                                )))
    # Put them together
    numeric_columns <- num_cols & !date_col
    for (i in which(numeric_columns)) {
      out[[i]] <- as.numeric(as.character(out[[i]]))
    }
  }

  # 'alldata' data (complete data with quality colums)
  if (this_format == "alldata") {
    df <- unlist(strsplit(query_response, "\n"))
    n_names <- grep("Date", df)
    n_first <- grep(this_date, df)
    this_names <- unlist(strsplit(df[n_names], "\\s+"))

    # Create df and provide names
    df <- df[n_first:length(df)]
    out <- stats::setNames(data.frame(matrix(
      nrow = length(df),
      ncol = length(this_names)
    )),
    nm = this_names)
    # Add data
    for (j in 1:length(df)) {
      out[j,] <- unlist(strsplit(df[j], "\\s+"))
    }

    # Set date columns to date class
    out$Date <- as.Date(out$Date, format = "%Y%m%d")
    out$Date2 <- as.Date(out$Date2, format = "%d-%m-%Y")

    # All columns were parsed as char, fix it
    # Select columns that have only numbers as obs
    num_cols <- unlist(lapply(names(out),
                              function(x)
                                all(grepl(
                                  "^[-0-9.]+$", out[[x]]
                                ))))

    # grab the column with dates
    date_col <- unlist(lapply(out,
                              function(x)
                                any(
                                  class(x) %in% c("POSIXct",
                                                  "POSIXt",
                                                  "Date")
                                )))
    # Put them together
    numeric_columns <- num_cols & !date_col
    for (i in which(numeric_columns)) {
      out[[i]] <- as.numeric(as.character(out[[i]]))
    }
  }
  return(out)
}
