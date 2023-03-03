
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


#' Convert station names to proper case for names
#'
#' Converts station names to proper name case, e.g., "York East".
#'
#' @param s a `string` to be converted
#' @keywords internal
#' @noRd
#' @author \R authors, taken from ?tolower()

.cap_names <- function(s, strict = FALSE) {
  s <- tolower(s)
  cap <- function(s)
    paste(toupper(substring(s, 1, 1)),
          {
            s <- substring(s, 2)
            if (strict)
              tolower(s)
            else
              s
          },
          sep = "", collapse = " ")
  vapply(
    X = strsplit(s, split = " "),
    FUN = cap,
    FUN.VALUE = character(length(!is.null(names(s)))),
    USE.NAMES = !is.null(names(s))
  )
}


#' Get response from a BOM URL
#'
#' Gets response from a BOM URL, checks the server for response first, then
#' tries to fetch the file or returns an informative message.
#'
#' @param remote_file file resource being requested from BOM
#'
#' @details Original execution came from
#' <https://community.rstudio.com/t/internet-resources-should-fail-gracefully/49199/12>
#'
#' @author Adam H. Sparks, adam.sparks@@dpird.wa.gov.au
#' @noRd
#'
.get_url <- function(remote_file) {

  # define custom useragent and handle for communicating with BOM servers
  USERAGENT <- paste0("{wrapique} R package (",
                      utils::packageVersion("wrapique"),
                      ")")
  # set a custom user-agent, restore original settings on exit
  # required for #130 - BOM returns 403 for RStudio
  op <- options()
  on.exit(options(op))
  options(HTTPUserAgent = USERAGENT)

  # BOM's FTP server can timeout too quickly
  # Also, BOM's http server sometimes sends a http response of 200, "all good",
  # but then will not actually serve the requested file, so we want to set a max
  # time limit for the complete process to complete as well.
  h <- curl::new_handle()
  curl::handle_setopt(
    handle = h,
    FTP_RESPONSE_TIMEOUT = 60L,
    CONNECTTIMEOUT = 60L,
    TIMEOUT = 120L,
    USERAGENT = USERAGENT
  )

  try_GET <- function(x, ...) {
    tryCatch({
      response = curl::curl_fetch_memory(url = x, handle = h)
    },
    error = function(e)
      conditionMessage(e),
    warning = function(w)
      conditionMessage(w))
  }

  # a proper response will return a list class object
  # otherwise a timeout will just be a character string
  is_response <- function(x) {
    inherits(x, "list")
  }

  # First check Internet connection
  if (!curl::has_internet()) {
    stop(call. = FALSE,
         "No Internet connection.")
  }

  resp <- try_GET(x = remote_file)

  # check for possible timeout message and stop if that's the case
  if (!is_response(resp)) {
    stop(call. = FALSE,
         resp) # return char string value server provides
  }

  # Then stop if status indicates file not found
  if (as.integer(resp$status_code) == 404 |
      as.integer(resp$status_code) == 550) {
    stop(
      call. = FALSE,
      "\nA file or station was matched. However, a corresponding file was not ",
      "found at bom.gov.au.\n"
    )
  }

  if (tools::file_ext(remote_file) == "xml") {
    xml_out <- xml2::read_xml(rawToChar(resp$content))
    return(xml_out)
  }
  if (tools::file_ext(remote_file) == "json") {
    json_out <-
      jsonlite::fromJSON(rawToChar(resp$content))
    return(json_out)
  }
  if (grepl(pattern = "dailyZippedDataFile", x = remote_file)) {
    csv_out <-
      data.table::fread(input = remote_file,
                        header = TRUE,
                        stringsAsFactors = TRUE)
    return(csv_out)
  }
}

#' @noRd
# Check states for précis and ag bulletin, use fuzzy matching

.check_states <- function(state) {
  state <- toupper(state)

  states <- c(
    "ACT",
    "NSW",
    "NT",
    "QLD",
    "SA",
    "TAS",
    "VIC",
    "WA",
    "CANBERRA",
    "NEW SOUTH WALES",
    "NORTHERN TERRITORY",
    "QUEENSLAND",
    "SOUTH AUSTRALIA",
    "TASMANIA",
    "VICTORIA",
    "WESTERN AUSTRALIA",
    "AUSTRALIA",
    "AU",
    "AUS",
    "OZ"
  )

  if (state %in% states) {
    the_state <- state
    return(the_state)
  } else {
    likely_states <- agrep(pattern = state,
                           x = states,
                           value = TRUE)

    if (length(likely_states) == 1) {
      the_state <- toupper(likely_states)
      message(
        paste0(
          "\nUsing state = ",
          likely_states,
          ".\n",
          "If this is not what you intended, please check your entry."
        )
      )
      return(the_state)
    } else if (length(likely_states) == 0) {
      stop(
        "\nA state or territory matching what you entered was not found. ",
        "Please check and try again.\n"
      )
    }
  }

  if (length(likely_states) > 1) {
    message(
      "Multiple states match state.",
      "'\ndid you mean:\n\tstate = '",
      paste(likely_states[1],
            "or",
            likely_states[2],
            "or",
            likely_states[3]),
      "'?"
    )
  }
}

#' convert_state
#'
#' Convert state to standard abbreviation
#' @noRd
.convert_state <- function(state) {
  state <- gsub(" ", "", state)
  state <-
    substring(gsub("[[:punct:]]", "", tolower(state)), 1, 2)

  state_code <- c(
    "AUS",
    "AUS",
    "AUS",
    "NSW",
    "NSW",
    "VIC",
    "VIC",
    "QLD",
    "QLD",
    "QLD",
    "WA",
    "WA",
    "WA",
    "SA",
    "SA",
    "SA",
    "TAS",
    "TAS",
    "ACT",
    "NT",
    "NT"
  )
  state_names <- c(
    "au",
    "oz",
    "as",
    "ne",
    "ns",
    "vi",
    "v",
    "ql",
    "qe",
    "q",
    "wa",
    "we",
    "w",
    "s",
    "sa",
    "so",
    "ta",
    "t",
    "ac",
    "no",
    "nt"
  )
  state <- state_code[pmatch(state, state_names)]

  if (any(is.na(state)))
    stop("Unable to determine state")

  return(state)
}

#' splits time cols and removes extra chars for forecast XML objects
#'
#' @param x an object containing a BOM forecast object parsed from XML
#'
#' @return cleaned data.table cols of date and time
#' @keywords internal
#' @author Adam H. Sparks, \email{adam.sparks@@dpird.wa.gov.au}
#' @importFrom data.table ":="
#' @noRd

.split_time_cols <- function(x) {
  .SD <- start_time_local <- end_time_local <- NULL

  x[, c("start_time_local",
        "UTC_offset_drop") := data.table::tstrsplit(start_time_local,
                                                    "+",
                                                    fixed = TRUE)]

  x[, c("end_time_local",
        "utc_offset") := data.table::tstrsplit(end_time_local,
                                               "+",
                                               fixed = TRUE)]

  x[, "UTC_offset_drop" := NULL]

  # remove the "T" from time cols
  x[, c("start_time_local",
        "end_time_local",
        "start_time_utc",
        "end_time_utc") := lapply(.SD, gsub, pattern = "T",
                                  replacement = " "),
    .SDcols = c("start_time_local",
                "end_time_local",
                "start_time_utc",
                "end_time_utc")]

  # remove the "Z" from UTC cols
  x[, c("start_time_utc", "end_time_utc") := lapply(.SD, gsub, pattern = "Z",
                                                    replacement = ""),
    .SDcols = c("start_time_utc", "end_time_utc")]
  return(x)
}

#' Validate user entered filepath value or return BOM URL
#'
#' @param filepath User provided value for checking
#'
#' @noRd
.validate_filepath <- function(filepath) {
  if (is.null(filepath)) {
    location <- "ftp://ftp.bom.gov.au/anon/gen/fwo"
    return(location)
  } else {
    location <- trimws(filepath)
    if (!file.exists(location)) {
      stop("\nDirectory does not exist: ", filepath,
           call. = FALSE)
    } else if (tolower(tools::file_ext(location)) == "xml") {
      stop("\nYou have provided a file, not a directory containing a file.",
           call. = FALSE)
    }
    return(location)
  }
}

#' Create the base URL/file location of BOM files for all XML functions
#'
#' Takes the XML file name and creates the full file path or URL
#'
#' @param AUS_XML a vector of XML file names for BOM products
#' @param the_state user provided state argument for requested data
#'
#' @noRd

.create_bom_file <- function(AUS_XML, .the_state, .file_loc) {
  if (.the_state != "AUS") {
    xml_url <-
      data.table::fcase(
        .the_state == "ACT" |
          .the_state == "CANBERRA" ~ paste0(.file_loc, "/", AUS_XML[1]),
        .the_state == "NSW" |
          .the_state == "NEW SOUTH WALES" ~ paste0(.file_loc, "/", AUS_XML[1]),
        .the_state == "NT" |
          .the_state == "NORTHERN TERRITORY" ~ paste0(.file_loc,
                                                      "/", AUS_XML[2]),
        .the_state == "QLD" |
          .the_state == "QUEENSLAND" ~ paste0(.file_loc, "/", AUS_XML[3]),
        .the_state == "SA" |
          .the_state == "SOUTH AUSTRALIA" ~ paste0(.file_loc, "/", AUS_XML[4]),
        .the_state == "TAS" |
          .the_state == "TASMANIA" ~ paste0(.file_loc, "/", AUS_XML[5]),
        .the_state == "VIC" |
          .the_state == "VICTORIA" ~ paste0(.file_loc, "/", AUS_XML[6]),
        .the_state == "WA" |
          .the_state == "WESTERN AUSTRALIA" ~ paste0(.file_loc, "/", AUS_XML[7])
      )
  }
  return(xml_url)
}

#' Internal function to rename column names
#'
#' @param df_out data.frame returned from DPIRD API query with camel case
#' column names
#' @keywords internal
#' @noRd
#'

#' Internal function to rename column names
#'
#' @param df_out data.frame returned from DPIRD API query with camel case
#' column names
#' @which_api a string with the choosen API, either 'weather' or 'silo'
#' @keywords internal
#' @noRd
#'

.rename_cols <- function(df_out,
                         which_api = "dpird") {
  if (which_api == 'dpird') {
    df_out <- data.table::data.table(df_out)
    df_out[, stationName := .cap_names(s = stationName)]
    names(df_out)[1:2] <- c("station_code", "station_name")
  }

  if (which_api == 'silo') {
    df_out <- data.table::data.table(df_out)
    df_out[, name := .cap_names(s = name)]
    names(df_out)[1] <- "station_code"
    names(df_out)[3] <- "station_name"
  }

  return(df_out)
}

#' Check user-input longitude and latitude values for validity
#'
#' @param longitude user provided numeric value as decimal degrees
#' @param latitude user provided numeric value as decimal degrees
#' @noRd
#' @return invisible `NULL`, called for its side-effects

.check_lonlat <- function(longitude, latitude) {
  if (longitude < 114.5 | longitude > 152.5) {
    stop(
      call. = FALSE,
      "Please check your longitude, `",
      paste0(longitude),
      "`, to be sure it is valid for Australian data.\n"
    )
  }
  if (latitude < -23 | latitude > -38.5) {
    stop(
      call. = FALSE,
      "Please check your latitude, `",
      paste0(latitude),
      "`, value to be sure it is valid for Australian data.\n"
    )
    return(invisible(NULL))
  }
}
