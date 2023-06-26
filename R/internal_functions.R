
#' Add %notin% function
#'
#' Negates `%in%` for easier (mis)matching.
#'
#' @param x A character string to match.
#' @param table A table containing values to match `x` against.
#'
#' @return A logical vector, indicating if a mismatch was located for any
#'  element of x: thus the values are TRUE or FALSE and never NA.
#' @keywords internal
#' @noRd
`%notin%` <- function(x, table) {
  match(x, table, nomatch = 0L) == 0L
}

#' Check user-provided dates for validity
#'
#' @param x User entered date value
#' @return Validated date string as a `POSIXct` object.
#' @note This was taken from \CRANpkg{nasapower}.
#' @example .check_date(x)
#' @author Adam H. Sparks \email{adam.sparks@@dpird.wa.gov.au}
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
                                    ),
                                    tz = Sys.timezone()),
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

#' Check user input dates to ensure sequential order
#'
#' @param .first a user supplied date for the start of data query
#' @param .last a user supplied date for the end of data query
#' @noRd

.check_date_order <- function(.start_date, .end_date) {
  if (.end_date < .start_date) {
    stop(call. = FALSE,
         "The start and end dates appear to be reversed.")
    return(invisible(NULL))
  }
  if (.start_date > Sys.Date() || .end_date > Sys.Date()) {
    stop(
      call. = FALSE,
      sprintf("The `start_date` nor `end_date` can neither one be past %s",
              lubridate::today())
    )
  }
}

#' Check user inputs for lat, lon or station_code
#' @param .latitude latitude passed from another function
#' @param .longitude longitude passed from another function
#' @param .station_code station_code passed from another function
#' @noRd
#' @return invisible `NULL`, called for its side-effects
.check_location_params <-
  function(.latitude, .longitude, .station_code) {
    if (((is.null(.latitude)) ||
         (is.null(.longitude))) && (is.null(.station_code))) {
      stop(
        call. = FALSE,
        "Provide valid `latitude` and `longitude` coordinates\n",
        "or a valid `station_code`."
      )
    }
    return(invisible(NULL))
  }

#' Check user-input longitude and latitude values for validity
#'
#' @param longitude user provided numeric value as decimal degrees
#' @param latitude user provided numeric value as decimal degrees
#' @noRd
#' @return invisible `NULL`, called for its side-effects

.check_lonlat <- function(longitude, latitude) {
  if (longitude < 112 || longitude > 154) {
    stop(
      call. = FALSE,
      "Please check your longitude, `",
      longitude,
      "`, to be sure it is valid for Australian data.\n"
    )
  }
  if (latitude < -44 || latitude > -10) {
    stop(
      call. = FALSE,
      "Please check your latitude, `",
      latitude,
      "`, value to be sure it is valid for Australian data.\n"
    )
    return(invisible(NULL))
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
          "Using state = ",
          likely_states,
          ".\n",
          "If this is not what you intended, please check your entry."
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
      "Multiple states match 'state', '",
      state,
      "'. Did you mean:\n\tstate = ",
      sprintf("'%s', '%s' or '%s?'",
              likely_states[1],
              likely_states[2],
              likely_states[3]
      ))
  }
}

#' Check user-input API value
#' @param which_api user-provided value for `which_api`
#' @return A lower-case string of a valid API value for \pkg{weatherOz}
#' @noRd
.check_which_api <- function(which_api) {
  which_api <- tolower(which_api)

  if (which_api %notin% c("all", "silo", "dpird")) {
    stop(
      call. = FALSE,
      "You have provided an invalid value for `which_api`.\n",
      "Valid values are 'all', 'silo' or 'dpird'."
    )
  }
  return(which_api)
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


#' Create the base URL/file location of BOM files for all XML functions
#'
#' Takes the XML file name and creates the full file path or URL
#'
#' @param AUS_XML a vector of XML file names for BOM products
#' @param .the_state user provided state argument for requested data
#' @param .file_loc file path for use with the `parse_` functions
#'
#' @noRd

.create_bom_file <- function(AUS_XML, .the_state, .file_loc) {
  if (.the_state != "AUS") {
    xml_url <-
      data.table::fcase(
        .the_state == "ACT" ||
          .the_state == "CANBERRA" ||
          .the_state == "NSW" ||
          .the_state == "New South Wales",
        sprintf("%s/%s", .file_loc, AUS_XML[1]),
        .the_state == "NT" ||
          .the_state == "NORTHERN TERRITORY",
        sprintf("%s/%s", .file_loc, AUS_XML[2]),
        .the_state == "QLD" ||
          .the_state == "QUEENSLAND",
        sprintf("%s/%s", .file_loc, AUS_XML[3]),
        .the_state == "SA" ||
          .the_state == "SOUTH AUSTRALIA",
        sprintf("%s/%s", .file_loc, AUS_XML[4]),
        .the_state == "TAS" ||
          .the_state == "TASMANIA",
        sprintf("%s/%s", .file_loc, AUS_XML[5]),
        .the_state == "VIC" ||
          .the_state == "VICTORIA",
        sprintf("%s/%s", .file_loc, AUS_XML[6]),
        default = sprintf("%s/%s", .file_loc, AUS_XML[7])
      )
  }
  return(xml_url)
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
#' @author Adam H. Sparks, \email{adam.sparks@@dpird.wa.gov.au}
#' @noRd

.get_url <- function(remote_file) {
  # define custom useragent and handle for communicating with BOM servers
  USERAGENT <- sprintf("{weatherOz} R package (%s)",
                       utils::packageVersion("weatherOz"))
  # set a custom user-agent, restore original settings on exit
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
    TCP_KEEPALIVE = 60L,
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

# Distance over a great circle. Reasonable approximation.
.haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # to radians
  lat1 <- lat1 * 0.01745
  lat2 <- lat2 * 0.01745
  lon1 <- lon1 * 0.01745
  lon2 <- lon2 * 0.01745

  delta_lat <- abs(lat1 - lat2)
  delta_lon <- abs(lon1 - lon2)

  # radius of earth
  12742 * asin(sqrt(`+`(
    (sin(delta_lat / 2)) ^ 2,
    cos(lat1) * cos(lat2) * (sin(delta_lon / 2)) ^ 2
  )))
}

#' Internal function to rename column names
#'
#' @param df_out data.frame returned from DPIRD API query with camel case
#' column names
#' @param which_api a string with the chosen API, either 'weather' or 'silo'
#' @keywords internal
#' @noRd
#'

.rename_cols <- function(df_out,
                         which_api = "dpird") {
  if (which_api == "dpird") {
    df_out <- data.table::data.table(df_out)
    df_out[, stationName := .strcap(x = stationName)]

    # Split the vector into two with an underscore between the names
    new_names <- lapply(names(df_out), function(x) {
      paste(strsplit(x,
                     "(?<=[a-z])(?=[A-Z])",
                     perl = TRUE)[[1]],
            collapse = "_")
    })

    # Transform to lower case and rename df_out
    names(df_out) <- gsub("[.]", "_", tolower(unlist(new_names)))
  }

  if (which_api == 'silo') {
    df_out <- data.table::data.table(df_out)
    df_out[, name := .strcap(x = name)]
    names(df_out)[1] <- "station_code"
    names(df_out)[3] <- "station_name"
  }

  return(df_out)
}

#' Convert camelCase names from DPIRD API to snake_case
#' @param x a `data.table` of results from a DPIRD Weather 2.0 API query with
#'  camelCase field names
#'
#' @return Modifies the the colnames of `x` in place
#' @author Adam H. Sparks, \email{adam.sparks@@dpird.wa.gov.au}
#' @keywords internal
#' @noRd

.set_snake_case_names <- function(x) {
  if (isFALSE(inherits(x, "data.table"))) {
    stop(call. = FALSE,
         "This function only works on `data.tables`.")
  }
  data.table::setnames(x, old = names(x),
                       new = gsub(" ", "_", tolower(
                         gsub("(.)([A-Z])", "\\1 \\2",
                              names(x))
                       )))
  data.table::setnames(x, old = names(x),
                       new = gsub(".", "_", names(x), fixed = TRUE))
}

#' splits time cols and removes extra chars for forecast XML objects
#'
#' @param x an object containing a BOM forecast object parsed from XML
#'
#' @return cleaned data.table cols of date and time
#' @keywords internal
#' @author Adam H. Sparks, \email{adam.sparks@@dpird.wa.gov.au}
#' @noRd

.split_time_cols <- function(x) {

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

#' Capitalise the First Letters of Words in a Vector of Strings
#'
#' @param x `vector` of `string` values to be capitalized.
#'
#' @note Taken from \CRANpkg{Hmisc} with modifications to simplify by
#'  Adam Sparks.
#'
#' @author Charles Dupont and Adam H. Sparks,
#'  \email{adam.sparks@@dpird.wa.gov.au}
#' @noRd

.strcap <- function(x) {

  .cap <- function(x) {
    capped <- grep('^[^A-Z]*', x, perl = TRUE)
    substr(x[capped], 1, 1) <- toupper(tolower(substr(x[capped], 1, 1)))
    x <- gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(x), perl = TRUE)
    return(x)
  }

  na <- is.na(x)

  # here `x` has to be wrapped in `as.character()` to handle some of the
  # station names in the BOM data when merging BOM metadata with SILO station
  # lists
  res <-
    unlist(lapply(lapply(
      strsplit(as.character(x), split = "\\b\\W+\\b"), .cap
    ),
    paste, collapse = " "))

  res[na] <- NA
  return(res)
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
