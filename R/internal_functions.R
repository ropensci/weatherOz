
#' Check User Input Dates for Validity
#'
#' @param x User entered date value
#' @return Validated date string as a `POSIXct` object.
#' @note This was taken from \CRANpkg{nasapower}.
#' @example .check_date(x)
#' @author Adam H. Sparks \email{adamhsparks@@gmail.com}
#' @keywords Internal
#' @autoglobal
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

#' Check User Input Dates to Ensure Sequential Order
#'
#' @param .start_date a user supplied date for the start of data query
#' @param .end_date a user supplied date for the end of data query
#' @return Called for its side-effects, returns an invisible `NULL` if no error.
#'
#' @keywords Internal
#' @autoglobal
#' @noRd

.check_date_order <- function(.start_date, .end_date) {
  if (.end_date < .start_date) {
    stop(call. = FALSE,
         "The start and end dates appear to be reversed.")
  }
  if (.start_date > Sys.Date() || .end_date > Sys.Date()) {
    stop(
      call. = FALSE,
      "The `start_date` nor `end_date` values can neither one be past ",
      lubridate::today()
    )
  }
  return(invisible(NULL))
}

#' Check Against Earliest Available BOM Data From SILO
#'
#' @param start_date User-provided start date.
#'
#' @return No value, called for its side-effects checking if the `start_date` is
#'   before the earliest available data in the SILO database.
#' @noRd
#' @autoglobal
#' @keywords Internal
#'
.check_earliest_available_silo <- function(start_date) {
  if (start_date < lubridate::date("1889-01-01")) {
    stop(call. = FALSE,
         "The start date requested exceeds the earliest available data.")
  }
}

#' Check user inputs for lat, lon or station_code
#' @param .latitude latitude passed from another function
#' @param .longitude longitude passed from another function
#' @param .station_code station_code passed from another function
#'
#' @keywords Internal
#' @autoglobal
#' @noRd
#' @return Nothing, called for its side-effects
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

#' Check User Input for `lat`, `lon` or `station_code`
#' @param .latitude latitude passed from another function
#' @param .longitude longitude passed from another function
#'
#' @keywords Internal
#' @autoglobal
#' @noRd
#' @return A vector of longitude and latitude rounded to four decimal places

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
  }
  lonlat <- round(c("longitude" = longitude, "latitude" = latitude), 4)
}

#' Check that the user hasn't blindly copied the "your_api_key" string from the
#' examples
#'
#' @keywords Internal
#' @autoglobal
#' @noRd

.check_not_example_api_key <- function(.api_key) {
  if (!is.null(.api_key) && .api_key == "your_api_key") {
    stop("You have copied the example code and not provided a proper API key.
         An API key may be requested from DPIRD or for SILO you must use your
         e-mail address as an API key. See the help for the respective functions
         for more.",
         call. = FALSE)
  }
  return(invisible(NULL))
}

#' Check That the User Provided a Valid Email String as API Key for SILO
#' @param .api_key a user-provided value for the `api_key`, should be a valid
#'   e-mail address
#'
#' @keywords Internal
#' @autoglobal
#' @noRd

.is_valid_email_silo_api_key <- function(.api_key) {

  # regular expression to check
  pattern <- "\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>"

  if (grepl(pattern, as.character(.api_key), ignore.case = TRUE) &&
      !is.null(.api_key)) {
    return(invisible(NULL))
  } else {
    stop("For SILO requests you must use your e-mail address as an API key.
         You have not provided a valid email address.",
         call. = FALSE)
  }
}

#' Check That the User Provided a Valid DPIRD API Key String
#'
#' @param .api_key a user-provided value for the `api_key`. Should be a random
#'   string of text and numbers.
#'
#' @keywords Internal
#' @autoglobal
#' @noRd

.is_valid_dpird_api_key <- function(.api_key) {

  # regular expression to check
  pattern <- "\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>"

  if (grepl(pattern, as.character(.api_key), ignore.case = TRUE) &&
      is.null(.api_key)) {
    stop("For DPIRD requests you must use your DPIRD provided API key.
         You (may) have provided your e-mail address, which is used
         for the SILO API instead.",
         call. = FALSE)
  } else {
    return(invisible(NULL))
  }
}

#' Check User Input `state` for Précis Forecast and Ag Bulletin
#' @param state User provided value to check against.
#'
#' @return A validated state
#' @keywords Internal
#' @autoglobal
#' @noRd

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
    stop(
      call. = FALSE,
      "Multiple states match 'state', '",
      state,
      "'. Did you mean:\n\t`state` = ",
      likely_states[1],
      ", ",
      likely_states[2],
      ", or ",
      likely_states[3],
      "?"
    )
  }
}

#' Check User Input Values for Passing to the SILO API
#' @param .values User provided values to query from the API
#' @return A vector of verified values
#' @keywords Internal
#' @autoglobal
#' @noRd

.check_silo_values <- function(.values = values) {
  if (any(.values != "all") &&
      any(.values %notin% names(weatherOz::silo_daily_values))) {
    stop(call. = FALSE,
         "You have specified invalid weather values.")
  }

  if (any(.values == "all")) {
    .v <- unname(weatherOz::silo_daily_values)
  } else {
    .v <-
      weatherOz::silo_daily_values[names(weatherOz::silo_daily_values) %in%
                                     .values]
  }
  return(.v)
}

#' Check User Input API Values
#'
#' @param which_api user-provided value for `which_api`
#' @return A lower-case string of a valid API value
#' @keywords Internal
#' @autoglobal
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


#' Convert `state` to a Standard Abbreviation
#'
#' @param state A user-provided value for the state to be converted.
#' @return A `string` representing a proper abbreviation of an Australian state
#'   or territory.
#' @keywords Internal
#' @autoglobal
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
    stop("Unable to determine state",
         call. = FALSE)

  return(state)
}


#' Create the Base URL or File Location of BOM files for all XML Functions
#'
#' Takes the XML file name and creates the full file path or URL.
#'
#' @param AUS_XML a vector of XML file names for BOM products.
#' @param .the_state user provided state argument for requested data.
#' @param .file_loc file path for use with the `parse_` functions.
#'
#' @return A `string` value of the URL of the requested XML file on BOM's FTP
#'   server.
#' @keywords Internal
#' @autoglobal
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

#' Get A Response From a BOM URL
#'
#' Gets response from a BOM URL, checks the server for response first, then
#' tries to fetch the file or returns an informative message.
#'
#' @param remote_file file resource being requested from BOM
#'
#' @details Original execution came from
#' <https://community.rstudio.com/t/internet-resources-should-fail-gracefully/49199/12>
#'
#' @return An XML file (hopefully).
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @keywords Internal
#' @autoglobal
#' @noRd

.get_url <- function(remote_file) {

  op <- options(timeout = 600L)
  on.exit(options(op))

  bom_file <- file.path(tempdir(), "BOM_file.xml")

  try_GET <- function(x, ...) {
    tryCatch({
      utils::download.file(
        destfile = bom_file,
        url = x,
        mode = "wb"
      )
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

  if (tools::file_ext(remote_file) == "xml") {
    xml_out <- xml2::read_xml(bom_file)
    return(xml_out)
  }
}

#' Distance Over a Great Circle
#'
#' This is a reasonable approximation of distances over great circles for
#'   finding nearby stations using latitude and longitude.
#' @param lat1 The known station's latitude
#' @param lon1 The known stations' longitude
#' @param lat2 The latitude of the other station to determine the distance to
#'   from the known station location.
#' @param lon2 The longitude of the other station to determine the distance to
#'   from the known station location.
#'
#' @return A numeric value representing distance in kilometres.
#' @keywords Internal
#' @autoglobal
#' @noRd

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

#' Convert camelCase Names From the DPIRD API to snake_case
#'
#' @param x a `data.table` of results from a DPIRD Weather 2.0 API query with
#'  camelCase field names
#'
#' @return Modifies the the colnames of `x` in place
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @keywords Internal
#' @autoglobal
#' @noRd

.set_snake_case_names <- function(x) {
  if (isFALSE(inherits(x, "data.table"))) {
    stop(call. = FALSE,
         "This function only works on `data.tables`.")
  }
  data.table::setnames(x, old = names(x),
                       new = gsub(" ", "_", tolower(gsub(
                         "(.)([A-Z])", "\\1 \\2",
                         names(x)
                       ))))
  data.table::setnames(x,
                       old = names(x),
                       new = gsub(".", "_", names(x), fixed = TRUE))
}

#' Splits Time Cols and Removes Extra Chars for Forecast XML Objects
#'
#' @param x an object containing a BOM forecast object parsed from XML
#'
#' @return cleaned data.table cols of date and time
#' @keywords Internal
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
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
#'  \email{adamhsparks@@gmail.com}
#' @keywords Internal
#' @autoglobal
#' @noRd

.strcap <- function(x) {
  .cap <- function(x) {
    capped <- grep('^[^A-Z]*', x, perl = TRUE)
    substr(x[capped], 1, 1) <-
      toupper(tolower(substr(x[capped], 1, 1)))
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

  # correct station names that should have all caps in them or where words
  # should be lower case, e.g., "at" or "on".
  res <- gsub("\\bAct\\b", "ACT", res)
  res <- gsub("\\bAt\\b", "at", res)
  res <- gsub("\\bAws\\b", "AWS", res)
  res <- gsub("\\bBSL\\b", "BSL", res)
  res <- gsub("\\bDpi\\b", "DPI", res)
  res <- gsub("\\bDwr\\b", "DWR", res)
  res <- gsub("\\bHmsd\\b", "HMSD", res)
  res <- gsub("\\bMo\\b", "MO", res)
  res <- gsub("\\bNsw\\b", "NSW", res)
  res <- gsub("\\bNt\\b", "NT", res)
  res <- gsub("\\bOn\\b", "on", res)
  res <- gsub("\\bPirsa\\b", "PIRSA", res)
  res <- gsub("\\bQld\\b", "QLD", res)
  res <- gsub("\\bRaaf\\b", "RAAF", res)
  res <- gsub("\\bRsl\\b", "RSL", res)
  res <- gsub("\\bSFR\\b", "SFR", res)
  res <- gsub("\\bStp\\b", "STP", res)
  res <- gsub("\\bSa\\b", "SA", res)
  res <- gsub("\\bTas\\b", "TAS", res)
  res <- gsub("?<!^\\bThe\\b", "the", res)
  res <- gsub("\\bWa\\b", "\\bWA\\b", res)

  res[na] <- NA
  return(res)
}

#' Check User Input Filepath Value or Return BOM URL
#'
#' @param filepath User provided value for checking
#' @keywords Internal
#' @autoglobal
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
