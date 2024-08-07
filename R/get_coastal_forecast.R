
#' Get a BOM Coastal Waters Forecast
#'
#' Fetch the \acronym{BOM} daily Coastal Waters Forecast for a specified state
#'   or region.
#'
#' @param state Australian state or territory as full name or postal code.
#'   Fuzzy string matching via [base::agrep()] is done.  Defaults to `AUS`
#'   returning all state forecasts, see details for further information.
#'
#' @details Allowed state and territory postal codes, only one state per request
#'   or all using 'AUS':
#'    \describe{
#'      \item{AUS}{Australia, returns forecast for all states, NT and ACT}
#'      \item{ACT}{Australian Capital Territory (will return NSW)}
#'      \item{NSW}{New South Wales}
#'      \item{NT}{Northern Territory}
#'      \item{QLD}{Queensland}
#'      \item{SA}{South Australia}
#'      \item{TAS}{Tasmania}
#'      \item{VIC}{Victoria}
#'      \item{WA}{Western Australia}
#'  }
#'
#' @return
#' A [data.table::data.table()] of an Australia \acronym{BOM} Coastal Waters
#'   Forecast.
#'
#' @examplesIf interactive()
#'
#' get_coastal_forecast(state = "NSW")
#'
#' @references
#' Forecast data come from Australian Bureau of Meteorology (BOM) Weather Data
#'   Services \cr <http://www.bom.gov.au/catalogue/data-feeds.shtml>.
#'
#' And also,
#'
#' Location data and other metadata come from the \acronym{BOM} anonymous
#'   \acronym{FTP} server with spatial data \cr
#'   <ftp://ftp.bom.gov.au/anon/home/adfd/spatial/>, specifically the
#'   \acronym{DBF} file portion of a shapefile, \cr
#'   <ftp://ftp.bom.gov.au/anon/home/adfd/spatial/IDM00003.dbf>.
#'
#' @author Dean Marchiori, \email{deanmarchiori@@gmail.com}, and Paul Melloy,
#'   \email{paul@@melloy.com.au}
#'
#' @family BOM
#' @family data fetching
#'
#' @seealso [parse_coastal_forecast]
#' @autoglobal
#' @export get_coastal_forecast

get_coastal_forecast <- function(state = "AUS") {


  # see internal_functions.R for these functions
  the_state <- .check_states(state)

  # `NULL` is used for functionality with parse_coastal_forecast(),
  # this just creates `location` with a string of
  # <ftp://ftp.bom.gov.au/anon/gen/fwo>
  location <- .validate_filepath(filepath = NULL)
  coastal_out <-
    .return_coastal(file_loc = location, cleaned_state = the_state)

  return(
    structure(
      coastal_out,
      class = union("weatherOz_tbl", class(coastal_out)),
      state = unique(coastal_out[, "state_code"]),
      product_id = unique(coastal_out[, "product_id"]),
      tbl_type = "coastal_forecast"
    )
  )
}

# Coastal forecast functions for get() and parse()------------------------------

.return_coastal <- function(file_loc, cleaned_state) {
  # create vector of XML files
  AUS_XML <- c(
    "IDN11001.xml",
    # NSW
    "IDD11030.xml",
    # NT
    "IDQ11290.xml",
    # QLD
    "IDS11072.xml",
    # SA
    "IDT12329.xml",
    # TAS
    "IDV10200.xml",
    # VIC
    "IDW11160.xml"  # WA
  )
  if (cleaned_state != "AUS") {
    xml_url <- .create_bom_file(AUS_XML,
                                .the_state = cleaned_state,
                                .file_loc = file_loc)
    coastal_out <- .parse_coastal_forecast(xml_url)
    if (is.null(coastal_out)) {
      return(invisible(NULL))
    }
    return(coastal_out[])
  } else {
    file_list <- sprintf("%s/%s", file_loc, AUS_XML)
    coastal_out <-
      lapply(X = file_list, FUN = .parse_coastal_forecast)
    coastal_out <- data.table::rbindlist(coastal_out, fill = TRUE)
    return(coastal_out[])
  }
}

#' Parse the Coastal Forecast XML File From BOM
#' @noRd
#' @keywords Internal
#' @autoglobal
.parse_coastal_forecast <- function(xml_url) {

  op <- options(timeout = 600L)
  on.exit(options(op))

  # load the XML from ftp
  if (substr(xml_url, 1, 3) == "ftp") {
    xml_object <- .get_url(xml_url)
    if (is.null(xml_object)) {
      return(invisible(NULL))
    }
  } else {
    # load the XML from local
    xml_object <- xml2::read_xml(xml_url)
  }
  out <- .parse_coastal_xml(xml_object)
  # clean up and split out time cols into offset and remove extra chars
  .split_time_cols(x = out)

  dbf_file <- file.path(tempdir(), "marine_AAC_codes.dbf")
  on.exit(unlink(dbf_file))

  utils::download.file(
    "ftp://ftp.bom.gov.au/anon/home/adfd/spatial/IDM00003.dbf",
    destfile = dbf_file,
    mode = "wb",
    quiet = TRUE
  )

  marine_AAC_codes <-
    data.table::data.table(foreign::read.dbf(dbf_file, as.is = TRUE))

  # convert names to lower case for consistency with bomrang output
  data.table::setnames(marine_AAC_codes,
                       old = names(marine_AAC_codes),
                       new = (tolower(names(marine_AAC_codes))))

  marine_AAC_codes <- marine_AAC_codes[, c(1, 3, 4:7)]
  data.table::setkey(marine_AAC_codes, "aac")

  # merge with aac codes for location information
  data.table::setkey(out, "aac")
  out <- marine_AAC_codes[out, on = c("aac", "dist_name")]
  # add state field
  out[, state_code := gsub("_.*", "", out$aac)]
  # add product ID field
  out[, product_id := substr(basename(xml_url),
                             1,
                             nchar(basename(xml_url)) - 4)]
  # some fields only come out on special occasions, if absent, add as NA
  if (!"forecast_swell2" %in% colnames(out)) {
    out[, forecast_swell2 := NA]
  }
  if (!"forecast_caution" %in% colnames(out)) {
    out[, forecast_caution := NA]
  }
  if (!"marine_forecast" %in% colnames(out)) {
    out[, marine_forecast := NA]
  }
  if (!"tropical_system_location" %in% colnames(out)) {
    out[, tropical_system_location := NA]
  }
  if (!"forecast_waves" %in% colnames(out)) {
    out[, forecast_waves := NA]
  }
  # reorder columns
  refcols <- c(
    "index",
    "product_id",
    "type",
    "state_code",
    "dist_name",
    "pt_1_name",
    "pt_2_name",
    "aac",
    "start_time_local",
    "end_time_local",
    "utc_offset",
    "start_time_utc",
    "end_time_utc",
    "forecast_seas",
    "forecast_weather",
    "forecast_winds",
    "forecast_swell1",
    "forecast_swell2",
    "forecast_caution",
    "marine_forecast",
    "tropical_system_location",
    "forecast_waves"
  )
  data.table::setcolorder(out, refcols)
  # set col classes
  # factors
  out[, c(1, 11) := lapply(.SD, function(x)
    as.factor(x)),
    .SDcols = c(1, 11)]
  out[, c(9:10) := lapply(.SD, function(x)
    as.POSIXct(x,
               origin = "1970-1-1",
               format = "%Y-%m-%d %H:%M:%OS")),
    .SDcols = c(9:10)]
  out[, c(12:13) := lapply(.SD, function(x)
    as.POSIXct(
      x,
      origin = "1970-1-1",
      format = "%Y-%m-%d %H:%M:%OS",
      tz = "GMT"
    )),
    .SDcols = c(12:13)]
  # character
  out[, c(6:8, 14:20) := lapply(.SD, function(x)
    as.character(x)),
    .SDcols = c(6:8, 14:20)]

  return(out[])
}

#' extract the values of a coastal forecast item
#'
#' @param xml_object coastal forecast XML object
#'
#' @return a `data.table` of the forecast for further refining
#' @keywords internal
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @noRd

.parse_coastal_xml <- function(xml_object) {
  # get the actual forecast objects
  meta <- xml2::xml_find_all(xml_object, ".//text")
  fp <- xml2::xml_find_all(xml_object, ".//forecast-period")
  locations_index <- data.table::data.table(
    # find all the aacs
    aac = xml2::xml_parent(meta) |>
      xml2::xml_find_first(".//parent::area") |>
      xml2::xml_attr("aac"),
    # find the names of towns
    dist_name = xml2::xml_parent(meta) |>
      xml2::xml_find_first(".//parent::area") |>
      xml2::xml_attr("description"),
    # find forecast period index
    index = xml2::xml_parent(meta) |>
      xml2::xml_find_first(".//parent::forecast-period") |>
      xml2::xml_attr("index"),
    start_time_local = xml2::xml_parent(meta) |>
      xml2::xml_find_first(".//parent::forecast-period") |>
      xml2::xml_attr("start-time-local"),
    end_time_local = xml2::xml_parent(meta) |>
      xml2::xml_find_first(".//parent::forecast-period") |>
      xml2::xml_attr("start-time-local"),
    start_time_utc = xml2::xml_parent(meta) |>
      xml2::xml_find_first(".//parent::forecast-period") |>
      xml2::xml_attr("start-time-local"),
    end_time_utc = xml2::xml_parent(meta) |>
      xml2::xml_find_first(".//parent::forecast-period") |>
      xml2::xml_attr("start-time-local")
  )
  vals <- lapply(fp, function(node) {
    # find names of all children nodes
    childnodes <- node |>
      xml2::xml_children() |>
      xml2::xml_name()
    # find the attr value from all child nodes
    names <- node |>
      xml2::xml_children() |>
      xml2::xml_attr("type")
    # create columns names based on either node name or attr value
    names <- ifelse(is.na(names), childnodes, names)
    # find all values
    values <- node |>
      xml2::xml_children() |>
      xml2::xml_text()
    # create data frame and properly label the columns
    df <- data.frame(t(values), stringsAsFactors = FALSE)
    names(df) <- names
    df
  })
  vals <- data.table::rbindlist(vals, fill = TRUE)
  sub_out <- cbind(locations_index, vals)
  if ("synoptic_situation" %in% names(sub_out)) {
    sub_out[, synoptic_situation := NULL]
  }
  if ("preamble" %in% names(sub_out)) {
    sub_out[, preamble := NULL]
  }
  if ("warning_summary_footer" %in% names(sub_out)) {
    sub_out[, warning_summary_footer := NULL]
  }
  if ("product_footer" %in% names(sub_out)) {
    sub_out[, product_footer := NULL]
  }
  if ("postamble" %in% names(sub_out)) {
    sub_out[, postamble := NULL]
  }
  return(sub_out)
}
