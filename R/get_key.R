#' Get or Set Up API Keys
#'
#' Checks first to get key from your .Rprofile or .Renviron (or similar) file.
#'   If it's not found, then it suggests setting it up.  Can be used to check
#'   that your key that \R is using is the key that you wish to be using or for
#'   guidance in setting up the keys.
#'
#' @details
#' The suggestion is to use your .Renviron to set up the \acronym{API} keys.
#'  However, if you regularly interact with the APIs outside of \R using some
#'  other language you may wish to set these up in your .bashrc, .zshrc, or
#'  config.fish for cross-language use.
#'
#'
#' @param service (character) The \acronym{API} host i.e., \dQuote{DPIRD},
#'   \dQuote{SILO}, or \dQuote{METNO}.
#'
#' @return A string value with either a \acronym{DPIRD} Weather 2.0 API,
#'   \acronym{SILO} and \acronym{METNO} API key value.
#'
#' @examples
#' \dontrun{
#'   get_key(service = "DPIRD")
#'   get_key(service = "SILO")
#'   get_key(service = "METNO")
#' }
#'
#' @export
get_key <- function(service = c("DPIRD", "SILO", "METNO")) {
  service <- match.arg(toupper(service))

  key <- switch(
    service,
    DPIRD = {
      k <- Sys.getenv("DPIRD_API_KEY")
      if (!nzchar(k)) .set_dpird_key() else k
    },
    SILO = {
      k <- Sys.getenv("SILO_API_KEY")
      if (!nzchar(k)) .set_silo_key() else k
    },
    METNO = {
      k <- Sys.getenv("METNO_API_KEY")
      if (!nzchar(k)) .set_metno_key() else k
    }
  )

  return(key)
}

#' Help the User Request an API Key for the DPIRD API
#'
#' Opens a browser window at the DPIRD API key request URL and provides
#'   instruction on how to store the key. After filling the form you will get
#'   the key soon, but not immediately.
#'
#' @keywords internal
#' @noRd
#' @return Called for its side-effects, opens a browser window at the DPIRD
#'   weather data API key request form.
.set_dpird_key <- function() {
  if (interactive()) {
    utils::browseURL(
      "https://www.dpird.wa.gov.au/forms/dpird-api-registration/"
    )
  }

  stop(
    "You need to set your DPIRD API key.\n",
    "After getting your key set it as 'DPIRD_API_KEY' in .Renviron.\n",
    "DPIRD_API_KEY='youractualkeynotthisstring'\n",
    "For that, use `usethis::edit_r_environ()`",
    call. = FALSE
  )
}

#' Help the User Set Up Their SILO API Key
#'
#' Instructs the user on how to set up the SILO API key to automatically find
#'   it.
#'
#' @return Invisible `NULL`, called for its side-effects, returns a message
#'   with instructions.
#' @keywords internal
#' @noRd

.set_silo_key <- function() {
  stop(
    "Set your SILO API key (email address) as 'SILO_API_KEY' in .Renviron.\n",
    "SILO_API_KEY='youractualemailnotthisstring'\n",
    "For that, use `usethis::edit_r_environ()`",
    call. = FALSE
  )
}

#' Help the User Set Up Their METNO API Key
#'
#' Instructs the user on how to set up the METNO API key to automatically find
#'  it.
#'
#' @return Invisible `NULL`, called for its side-effects, returns a message
#' with instructions.
#' @keywords internal
#' @noRd

.set_metno_key <- function() {
  stop(
    "Set your METNO API key as 'METNO_API_KEY' in .Renviron.\n",
    "METNO_API_KEY='youractualkeynotthisstring'\n",
    "For that, use `usethis::edit_r_environ()`",
    call. = FALSE
  )
}
