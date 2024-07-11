#' Get or Set Up API Keys
#'
#' Checks first to get key from your .Rprofile or .Renviron (or similar) file.
#'   If it's not found, then it suggests setting it up.  Can be used to check
#'   that your key that R is using is the key that you wish to be using or for
#'   guidance in setting up the keys.
#'
#' @details
#' The suggestion is to use your .Renviron to set up the API keys. However, if
#'   you regularly interact with the APIs outside of R using some other language
#'   you may wish to set these up in your .bashrc, .zshrc, or config.fish for
#'   cross-language use.
#'
#'
#' @param service (character) The \acronym{API} host, either \dQuote{DPIRD} or
#'   \dQuote{SILO}.
#'
#' @return A string value with either a \acronym{DPIRD} Weather 2.0 API or
#'   \acronym{SILO} API key value.
#'
#' @examples
#' \dontrun{
#'   get_key(service = "DPIRD")
#'   get_key(service = "SILO")
#' }
#'
#' @export
get_key <- function(service = c("DPIRD", "SILO")) {

  service <- rlang::arg_match(arg = service)

  if (service == "DPIRD") {
    DPIRD_API_KEY <- Sys.getenv("DPIRD_API_KEY")

    if (!nzchar(DPIRD_API_KEY)) {
      .set_dpird_key()
    } else {
      return(DPIRD_API_KEY)
    }
  } else {
    SILO_API_KEY <- Sys.getenv("SILO_API_KEY")
    if (!nzchar(SILO_API_KEY)) {
      .set_silo_key()
    } else {
      return(SILO_API_KEY)
    }
  }
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
      utils::browseURL("https://www.agric.wa.gov.au/form/dpird-api-registration")
    }

    stop(
        "You need to set your DPIRD API key.\n",
        "After getting your key set it as 'DPIRD_API_KEY' in .Renviron.\n",
        "DPIRD_API_KEY='youractualkeynotthisstring'\n",
        "For that, use `usethis::edit_r_environ()`"
    )

    invisible("https://www.agric.wa.gov.au/form/dpird-api-registration")
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
        "For that, use `usethis::edit_r_environ()`"
    )

    invisible(NULL)
  }
