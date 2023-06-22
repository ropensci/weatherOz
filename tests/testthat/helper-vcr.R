library("vcr")

vcr_dir <- vcr::vcr_test_path("fixtures")
vcr::vcr_configure(serialize_with = "json")

if (!nzchar(Sys.getenv("GITHUB_PAT"))) {
  if (dir.exists(vcr_dir)) {
    # Fake API token to fool our package
    Sys.setenv("GITHUB_PAT" = "foobar")
  } else {
    # If there's no mock files nor API token, impossible to run tests
    stop("No API key nor cassettes, tests cannot be run.",
         call. = FALSE)
  }
}

# Set up a fake API key if none is available in the environment
if (Sys.getenv("DPIRD_API_KEY") == "") {
  Sys.setenv("DPIRD_API_KEY" = "ou812")
}

Sys.setenv("VCR_VERBOSE_ERRORS" = TRUE)

invisible(vcr::vcr_configure(
  dir = vcr_dir,
  filter_sensitive_data =
    list("<<<dpird_api_key>>>" = Sys.getenv("DPIRD_API_KEY"))
))
