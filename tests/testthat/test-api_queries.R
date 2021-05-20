# set up -----
# (currently none)

# weather.data ----
# test scenarios, minimal and incomplete
# * bom - from silo
# * bom - from mirror
# * dpird

# bom_data_full ----
#
# testing can't be done until parse_silo_data is completely transferred
#
# test scenarios, may not be complete
# site - any one is fine - missing is a failure. test with site number and with lat/lon
# first; last -- depends on the question, but maybe 1 month, 1 year, 10 years?
#   Also, with and without last provided
# email - with correct and incorrectly formatted; without
# vars_string - currently does nothing, shouldn't be tested
# interval currently allows daily and monthly; check that these both work

# bom_data_full(site,
#               first,
#               last = Sys.Date(),
#               email = NULL,
#               vars_string = NULL,
#               interval = "daily")

# failure modes
# site is missing
# start date is missing
