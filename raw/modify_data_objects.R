# modifying data objects
#

# SSF mapping objects ----
# These were all copied across, but the names needed changing;
# loaded them all in and then saved with the new names and the
# original .rda files deleted
agregion_img       <- agregion.img
agregion_lines     <- agregion.lines
coast_img          <- coast.img
coast_lines        <- coast.lines
shires_lines       <- shires.lines
towns_wa_grainbelt <- towns.wa.grainbelt

# create the files as data files.
usethis::use_data(
  agregion_img,
  overwrite = TRUE)
usethis::use_data(
  agregion_lines,
  overwrite = TRUE)
usethis::use_data(
  coast_img,
  overwrite = TRUE)
usethis::use_data(
  coast_lines,
  overwrite = TRUE)
usethis::use_data(
  shires_lines,
  overwrite = TRUE)
usethis::use_data(
  towns_wa_grainbelt,
  overwrite = TRUE)

