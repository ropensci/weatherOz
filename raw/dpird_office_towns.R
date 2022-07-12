#  DPIRD office locations from https://www.agric.wa.gov.au/office-locations
create_dpird_offices <- function() {

  office_locations <- c("Perth",
                        "Albany",
                        "Broome",
                        "Bunbury",
                        "Canning Vale",
                        "Carnarvon",
                        "Derby",
                        "Esperance",
                        "Geraldton",
                        "Kalgoorlie",
                        "Karratha",
                        "Katanning",
                        "Kununurra",
                        "Manjimup",
                        "Merredin",
                        "Moora",
                        "Mount Barker",
                        "Narembeen",
                        "Narrogin",
                        "Norseman (Eucla checkpoint)",
                        "Northam",
                        "South Perth",
                        "Southern Cross",
                        "Wongan Hills")

  # Build a data frame with town and create a location column and then geocode locations
  dpird_office_towns <-
  data.frame(town = office_locations) |>
    dplyr::mutate(
      location = paste0(office_locations, ", WA"),
      location = dplyr::case_when(location == "Norseman (Eucla checkpoint), WA" ~ "Norseman, WA",
                                  TRUE ~ location
      )
    ) |>
    tidygeocoder::geocode(location) |>
    dplyr::transmute(
      town,
      location,
      longitude = dplyr::case_when(town == "Derby" ~ 123.6402,
                            TRUE ~ long),
      latitude = dplyr::case_when(town == "Derby" ~ -17.3093,
                           TRUE ~ lat)
    )

  return(dpird_office_towns)
}

dpird_office_towns <- create_dpird_offices()

write.csv(dpird_office_towns, "raw/dpird_office_towns.csv")
usethis::use_data(dpird_office_towns, overwrite = TRUE)
