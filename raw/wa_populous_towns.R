# Scrap list of most populous cities in Western Australia
list_url <- "https://en.wikipedia.org/wiki/List_of_places_in_Western_Australia_by_population"

url_bow <- polite::bow(list_url)

ind_html <-
  polite::scrape(url_bow) |>
  rvest::html_nodes("table.wikitable.sortable") |>
  rvest::html_table(fill = TRUE)

towns <- ind_html[[1]]

list_names <- towns[1, ] |> tolower()
names(towns) <- list_names

# Clean names and geocode locations
wa_populous_towns <- towns[-1, ] |>
  janitor::clean_names() |>
  dplyr::mutate(
    location = paste0(urban_centre, ", WA")
  ) |>
  tidygeocoder::geocode(location) |>
  dplyr::rename_with(.fn = tolower)

# Correct long/lat for Boddington-Ranford
# -32.79612, 116.48098
wa_populous_towns <- wa_populous_towns |>
  dplyr::mutate(
    long = dplyr::case_when(urban_centre == "Boddington-Ranford" ~ 116.48098,
                                 TRUE ~ long),
    lat = dplyr::case_when(urban_centre == "Boddington-Ranford" ~ -32.79612,
                                TRUE ~ lat)
  ) |>
  dplyr::rename(rank_position = rank,
                pop_census_2001 = x2001_census_6,
                pop_census_2006 = x2006_census_7,
                pop_census_2011 = x2011_census_8,
                pop_census_2016 = x2016_census_9,
                longitude = long,
                latitude = lat
  )

write.csv(wa_populous_towns, "raw/wa_populous_towns.csv")
usethis::use_data(wa_populous_towns, overwrite = TRUE)

