#' DPIRD logo
#'
#' Black and white logo for the Department of Primary Industries and Regional
#' Development. Current as of May 2021.
#'
#' @name dpird_logo
#' @docType data
#' @keywords datasets
#'
#' @format raster
#' @source version in use in the SSF copied; original source forgotten

"dpird_logo"

# Fiona's map objects ----
#' Ocean image mask
#'
#' Image mask to restrict mapping to land areas of south west Western Australia
#'
#' @name coast_img
#' @docType data
#' @keywords datasets
#'
#' @format a 381 * 321 matrix consisting of zeroes/ones.
#' @source copied from the SSF: unknown -- originally sourced or created by
#'   Fiona Evans

"coast_img"

#' Agricultural areas image mask
#'
#' Image mask to restrict mapping to agricultural areas of south west Western
#' Australia, including both the grainbelt and the lower south-west corner.
#'
#' @name agregion_img
#' @docType data
#' @format a 381 * 321 matrix consisting of zeroes/ones.
#' @keywords datasets
#' @source copied from the SSF: unknown -- originally sourced or created by
#'   Fiona Evans.

"agregion_img"

# mapping lines ----
#' South west WA coast line
#'
#' Set of line segments that together make up the WA coast line.
#' ** currently includes some boundary lines on the ocean -- plan is to get rid
#' of those
#' ** format needs to be sorted, and saved as something else.
#'
#' @name coast_lines
#' @docType data
#' @format  ...
#' @keywords datasets
#' @source copied from the SSF: unknown -- originally sourced or created by
#'   Fiona Evans.

"coast_lines"

#' South west shire boundaries
#'
#' Set of line segments that make up the south west WA shire boundaries
#' ** current version has bits that go outside the agricultural region
#' ** format needs sorting out
#'
#' @name shires_lines
#' @docType data
#' @format  ...
#' @keywords datasets
#' @source copied from the SSF: unknown -- originally sourced or created by
#'   Fiona Evans.

"shires_lines"

#' Grainbelt inland border
#'
#' Set of line segments that together make up the inland border of the
#' south-west WA grainbelt region.
#' ** format needs sorting out.
#'
#' @name agregion_lines
#' @docType data
#' @format  ...
#' @keywords datasets
#' @source copied from the SSF: unknown -- originally sourced or created by
#'   Fiona Evans.

"agregion_lines"

# Named locations ----
#' WA grainbelt towns
#'
#' Default set of location names to be added to grainbelt region maps
#' @name towns_wa_grainbelt
#' @docType data
#' @format  ...
#' @keywords datasets
#' @source copied from the SSF, from the grainbelt stations list.

"towns_wa_grainbelt"

# DPIRD office locations ----
#' Localities with a DPIRD office
#'
#' Default set of office locations to be added region maps
#' @name dpird_office_towns
#' @docType data
#' @format  ...
#' @keywords datasets
#' @source create based on current office locations
#' (https://www.agric.wa.gov.au/office-locations).

"dpird_office_towns"

# WA most populous towns ----
#' List of WA towns by population
#'
#' Default set locations to be added region maps
#' @name wa_populous_towns
#' @docType data
#' @format  ...
#' @keywords datasets
#' @source create by scrapping list of most populous places in WA from Wikipedia
#' (https://en.wikipedia.org/wiki/List_of_places_in_Western_Australia_by_population).

"wa_populous_towns"
