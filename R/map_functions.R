# copies/variants of the weather map function originally written for the SSF by
# Fiona Evans. Modifications in the original package were made by Anna Hepworth.
# The 'map.ssf' function from that package is the 'map_weather' function; the
# masks for the South West Land Division (SWLD) are now used in map_weather_swld
# rather than being the defaults for the more general function.
#
# Note that the 'read logo' function is not included; the DPIRD logo is included
# in this package as a data object.

# Legend ----
#' Add legend
#'
#' Embed a legend in a plot. (vertical legend with top-left at left, top + u/2)
#' Note that this version is a combination of two pre-existing versions. One
#' version had a single scaling parameter, the second had two.
#' **There may still be some optimising to be had here.
#'
#' @param top Position of vertical centre of the top box of the legend. Top of
#'   the legend will be at top + u/2
#' @param left Position of left edge of legend.
#' @param col Data frame of discrete colours to use. If this does not contain a
#'   column called 'label', then the minimum and maximum values will be used to
#'   determine the breaks, and the values will be added at the breaks. ie. there
#'   will be one more value than there are colours.
#' @param u Scaling parameter for legend boxes (Default = 0.5). Boxes will be
#'   square with sides 'u' long.
#' @param cex Scaling parameter for the text. If this is not provided, it will
#'   be calculated as u/2, for backwards compatibility.
#' @param percent Should the legend values be converted to percentages? If true,
#'   the values are multiplied by 100 and '%' added. (Default = FALSE)
#'
#' @export

embed_legend <- function(top,
                         left,
                         col,
                         u = 0.5,
                         cex = NULL,
                         percent = FALSE) {
  if (percent) {
    colors$min[colors$min == -Inf] <- 0
  }

  if (is.null(cex)) {
    cex <- u * 2
  }

  # vertical legend with top-left corner at c(left, top + u/2)
  # the top left corner of the legend is defined by c(left, top + u/2)
  len <- nrow(col) # number of cells in the legend
  bottom <- top - (len - 1) * u # position: vertical middle, bottom cell
  right <- left + u # position: right vertical edge
  lines <- seq(bottom, top, u) # position: horizontal middles of the boxes

  # draw the squares of the legend.
  # values for lines are the central horizontal line of the box
  # plots from the bottom, using the colours of the colour set provided
  graphics::rect(
    left,
    lines - u / 2,
    right,
    lines + u / 2,
    col    = colors$col,
    border = "black"
  )

  if (is.null(colors$label)) { # add default labels
    vals <- sort(
      unique(c(
        colors$min,
        colors$max
      ))
    ) # identify and sort unique cut-points
    y_text <- seq(
      bottom - u / 2,
      top + u / 2,
      u
    )

    # these labels line up with the lines between colours
    # ie. they represent the boundaries between the categories
    if (percent) {
      # Note that this doesn't actually do anything sensible with Inf -> Inf %
      graphics::text(
        x = right + u / 4,
        y = y_text,
        pos = 4,
        labels = paste(100 * vals, "%"),
        cex = cex
      )
    } else {
      # default values which are not percent
      vals[length(vals)] <- paste(">", vals[length(vals)])
      graphics::text(
        x = right + u / 4,
        y = y_text,
        pos = 4,
        labels = vals,
        cex = cex
      )
    }
  } else {
    # add pre-specified labels
    graphics::text(
      x = right + u / 4,
      y = seq(bottom, top, u),
      pos = 4,
      labels = colors$label,
      cex = cex
    )
  }
}

# Create color data objects  ----
#' Make colour sets
#'
#' Create a colour set for use in \code{map.weather} from a set of n colours
#' and a set of n + 1 cut points. It is assumed that the lowest and highest
#' values in the break points are set so that the bottom and top segments are <
#' the second value and > second last value, respectively. There are multiple
#' 'default' colour sets available.
#'
#' @param col set of colours in any format recognised by R. One colour per
#'   grouping
#' @param cuts set of break points. Must contain one more item than the list of
#'   colours. \code{-Inf} and \code{Inf} are valid options for the minimum and
#'   maximum values, respectively.
#' @param key if a non-standard set of labels are required for the key, they can
#'   be supplied. The default set just uses the break points as the ranges.
#' @param sep Separator between number in key; default is hyphen
#'
#' @export

make_col_set <- function(col,
                         cuts,
                         key = NULL,
                         sep = "-") {
  n <- length(cuts) - 1
  a <- data.frame(
    colours = col,
    min = cuts[1:n],
    max = cuts[2:(n + 1)],
    stringsAsFactors = FALSE
  )

  # create the labels for each category
  # if none is provided, the default uses the boundaries of the category
  if (is.null(key)) {
    a$label <- paste0(a$min, sep, a$max)
    a$label[1] <- paste0("< ", a$max[1])
    a$label[n] <- paste0("> ", a$min[n])
  } else {
    a$label <- as.character(key)
  }
  return(a)
}

# New Plot ----
#' Opens a plot device
#'
#' Opens the specified type of device to fit a plot of the desired aspect ratio
#' and size. different plot devices require different values to get the same
#' sized plot. This has been specifically set up for the SWLD of WA, as
#' per the maps from the SSF. It is included in this package as an interim
#' function and is likely to be deprecated and removed in later versions.
#'
#' @param type plot type (0 = separate window, 1 = jpg, 4 = png). Default = 0.
#'   Note: png files will give a clearer plot and a smaller file for the same
#'   width/height parameters.
#' @param width plot width. These are converted internally; Finished measurement
#'   in inches is ~ width/10. Saved image files are slightly different from
#'   those opened in a new window. This is not a change to be dealt with now.
#' @param height plot height (units as per width)
#' @param name (something something file name; will have _probs and then file
#'   type appended) Defaults to NULL to allow for opening an unnamed window.
#' @param path directory in which the file will be saved for types 1 through 4.
#'   Default is NULL.
#' @param background background colour. Will only be relevant if type is 4.
#'
#' @export

weather_plot <- function(type = 0,
                         width,
                         height,
                         name = NULL,
                         path = NULL,
                         background = "white") {

  # assume that if path is given as blank it means current folder;
  # not required if plot type is 0
  if (type != 0) {
    if (path == "" | is.null(path)) {
      path <- "."
    }
  }

  # ** pointsize; gamma; xpos; ypos set for option 0 as per help file for x11;
  # these may need changing
  switch(as.character(type),
    "0" = {
      grDevices::x11(
        width = width / 10,
        height = height / 10,
        pointsize = 12,
        bg = background,
        gamma = 1,
        xpos = -25,
        ypos = 0,
        title = ""
      )
    },
    "1" = {
      save.file <- paste0(name, ".jpg")
      grDevices::jpeg(
        filename = file.path(path, save.file),
        width = width * 3,
        height = height * 3,
        bg = background
      )
    },
    "4" = {
      save.file <- paste0(name, ".png")
      grDevices::png(
        filename = file.path(path, save.file),
        width = width * 3,
        height = height * 3,
        bg = background
      )
    },
    message("Type is not supported")
  )
}

# Internal functions ----
#' Apply colours to vector
#'
#' Returns a vector of colours corresponding to the numerical values of input
#' vector. Colour sets can be one of the inbuilt sets (eg.
#' \code{mapcol_deciles}) or can be created using \code{make_col_set}
#'
#' @param x Input vector
#' @param col_df Data frame of discrete colours to use
#' @param trunc Should the range be truncated to within the range of the colour
#'   data frame? If true (default), values less than the minimum will be mapped
#'   as the minimum while values greater than the maximum will be mapped as the
#'   maximum. If false, values below minimum or above maximum will be set to NA.
#'   This is set to TRUE, to allow for correct implementation of legacy calls to
#'   the function.
#'
#' @keywords internal

color_df <- function(x,
                     col_df,
                     trunc = TRUE) {

  # Function for discrete colour table - identify the break points
  cuts <- sort(
    unique(
      c(
        col_df$min,
        col_df$max
    ))
  )

  # restrict the range of the provided vector, if 'truncate' has been selected.
  if (trunc) {
    x[x > max(cuts)] <- max(cuts)
    x[x < min(cuts)] <- min(cuts)
  }

  # map the x vector to the colour set
  indx <- as.numeric(
    cut(x,
      breaks = cuts,
      include.lowest = TRUE
    )
  )
  # convert all the items of x to colour values
  return(col_df[indx, "col"])
}

#' convert the kriged values to colours
#'
#' @param im object created by the 'map_krig' internal function
#' @param col mapping colour set; should be a data.frame. Can be created using
#'   \code{make_col_set}
#'
#' @keywords internal

convert_krig_to_col <- function(im,
                                col) {
  b <- matrix(
    color_df(
      x = im$z,
      col_df = col,
      trunc = TRUE
    ),
    nrow = nrow(im$z)
  )
  dimnames(b) <- list(
    unique(im$x),
    unique(im$y)
  )

  return(b)
}

#' Internal kriging functions
#'
#' A set of internal functions that are called by various mapping functions (as
#' of writing there is only the one, which does the whole SWWA).
#'
#' @param data data frame containing the variable to be krigged (identified by
#'   'varname') and latitude and longitude variables, which provide the spatial
#'   structure to the data.
#' @param varname String indicating which column is to be krigged.
#' @param lambda Smoothing parameter for the Kriging. Larger values will give
#'   greater smoothing
#' @param theta Kriging parameter (from the fields::Krig help file, the 'range
#'   parameter'). Default is 1.
#' @param long_lims longitudinal limits of mapping
#' @param lat_lims latitudinal limits of mapping
#'
#' @keywords internal

map_krig <- function(data,
                     varname,
                     lambda,
                     theta = 1,
                     lat_lims,
                     long_lims) {

  # standardise names
  names(data) <- tolower(names(data))
  varname <- tolower(varname)

  # fit a surface to the 'varname' data
  fit <- fields::Krig(
    x = data[, c("longitude", "latitude")],
    Y = data[[varname]],
    lambda = lambda,
    theta = theta
  )
# predict for all points in the grid, at 0.025 intervals
  im <- fields::predictSurface(
    object = fit,
    grid.list = list(
      x = seq(long_lims[1],
        long_lims[2],
        by = 0.025
      ),
      y = seq(lat_lims[1],
        lat_lims[2],
        by = 0.025
      )
    ),
    extrap = TRUE
  )

  return(im)
}

#' wrapper for map.krig
#'
#' this is the interim function between map.ssf and map.krig
#' so that the map layer is generated.
#'
#' @inheritParams map_krig
#' @param col specified colour data set needed to convert the continuous
#'   values to the categorical colour map
#' @param agregion required mask to remove mapping of the land outside the
#'   ag region
#' @param coast required mask to remove mapping of the ocean
#'
#' @export

map_krig_layer <- function(data,
                           varname,
                           lambda,
                           lat_lims,
                           long_lims,
                           col,
                           agregion,
                           coast) {

  # Interpolate using kriging
  # Note that this is not intuitively set up in terms of mapping because the
  # image maps that are added in the next step don't map that way.
  im <- map_krig(
    data = data,
    varname = varname,
    lambda = lambda,
    long_lims = long_lims,
    lat_lims = lat_lims
  )

  # convert to map
  # set the edges of the map; convert the Kriged values to colours
  # x <- unique(im$x)
  # y <- unique(im$y)
  b <- convert_krig_to_col(
    im = im,
    col = col
  )

  # Apply masks so that ocean and land outside the mask are not given a colour
  b[agregion == 1] <- NA
  b[coast == 1] <- NA
  # Rotate counter-clockwise 90 degrees
  b <- t(b)[ncol(b):1, ]

  return(b)
}
