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
#' Create a colour set for use in \code{map_weather} from a set of n colours
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
      )
    )
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

# ---- Maps ----
#' Gradient map of South-West WA.
#'
#' Generic function for plotting South West WA SSF-style maps. Plots a spatial
#' map, ensuring that the x and y axes are scaled equally. Plots coloured
#' rectangles so that plot area is properly filled.
#'
#' @param data Data frame with columns for the latitude and longitude locations
#'   of the points, and the measurement of \code{varname} at those points. Other
#'   columns will be ignored.
#' @param varname Name of variable to map
#' @param col.dataframe Data frame containing discrete color information. For
#'   \code{map.ssf.dots} the defaults is NULL. When this is left as NULL, then a
#'   rainbow of colours is applied to the unique values. Note that this may give
#'   strange results if the variable to be plotted is factor and the factor
#'   order is not in the desired legend order.
#' @param percent Color scale in percentages? If this is true, the legend values
#'   will be assumed to be percentages, and the percent symbol appended, unless
#'   labels are included in the \code{col.dataframe}
#' @param title  Main title for the map. This will be the first line of text in
#'   the finished map (required)
#' @param subtitle1 The first line of the map subtitle (required). To leave this
#'   blank, use 'subtitle1 = ""'.
#' @param subtitle2 The second line of the map subtitle (required). To leave
#'   this blank, use 'subtitle2 = ""'.
#' @param subtitle3 Third subtitle for the map
#' @param dots Data frame containing the latitude and longitude of any points to
#'   plot. It is assumed that these will have the variable names 'LATITUDE' and
#'   'LONGITUDE'. (not case sensitive?). If type is 'point' and this parameter
#'   is provided, then only the points that are in both will be plotted.
#' @param dots.label Data frame containing the latitude, longitude, and text for
#'   any labelled points to plot. Note that this does not have to be the same
#'   set as the \code{dots}; inclusion of the latitude and longitude here as
#'   well as for that parameter means that different sub-sets can be plotted.
#'   (ie. dots without location names, and location names without dots)
#' @param plot.type Type of plot
#' @param name Name of file for output image. Not required if plot.type = 0;
#'   NULL as default
#' @param html.path Path for output image file. Not required if plot.type = 0;
#'   NULL as default
#' @param mask.agregion Object indicating the mapping region of interest
#'   (non-coastal lines). This is a mask for the mapping. Default is the WA
#'   grainbelt eastern edge
#' @param mask.coast Object indicating the location of the ocean in the mapping
#'   area of interest. This is a mask for the mapping, so that nothing is
#'   plotted here. Default is the coast for the WA grainbelt
#' @param lines.shire Object containing the location of the shire boundaries of
#'   the mapping area of interest. This adds lines to the map
#' @param lines.coast Plot object to add the coast line to the map. Default is
#'   coast line for SW WA
#' @param lines.agregion Plot object to add a line indicating the
#'   northern/eastern edge of the grainbelt.
#' @param logo image object for the logo to be added to the map (optional).
#' @param scale Plotting scale, default = 10.
#' @param lambda Smoothing parameter for the Kriging. Default is 0.01. Larger
#'   values (e.g. 0.5) will give greater smoothing.
#' @param legendx left edge of a vertical legend to be placed using
#'   'embed.legend'. Will only plot a legend if this is a numeric parameter.
#'   Should not throw errors.
#' @param transparency Should the background be transparent? If false, the
#'   background will be white. (default = FALSE)
#' @param box Should a box be drawn around the plot area? (default = TRUE)
#' @param type What kind of map are you after? Current options are 'krig' and
#'   'point', future plans will include options for categorical data (either
#'   points or interpolated). 'Krig' takes a continuous variable and interpolates
#'   to fill the map space with categorical colour; point takes a continuous
#'   variable and plots just the points with categorical colour. Default value
#'   is 'krig' for backwards compatibility
#'
#' @keywords hplot
#' @export

# ** to do
#    * scale: make option to have a vector. This does dot size, heading size, legend text size,
#      so allow for three? or if we want to scale the headings differently, five (or six?)
#      The internal functionality will use a vector of given length; if the vector is not the full
#      length, then an if statement at the beginning would created one based on what is passed.

map.ssf <- function(data,
                    varname,
                    col.dataframe,
                    percent,
                    title,
                    subtitle1,
                    subtitle2,
                    subtitle3 = NULL,
                    dots = NULL,
                    dots.label = NULL,
                    plot.type = 0,
                    name = NULL,
                    html.path = NULL,
                    mask.agregion = ssf::agregion.img,
                    mask.coast = ssf::coast.img,
                    lines.shire = ssf::shires.lines,
                    lines.coast = ssf::coast.lines,
                    lines.agregion = ssf::agregion.lines,
                    logo = NULL,
                    scale = 10,
                    lambda = 0.01,
                    legendx = 122.25,
                    transparency = FALSE,
                    box = TRUE,
                    type = "krig") {

  # if file is to be saved, make sure the directory exists; and that name and html.path are allocated
  if (plot.type != 0) {
    # create directory if required.
    if (!dir.exists(html.path)) {
      dir.create(html.path, recursive = TRUE)
    }
    # set opened device to close at the end
    on.exit(grDevices::dev.off())
  }

  # make sure the type is correctly sorted.
  type <- match.arg(type,
    choices = c("krig", "point", "category"),
    several.ok = FALSE
  )

  # set variables ----
  # ** if this is going to be generalised, then this will need modifying
  # ** this is almost but not quite the same as the Kriging grid values
  # longitude and latitude limits are hard coded at present, but this can be
  # modified in later versions. Values here are for mapping the WA grainbelt
  # Do not change unless the masks are modified accordingly.
  long.lims <- c(114, 123.5)
  lat.lims <- c(-35.5, -27.5)

  # Plot parameters
  # Original SSF grid cells
  lon.sw <- seq(long.lims[1],
    long.lims[2],
    by = 0.5
  ) # length is used for deriving plot parameters; nothing else
  lat.sw <- seq(lat.lims[1],
    lat.lims[2],
    by = 0.5
  ) # length is used for deriving plot parameters; nothing else
  # sc <- 0.005
  px1 <- 117.75 # position of text
  py1 <- -28 # for determining positioning of text.
  if (transparency) {
    bg <- "transparent"
  } else {
    bg <- "white"
  }

  # create the plot ----
  # open plot device
  new.ssf.plot(
    type = plot.type,
    width = (length(lon.sw) - 1) * scale, # plot width,
    height = (length(lat.sw) - 1) * scale, # plot height,
    name = name,
    path = html.path,
    background = bg
  )
  # set the plot margins so that the plot takes up the full space
  # (i = inches; mar would do the same)
  graphics::par(mai = c(0, 0, 0, 0))
  # initialise plot
  graphics::plot.new()
  # set limits on window
  graphics::plot.window(
    xlim = long.lims,
    ylim = lat.lims,
    xaxs = "i",
    yaxs = "i"
    # type: internal - pretty labels encompassing the original data range
  )
  # add box, if required ----
  if (box) {
    graphics::box()
  }

  # map layer ----
  # generate the map layer, if required (map layer required for krig,
  # but not point)
  if (type == "krig") {
    b <- map.krig.layer(
      data = data,
      varname = varname,
      lambda = lambda,
      long.lims = long.lims,
      lat.lims = lat.lims,
      color = col.dataframe,
      mask.agregion = mask.agregion,
      mask.coast = mask.coast
    )
    # plot the color sections
    graphics::rasterImage(
      b,
      xleft = min(long.lims), # where is this getting x and y from?
      ybottom = min(lat.lims),
      xright = max(long.lims),
      ytop = max(lat.lims)
    )
  }

  # note that axes are not included in this map because it would clutter the map
  # and most people will be using landmarks to find where they are interested
  # in. However, for later versions, this is where it would be added.

  # add lines ----
  # add shire lines
  # This has to be first, so that the coast line and inland edge of the region
  # get plotted over the top of any lines that are in both lines entirely
  # outside the grainbelt region; to be omitted
  # ** it would be nice to get this tidied up
  # ** and create the correct object
  outside.region <- c(
    53, 78, 174, 176, 178, 180:181, 482,
    519, 522, 527, 531:532, 537:543, 546, 548, 557, 559:560, 564:567, 576:577,
    611, 616, 620
  )
  # lines that are partially in the region, and not in the region
  # overlaps <- c(520, 525, 544, 545, 547, 553, 563, 573, 583)
  invisible(lapply(lines.shire[-outside.region], FUN = graphics::lines, lwd = scale / 10))
  # add coast line
  graphics::lines(lines.coast, lwd = 2 * scale / 10)
  # add inland edge of agricultural region
  graphics::lines(lines.agregion, lwd = 2 * scale / 10)

  # add legend ----
  if (is.numeric(legendx)) {
    embed.legend(
      left = legendx,
      top = -29.25,
      colors = col.dataframe,
      u = 0.375,
      cex = 0.7 * scale / 10,
      percent = percent
    )
  }
  # Add logo (if supplied) ----
  if (!is.null(logo)) {
    logo_scale <- 0.004 # scale modifies size.
    logo_offset <- 0.2 # distance from the corner
    logo_x <- max(long.lims) - logo_offset # logo position is specified from the bottom right corner;
    logo_y <- min(lat.lims) + logo_offset
    graphics::rasterImage(
      logo,
      xright  = logo_x,
      ybottom = logo_y,
      xleft   = logo_x - logo_scale * ncol(logo),
      ytop    = logo_y + logo_scale * nrow(logo)
    )
  }
  # add station locations and/or labels (if supplied) ----
  # if points, we do that first, and set dots to NULL;
  # otherwise, we get black dots if required.
  if (type == "point") {
    # get the points to plot
    if (!is.null(dots)) {
      # make sure that dots is a data frame, not a data table
      dots <- data.frame(dots)
      locations <- merge(data, dots, all.x = FALSE, all.y = FALSE)
      # once we have used the dots, delete them (why?)
      dots <- NULL
    } else {
      locations <- data
    }
    names(locations) <- tolower(names(locations))

    # set the colours
    point.cols <- color.df(locations[[tolower(varname)]],
      col.df = col.dataframe
    )
    # plot them!
    graphics::points(locations[["longitude"]],
      locations[["latitude"]],
      cex = scale * 0.16, # all I have done is double the black dots size
      pch = 20,
      col = point.cols
    )
    # graphics::points(locations[["longitude"]],
    #                  locations[["latitude"]],
    #                  cex = scale * 0.16, # all I have done is double the black dots size
    #                  pch = 20,
    #                  col = "red")
  }


  if (!is.null(dots)) {
    names(dots) <- tolower(names(dots))
    graphics::points(dots[["longitude"]],
      dots[["latitude"]],
      cex = scale * 0.08,
      pch = 20
    )
  }
  if (!is.null(dots.label)) {
    names(dots.label) <- tolower(names(dots.label))
    graphics::text(
      x = dots.label[["longitude"]],
      y = dots.label[["latitude"]],
      labels = dots.label[["station_name"]],
      cex = scale / 20,
      pos = 4,
      offset = 0.1
    )
  }
  # add text ----
  # heading and top two sub-titles
  graphics::text(
    x = px1 - 1,
    y = py1,
    labels = title,
    pos = 4, # to the right of the specified coordinates
    cex = scale * 0.2
  )
  graphics::text(
    x = px1,
    y = py1 - 0.4,
    labels = subtitle1,
    cex = scale * 0.1,
    pos = 4
  )
  graphics::text(
    x = px1,
    y = py1 - 0.65,
    labels = subtitle2,
    cex = scale * 0.1,
    pos = 4
  )
  # third subtitle, if included; 'stations used'
  if (!is.null(subtitle3)) {
    # add subtitle3
    graphics::text(
      x = px1,
      y = py1 - 0.9,
      labels = subtitle3,
      cex = scale * 0.1,
      pos = 4
    )
    if (!is.null(dots)) {
      # add stations used
      graphics::points(
        x = px1,
        y = py1 - 1.15,
        cex = scale * 0.08,
        pch = 20
      )
      graphics::text(
        x = px1 + 0.1,
        y = py1 - 1.15,
        labels = "Stations used in analysis",
        cex = scale / 12.5,
        pos = 4
      )
    }
  } else {
    if (!is.null(dots)) {
      graphics::points(
        x = px1,
        y = py1 - 0.9,
        cex = scale * 0.08,
        pch = 20
      )
      graphics::text(
        px1 + 0.1,
        py1 - 0.9,
        "Stations used in analysis",
        cex = scale * 0.08,
        pos = 4
      )
    }
  }
}

#' Map the SWLD
#'
#' Map the weather for the South West Land Division of Western Australia
#'
#' @inheritParams map_weather
#' @param ... other parameters as per \code{map_weather} which will be passed to
#'   that function. This excludes the masks and lines, which are hard coded in
#'   this function

map_weather_swld <- function(data,
                             varname,
                             col_df,
                             ...) {

  map_weather(data,
              varname,
              col_df,
              mask_agregion = wrapique::agregion_img,
              mask_coast = wrapique::coast_img,
              lines_shire = wrapique::shires_lines,
              lines_coast = wrapique::coast_lines,
              lines_agregion = wrapique::agregion_lines,
              logo = wrapique::dpird_logo,
              ...)

