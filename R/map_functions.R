# copies/variants of the weather map function originally written for the SSF
# by Fiona Evans. Modifications in the original package were made by Anna Hepworth.
# The 'map.ssf' function from that package is the 'map.weather' function; the
# masks for the South West Land Division (SWLD) are now used in map.weather.swld rather
# than being the defaults for the more general function.

# ---- Legend ----
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
#' @param colors Data frame of discrete colors to use. If this does not contain
#'   a column called 'label', then the minimum and maximum values will be used
#'   to determine the breaks, and the values will be added at the breaks. ie.
#'   there will be one more value than there are colors.
#' @param u Scaling parameter for legend boxes (Default = 0.5). Boxes will be
#'   square with sides 'u' long.
#' @param cex Scaling parameter for the text. If this is not provided, it will
#'   be calculated as u/2, for backwards compatability.
#' @param percent Should the legend values be converted to percentages? If true,
#'   the values are multiplied by 100 and '\%' added. (Default = FALSE)
#'
#' @keywords hplot
#' @export

embed.legend <- function(
  top,
  left,
  colors,
  u       = 0.5,
  cex     = NULL,
  percent = FALSE) {

  if (percent) {
    colors$min[colors$min == -Inf] <- 0
  }

  if (is.null(cex)) {
    cex = u*2  # for backwards compatability; not all existing versions had the cex parameter
  }

  # vertical legend with top-left corner at c(left, top + u/2)
  # the top left corner of the legend is defined by c(left, top + u/2)
  len    <- nrow(colors)          # number of cells in the legend
  bottom <- top - (len - 1) * u   # location of the vertical middle of the bottom legend cell
  right  <- left + u              # location of the right vertical edge of the legend cells
  lines  <- seq(bottom, top, u)   # locations of the horizontal middles of the boxes

  # draw the squares of the legend.
  # values for lines are the central horizontal line of the box
  # plots from the bottom, using the colours of the colour set provided
  # At this point, it allows for any number of colours, but this might be limited later.
  graphics::rect(left,
                 lines - u / 2,
                 right,
                 lines + u / 2,
                 col = colors$col,
                 border = "black")

  if (is.null(colors$label)) {# add default labels

    vals   <- sort(unique(c(colors$min,
                            colors$max))) # identify and sort unique cut-points
    y.text <- seq(bottom - u / 2,
                  top + u / 2,
                  u)

    # these labels line up with the lines between colours
    # ie. they represent the boundaries between the categories
    if (percent) {
      # Note that this doesn't actually do anything sensible with Inf -> Inf %
      graphics::text(x      = right + u / 4,
                     y      = y.text,
                     pos    = 4,
                     labels = paste(100 * vals, "%"),
                     cex    = cex)
    } else {
      # default values which are not percent
      vals[length(vals)] <- paste(">", vals[length(vals)])
      graphics::text(x      = right + u / 4,
                     y      = y.text,
                     pos    = 4,
                     labels = vals,
                     cex    = cex)
    }
  } else {
    # add pre-specified labels
    graphics::text(x      = right + u / 4,
                   y      = seq(bottom, top, u),
                   pos    = 4,
                   labels = colors$label,
                   cex    = cex)
  }
}

