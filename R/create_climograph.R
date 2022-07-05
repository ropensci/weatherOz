
#' Creates a climograph of weather data from the DPIRD API
#'
#' @param weather A list of weather station data from the \acronym{DPIRD}
#' Science \acronym{API}.
#'
#' @return A \CRANpkg{ggplot2} object
#' @export
#'
#' @examples
#' create_climograph(weather = weather)
#'
create_climograph <- function(weather) {

  MAX_TEMP <- MIN_TEMP <- RAIN <- NULL

  # scale axis to max rainfall and min/max temperature
  ylim.prim <- c(0, max(weather$RAIN))
  ylim.sec <- c(min(weather$MIN_TEMP), max(weather$MAX_TEMP))

  b <- diff(ylim.prim) / diff(ylim.sec)
  a <- b * (ylim.prim[1] - ylim.sec[1])

  ggplot2::ggplot(weather, ggplot2::aes(x = as.Date(DATE), y = RAIN)) +
    ggplot2::geom_col(fill = "navyblue",
                      colour = "navyblue") +
    ggplot2::geom_line(ggplot2::aes(y = a + MAX_TEMP * b),
                       colour = "firebrick",
                       size = 0.75) +
    ggplot2::geom_line(ggplot2::aes(y = a + MIN_TEMP * b),
                       colour = "steelblue3",
                       size = 0.75) +
    ggplot2::scale_y_continuous(
      "Precipitation (mm)",
      sec.axis = ggplot2::sec_axis( ~ (. - a) / b,
                        name = "Max (red) and Min (blue) Temperature (\u00B0C)")
    ) +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels =  "%b\n%Y") +
    ggplot2::xlab("Date") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.ticks.y.right = ggplot2::element_line(color = "firebrick"),
      axis.text.y.right = ggplot2::element_text(color = "firebrick")
    )
}
