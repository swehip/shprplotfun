#' Trend plot function
#'
#' Simple function to show trend in percent (y_var) between different years
#' (x_var) using \code{\link{geom_smooth}}.
#'
#' @param df               Data frame.
#' @param x_var,y_var      X and Y variable.
#' @param y_breaks         Length between each break on y-axis.
#' @param y_lim            Limit on y-axis.
#' @param percent_accuracy Set accuracy for \code{\link{percent_format}}.
#' @param x_breaks         Length between each break on x-axis.
#' @param x_lab,y_lab      X and Y-axis labels, use NULL for no label.
#' @param title            Plot title, NULL for no title.
#' @param subtitle         Small text under title, NULL for no subtitle.
#' @param title_size       Text size of title in pt.
#' @param subtitle_size    Text size of subtitle in pt.
#' @param line_color       Color of the line.
#' @param line_size        Size of the line.
#' @param point_size       Size of the points.
#' @param background_color Color of the panel background.
#' @param panel_grid_color Color of the panel grid lines.
#' @param panel_grid_size  Size of the panel grid lines in plot and contour
#'   lines around bars, useful to change if large dpi!
#' @param axis_size        Size of the axis lines.
#' @param axis_text_angle  Angle of the tick texts, 45 is recommended for many x
#'   levels.
#' @param text_size        Size of the text in pt.
#' @return                 ggplot object containg trend plot.
#' @examples
#'
#' # Creating data
#'
#' df <- data.frame(year = 2000:2017, prob = rnorm(18, 0.5, 0.02))
#'
#' # Trend
#'
#' trend_plot(df, 'year', 'prob', y_breaks = 2, y_lim = range(df$prob) * 100)
#' @export
trend_plot <-
  function(
    df,
    x_var,
    y_var,
    y_breaks         = 5,
    y_lim            = c(54.9, 65.1),
    x_breaks         = 5,
    y_lab            = "Procent kvinnor",
    x_lab            = "\u00E5r",
    title            = NULL,
    subtitle         = NULL,
    title_size       = 14,
    subtitle_size    = 12,
    line_color       = "#377EB8",
    line_size        = 1,
    point_size       = 1,
    background_color = "moccasin",
    panel_grid_color = "grey",
    panel_grid_size  = 0.3,
    axis_size        = 0.3,
    axis_text_angle  = 0,
    text_size        = 8,
    percent_accuracy = 1
  ) {

    y_breaks <- y_breaks / 100

    if (is.vector(y_lim)) {
      y_lim <- y_lim / 100
    }

  ggplot2::ggplot(df, ggplot2::aes_string(x = x_var, y = y_var)) +
    ggplot2::theme_classic() +
    ggplot2::xlab(x_lab) +
    ggplot2::ylab(y_lab) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = percent_accuracy),
      breaks = seq(0, 1, by = y_breaks),
      limits = y_lim
    ) +
    ggplot2::scale_x_continuous(breaks = seq(1900, 2100, by = x_breaks)) +
    ggplot2::ggtitle(title, subtitle = subtitle) +
    ggplot2::geom_point(size = point_size) +
    ggplot2::geom_smooth(method = "loess",
                colour = line_color,
                size = line_size) +
    ggplot2::theme(
      panel.background   = ggplot2::element_rect(fill = background_color),
      panel.grid.major.y = ggplot2::element_line(size = panel_grid_size, color =
                                          panel_grid_color),
      axis.line  = ggplot2::element_line(size = axis_size),
      axis.ticks = ggplot2::element_line(size = axis_size),
      axis.title = ggplot2::element_text(size = text_size, color = "black"),
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = title_size,
        color = "black"
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5,
        size = subtitle_size,
        color = "black"
      ),
      axis.text.x = ggplot2::element_text(
        color = "black",
        size = text_size,
        angle = axis_text_angle
      ),
      axis.text.y = ggplot2::element_text(color = "black", size = text_size)
    )
}
