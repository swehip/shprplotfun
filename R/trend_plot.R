#' Trend plot function
#'
#' Simple function to show trend in percent (y_var) between different years (x_var) using geom_smooth (ggplot2).
#' @param df  Data frame.
#' @param x_var  X variable.
#' @param y_var  Y variable.
#' @param y_breaks  Length between each break on y-axis.
#' @param y_lim  Limit on y-axis.
#' @param x_breaks  Length between each break on x-axis.
#' @param y_lab  Y-axis label, use NULL for no label.
#' @param x_lab  X-axis label, use NULL for no label.
#' @param title  Plot title, NULL for no title.
#' @param subtitle  Small text under title, NULL for no subtitle.
#' @param title_size  Text size of title in pt.
#' @param subtitle_size  Text size of subtitle in pt.
#' @param line_color  Color of the line.
#' @param line_size  Size of the line.
#' @param point_size  Size of the points.
#' @param background_color  Color of the panel background.
#' @param panel_grid_color  Color of the panel grid lines.
#' @param panel_grid_size  Size of the panel grid lines in plot and contour lines around bars, useful to change if large dpi!
#' @param axis_size  Size of the axis lines.
#' @param axis_text_angle  Angle of the tick texts, 45 is recommended for many x levels.
#' @param text_size  Size of the text in pt.
#' @return Ggplot object containg trend plot.
#' @examples
#'
#' # Creating data
#'
#' df <- data.frame(year = 2000:2017, prob = rnorm(18, 0.5, 0.02))
#'
#' # Trend
#'
#' trend_plot(df = df, x_var = 'year', y_var = 'prob', y_breaks = 2, y_lim = range(df$prob)*100)
#' @export



trend_plot <- function(df, x_var, y_var, y_breaks = 5, y_lim = c(54.9, 65.1), x_breaks = 5,
                      y_lab = "Procent kvinnor", x_lab = "år",
                      title = NULL, subtitle = NULL, title_size = 14, subtitle_size = 12,
                      line_color = "#377EB8", line_size = 1, point_size = 1,
                      background_color = "moccasin", panel_grid_color = "grey",
                      panel_grid_size = 0.3, axis_size = 0.3, axis_text_angle = 0, text_size = 8){

  y_breaks <- y_breaks / 100

  if(is.vector(y_lim)){
    y_lim <- y_lim / 100
  }


  trend <- ggplot(df, aes_string(x = x_var, y = y_var)) +
    theme_classic() +
    xlab(x_lab) +
    ylab(y_lab) +
    scale_y_continuous(label = scales::percent, breaks = seq(0, 1, by = y_breaks), limits = y_lim) +
    scale_x_continuous(breaks = seq(1900, 2100, by = x_breaks)) +
    ggtitle(title, subtitle = subtitle) +
    geom_point(size = point_size) +
    geom_smooth(method = "loess", colour = line_color, size = line_size) +
    theme(panel.background = element_rect(fill = background_color),
          panel.grid.major.y = element_line(size=panel_grid_size, color=panel_grid_color),
          axis.line = element_line(size = axis_size),
          axis.ticks = element_line(size = axis_size),
          axis.title = element_text(size = text_size, color = "black"),
          plot.title = element_text(hjust = 0.5, size = title_size, color = "black"),
          plot.subtitle = element_text(hjust = 0.5, size = subtitle_size, color = "black"),
          axis.text.x = element_text(color = "black", size = text_size, angle = axis_text_angle),
          axis.text.y = element_text(color = "black", size = text_size))


  return(trend)


}
