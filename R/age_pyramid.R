#' Age-pyramid plot function
#'
#' Plot an age pyramid using ggplot2.
#'
#' @param df               Data frame.
#' @param age_var          Name of age variable.
#' @param gender_var       Name of gender variable.
#' @param man_level        Name of man level, probably "Man" or "Män".
#' @param age_breaks       Each age observation in an interval.
#' @param age_labels       Label of the interval.
#' @param percent          If TRUE, x-axis is in percent form.
#'                           Otherwise in count form.
#' @param x_breaks         Length between each break on x-axis.
#' @param x_breaks_end     Break end, default for 100000. Works for all count
#'                           values below that.
#' @param title,subtitle   Plot title/subtitle, NULL for no title.
#' @param title_size,subtitle_size Text size of title/subtitle in pt.
#' @param title_margin     Margin for title
#' @param x_lab,y_lab      X/Y-axis labels, use NULL for no label.
#' @param background_color Color of the panel background.
#' @param panel_grid_color Color of the panel grid lines.
#' @param panel_grid_size  Size of the panel grid lines in plot and contour
#'                           lines around bars, useful to change if large dpi!
#' @param axis_size        Size of the axis lines.
#' @param axis_text_angle  Angle of the tick texts, 45 is recommended for many x
#'                           levels.
#' @param text_size        Size of the text in pt.
#' @param fill_colors      Colors of the genders.
#' @param legend_pos       Position of the legend in plot,
#'                           if c(1,1), c(1,0) etc, legend inside plot.
#' @param legend_labels    Label for each legend key.
#' @param label_breaks     Order of the legend keys.
#' @param legend_row,legend_col How many rows/columns for the legends.
#' @param contour_line_size Passed to argument "size" to \code{\link{geom_bar}}
#'
#' @return                ggplot object containing age pyramid plot.
#' @examples
#' # Creating data
#' df <- data.frame(age = rpois(100000, 65),
#'   gender = sample(c('Woman', 'Woman', 'Man'), 100000, replace = TRUE))
#'
#' # Age pyramid
#' age_pyramid(df, age_var = 'age', gender_var = 'gender',
#'   man_level = 'Man', title = "This is an age pyramid")
#'
#' # Age pyramid with percent = FALSE
#' age_pyramid(df, age_var = 'age', gender_var = 'gender',
#'   man_level = 'Man', percent = FALSE, x_breaks = 5000,
#'   title = "This is an age pyramid")
#'
#' @export
age_pyramid <-
  function(df,
           age_var           = "Alder",
           gender_var        = "Kon",
           man_level         = "Man",
           age_breaks        = c(0, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, Inf),
           age_labels        = c("0-39", "40-44", "45-49", "50-54", "55-59",
                                 "60-64", "65-69", "70-74", "75-79", "80-84",
                                 "85+" ),
           percent           = TRUE,
           x_breaks          = 5,
           x_breaks_end      = x_breaks * 1e5,
           title             = "",
           subtitle          = NULL,
           title_size        = 14,
           subtitle_size     = 10,
           title_margin      = 1,
           y_lab             = NULL,
           x_lab             = NULL,
           background_color  = "moccasin",
           panel_grid_color  = "grey",
           panel_grid_size   = 0.3,
           contour_line_size = 0.1,
           axis_size         = 0.3,
           axis_text_angle   = 0,
           text_size         = 8,
           fill_colors       = c("#377EB8", "#E41A1C"),
           legend_pos        = c(0, 0),
           legend_labels     = ggplot2::waiver(),
           label_breaks      = ggplot2::waiver(),
           legend_row        = NULL,
           legend_col        = NULL) {

  # Data transformation -----------------------------------------------------

  df[, age_var] <- cut(df[, age_var], breaks = age_breaks, labels = age_labels)

  df <-
    df %>%
    dplyr::group_by_(gender_var, age_var) %>%
    dplyr::summarise_(Population = ~n())

  df      <- stats::na.omit(df)
  n_man   <- sum(df$Population[df[, gender_var] == man_level])
  n_woman <- sum(df$Population[df[, gender_var] != man_level])
  x_lim   <- max(df$Population)

  df$Population[df[, gender_var] == man_level] <-
    -1 * df$Population[df[, gender_var] == man_level]

  # Breaks and labels for x-axis (which is currently y-axis before coordflip)
  br <- seq(0, x_breaks_end, x_breaks)
  x_breaks <- unique(c(-br, 0, br))
  x_labels <- abs(x_breaks)

  if (percent) {
    df$Population[df[, gender_var] == man_level] <-
      df$Population[df[, gender_var] == man_level] / n_man * 100
    df$Population[df[, gender_var] != man_level] <-
      df$Population[df[, gender_var] != man_level] / n_woman * 100

    x_lim <- max(abs(df$Population))
    x_labels <- paste(x_labels, "%")
  }

  # Ggplot ------------------------------------------------------------------

  if (!is.character(subtitle)) {
    title_margin <- 0.5 * title_size
  }

  ggplot2::ggplot(df,
    ggplot2::aes_string(age_var, "Population", fill = gender_var)
  ) +

  ggplot2::geom_bar(
    stat = "identity", color = "black", size = contour_line_size
  ) +

  ggplot2::xlab(x_lab) +
  ggplot2::ylab(y_lab) +
  ggplot2::ggtitle(title, subtitle = subtitle) +

  ggplot2::scale_y_continuous(
    breaks = x_breaks,
    labels = x_labels,
    limits = c(-x_lim, x_lim)
  ) +

  ggplot2::scale_fill_manual(
    paste0("n = ", as.character(n_woman + n_man)),
    values = fill_colors,
    labels = legend_labels,
    breaks = label_breaks,
    guide  = ggplot2::guide_legend(nrow = legend_row, ncol = legend_col)
  ) +

  ggplot2::coord_flip() +
  ggplot2::theme_classic() +

  ggplot2::theme(
    panel.background     = ggplot2::element_rect(fill = background_color),
    panel.grid.major.y   = ggplot2::element_line(
                             colour = panel_grid_color,
                             size = panel_grid_size),
    axis.line            = ggplot2::element_line(size = axis_size),
    axis.ticks           = ggplot2::element_line(size = axis_size),
    plot.title           = ggplot2::element_text(hjust = 0.5, size = title_size,
                             colour = "black",
                             margin = ggplot2::margin(b = title_margin)),
    plot.subtitle        = ggplot2::element_text(hjust = 0.5,
                            size = subtitle_size, colour = "black"),
    axis.text            = ggplot2::element_text(colour = "black",
                             size = text_size),
    axis.text.x          = ggplot2::element_text(angle = axis_text_angle),
    axis.title           = ggplot2::element_text(size = text_size),
    legend.text          = ggplot2::element_text(size = text_size),
    legend.background    = ggplot2::element_rect(fill = "transparent"),
    legend.title         = ggplot2::element_text(size = text_size),
    legend.key.height    = ggplot2::unit(text_size, "pt"),
    legend.key.width     = ggplot2::unit(text_size, "pt"),
    legend.position      = legend_pos,
    legend.justification = legend_pos
  )
}
