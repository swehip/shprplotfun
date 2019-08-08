#' Line plot function
#'
#' Standard line plot using ggplot2. Y-variable not necessary.
#'
#' @param df               Data frame.
#' @param x_var            Variable for x-axis, use string name.
#'                           Recommended that x_var is
#'                           in character in df (not necessary).
#' @param color_var        Variable for the different colors in lines, use string
#'                           name. Use NULL if only one color for lines.
#' @param y_var            Variable for y axis, if NULL, count is used.
#' @param group_by_x_var   Boolean indicating if percentages should be for x_var
#'                           or color_var.
#' @param y_percent        If TRUE, y-axis is in percent form. Otherwise in count
#'                           form.
#' @param percent_accuracy Set accuracy for \code{\link{percent_format}}.
#' @param y_lim            Limit on y-axis.
#' @param y_breaks         Length between each break on y-axis.
#' @param y_breaks_end     Break end, default for 100000. Works for all count
#'                           values below that.
#' @param line_size        Size of the lines.
#' @param title            Plot title, NULL if no title.
#' @param subtitle         Small text under title, NULL if no subtitle.
#' @param title_size       Text size of title in pt.
#' @param subtitle_size    Text size of subtitle in pt.
#' @param title_margin     Distance between subtitle and title in pt. If no
#'                           subtitle, title_margin  0.5*title_size.
#' @param y_lab            Y-axis label, use NULL for no label.
#' @param x_lab            X-axis label, use NULL for no label.
#' @param background_color Color of the panel background.
#' @param panel_grid_color Color of the panel grid lines.
#' @param panel_grid_size  Size of the panel grid lines in plot, useful to
#'                           change if large dpi!
#' @param axis_size        Size of the axis lines.
#' @param axis_text_angle  Angle of the tick texts, 45 is recommended for many x
#'                           levels.
#' @param text_size        Size of the text in pt.
#' @param fill_colors      Colors of the different categories in color_var.
#' @param legend_pos       Position of the legend in plot, if c(1,1), c(1,0) etc,
#'                           legend inside plot.
#' @param legend_labels    Label for each legend key.
#' @param label_breaks     Order of the legend keys.
#' @param legend_background  Color of the legend background.
#' @param legend_row       How many rows for the legends.
#' @param legend_col       How many columns for the legends.
#'
#' @return Ggplot object containing line-plot.
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' # y_percent = TRUE
#' line_plot(df = diamonds, x_var = 'cut', color_var = 'color', y_breaks = 2)
#' line_plot(df = diamonds, x_var = 'cut', color_var = 'color',
#' group_by_x_var = FALSE, y_breaks = 2)
#'
#' # y_percent = FALSE
#' line_plot(df = diamonds, x_var = 'cut', color_var = 'color',
#' y_percent = FALSE, y_breaks = 2000)
#'
#' # y variable included
#' df <- diamonds %>% group_by_('color', 'cut') %>% summarise(y = n())
#' line_plot(df = df, x_var = 'cut', color_var = 'color', y_var = 'y',
#' y_percent = FALSE, y_breaks = 2000)
#' line_plot(df = df[df$color == 'D',], x_var = 'cut', y_var = 'y',
#' y_percent = FALSE, y_breaks = 500)
#' @export
line_plot <-
  function(
    df,
    x_var,
    color_var         = NULL,
    y_var             = NULL,
    group_by_x_var    = TRUE,
    y_percent         = TRUE,
    percent_accuracy  = 1,
    y_lim             = NULL,
    y_breaks          = 2000,
    y_breaks_end      = 100000,
    line_size         = 1,
    title             = NULL,
    subtitle          = NULL,
    title_size        = 14,
    subtitle_size     = 10,
    title_margin      = 1,
    y_lab             = NULL,
    x_lab             = NULL,
    background_color  = "moccasin",
    panel_grid_color  = "grey",
    panel_grid_size   = 0.3,
    axis_size         = 0.3,
    axis_text_angle   = 0,
    text_size         = 8,
    fill_colors       = c("#377EB8", "#E41A1C", "#4DAF4A", "#984EA3",
                          "#FF7F00", "#FFFF33", "#A65628", "#F781BF"),
    legend_pos        = "bottom",
    legend_labels     = ggplot2::waiver(),
    label_breaks      = ggplot2::waiver(),
    legend_background = "transparent",
    legend_row        = NULL,
    legend_col        = NULL
  ) {

    # If y_var != NULL, no summarise is needed. -------------------------------

    show_legend <- TRUE

    if (is.character(y_var)) {
      names(df)[names(df) == y_var] <- 'y'
      df$y2 <- 1

      if (!is.character(color_var)) {
        color_var <- "color_var"
        show_legend <- FALSE
      }

    } else{
    # Only one fill variabel used means no legend needed  ---------------------

      if (!is.character(color_var)) {
        color_var <- "color_var"
        show_legend <- FALSE
        df <-
          df %>%
          dplyr::group_by_(x_var) %>%
          dplyr::summarise_(y = ~n()) %>%
          dplyr::mutate_(y2 = ~sum(y))
      } else{
    # Data transformations ----------------------------------------------------

        if (group_by_x_var) {
          df <-
            df %>%
            dplyr::group_by_(x_var, color_var) %>%
            dplyr::summarise_(y = ~n()) %>%
            dplyr::group_by_(x_var) %>%
            dplyr::mutate_(y2 = ~sum(y))

        } else{
          df <-
            df %>%
            dplyr::group_by_(x_var, color_var) %>%
            dplyr::summarise_(y = ~n()) %>%
            dplyr::group_by_(color_var) %>%
            dplyr::mutate_(y2 = ~sum(y))
        }
      }
    }

    # Ggplot ------------------------------------------------------------------

    if (!is.character(subtitle)) {
      title_margin <- 0.5 * title_size
    }

    lines <-
      ggplot2::ggplot(data = df) +
      ggplot2::theme_classic() +
      ggplot2::scale_color_manual(
        values = fill_colors,
        labels = legend_labels,
        breaks = label_breaks,
        guide = ggplot2::guide_legend(nrow = legend_row, ncol = legend_col)
      ) +
      ggplot2::ylab(y_lab) +
      ggplot2::xlab(x_lab) +
      ggplot2::ggtitle(title, subtitle = subtitle) +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = background_color),
        panel.grid.major.y =
          ggplot2::element_line(
            colour = panel_grid_color, size = panel_grid_size),
        axis.line  = ggplot2::element_line(size = axis_size),
        axis.ticks = ggplot2::element_line(size = axis_size),
        plot.title = ggplot2::element_text(
          hjust = 0.5,
          size = title_size,
          colour = "black",
          margin = ggplot2::margin(b = title_margin)
        ),
        plot.subtitle = ggplot2::element_text(
          hjust = 0.5,
          size = subtitle_size,
          colour = "black"
        ),
        axis.text            = ggplot2::element_text(
                                 colour = "black", size = text_size),
        axis.text.x          = ggplot2::element_text(angle = axis_text_angle),
        axis.title           = ggplot2::element_text(size = text_size),
        legend.text          = ggplot2::element_text(size = text_size),
        legend.background    = ggplot2::element_rect(fill = legend_background),
        legend.title         = ggplot2::element_blank(),
        legend.key.height    = ggplot2::unit(text_size, "pt"),
        legend.key.width     = ggplot2::unit(text_size, "pt"),
        legend.position      = legend_pos,
        legend.justification = legend_pos
      )

    if (y_percent) {
      y_breaks <- y_breaks / 100

      if (is.vector(y_lim)) {
        y_lim <- y_lim / 100
      }

      lines <-
        lines + ggplot2::geom_line(
          mapping = ggplot2::aes_string(
            x = x_var,
            y = "y/y2",
            color = color_var,
            group = color_var
          ),
          show.legend = show_legend,
          size = line_size
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::percent_format(accuracy = percent_accuracy),
          breaks = seq(0, 1, by = y_breaks),
          limits = y_lim
        )

    } else{
      lines <-
        lines + ggplot2::geom_line(
          mapping = ggplot2::aes_string(
            x = x_var,
            y = "y",
            color = color_var,
            group = color_var
          ),
          show.legend = show_legend,
          size = line_size
        ) +
        ggplot2::scale_y_continuous(
          breaks = seq(0, y_breaks_end, by = y_breaks),
          limits = y_lim
        )
    }
  lines
}
