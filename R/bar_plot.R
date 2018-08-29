#' Bar plot function
#'
#' Plot a bar plot using ggplot2.
#' @param df  Data frame.
#' @param x_var  Variable for x axis, use string name. Recommended that x_var is
#'   in character in df.
#' @param fill_var  Variable for the different colors in bars, use string name.
#'   Use NULL if only one color for bars.
#' @param y_var  Variable for y axis, if NULL, count is used.
#' @param style  3 different styles of bar plots, stack, fill, or dodge. fill
#'   requires y_percent  TRUE.
#' @param group_by_x_var  Only relevant for style dodge. Boolean indicating if
#'   percentages should be for x_var or fill_var.
#' @param y_percent  If TRUE, y axis is in percent form. Otherwise in count
#'   form.
#' @param y_lim  Limit on y axis.
#' @param y_breaks  Length between each break on y axis.
#' @param y_breaks_end  Break end, default for 100000. Works for all count
#'   values below that.
#' @param title  Plot title, NULL if no title.
#' @param subtitle  Small text under title, NULL if no subtitle.
#' @param title_size  Text size of title in pt.
#' @param subtitle_size  Text size of subtitle in pt.
#' @param title_margin  Distance between subtitle and title in pt. If no
#'   subtitle, title_margin  0.5*title_size.
#' @param y_lab  Y-axis label, use NULL for no label.
#' @param x_lab  X-axis label, use NULL for no label.
#' @param background_color  Color of the panel background.
#' @param panel_grid_color  Color of the panel grid lines.
#' @param panel_grid_size  Size of the panel grid lines in plot, useful to
#'   change if large dpi!
#' @param contour_line_size  Contour around bars size.
#' @param axis_size  Size of the axis lines.
#' @param axis_text_angle  Angle of the tick texts, 45 is recommended for many x
#'   levels.
#' @param text_size  Size of the text in pt.
#' @param fill_colors  Color of the different categories in fill_var.
#' @param legend_pos  Position of the legend in plot, if c(1,1), c(1,0) etc,
#'   legend inside plot.
#' @param legend_labels  Label for each legend key.
#' @param label_breaks  Order of the legend keys.
#' @param legend_background  Color of the legend background.
#' @param legend_row  How many rows for the legends.
#' @param legend_col  How many columns for the legends.
#' @return Ggplot object containing bar plot.
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' # Style stack
#' bar_plot(df = diamonds, x_var = 'color',
#' fill_var = 'cut', y_breaks = 2)
#' bar_plot(df = diamonds, x_var = 'color',
#' fill_var = 'cut', y_percent = FALSE, y_breaks = 2000)
#'
#' # Style stack with y variable included
#' df <- diamonds %>% group_by_('color', 'cut') %>% summarise(y = n())
#' bar_plot(df = df, x_var = 'color',
#'   fill_var = 'cut', y_var = 'y', y_breaks = 2)
#'
#' # Style fill
#' bar_plot(df = diamonds, x_var = 'color', fill_var = 'cut',
#' y_breaks = 10, style = 'fill')
#'
#' # Style dodge grouped by x_var (color in this case)
#' bar_plot(df = diamonds, x_var = 'color', fill_var = 'cut',
#' style = 'dodge', y_breaks = 10)
#' bar_plot(df = diamonds, x_var = 'color', fill_var = 'cut',
#' style = 'dodge', y_percent = FALSE, y_breaks = 2000)
#'
#' # Style dodge grouped by fill_var (cut in this case)
#' bar_plot(df = diamonds, x_var = 'color', fill_var = 'cut',
#' style = 'dodge', group_by_x_var = FALSE, y_breaks = 10)
#'
#' # Since bar_plot() returns ggplot object, it is possible to add more features
#' # Here we zoom the plot using coord_cartesian():
#' df <- dplyr::filter(diamonds, clarity %in% c('SI1', 'SI2', 'VS2'))
#' bar_plot(df = df, x_var = 'clarity',
#' style = 'dodge', y_percent = FALSE, y_breaks = 2000) +
#'   coord_cartesian(ylim = c(8000, 14000))
#' @export
bar_plot <-
  function(df,
           x_var,
           fill_var = NULL,
           y_var = NULL,
           style = c("stack", "fill", "dodge")[1],
           group_by_x_var = TRUE,
           y_percent = TRUE,
           y_lim = NULL,
           y_breaks = 2000,
           x_breaks = NULL,
           y_breaks_end = 100000,
           title = NULL,
           subtitle = NULL,
           title_size = 14,
           subtitle_size = 10,
           title_margin = 1,
           y_lab = NULL,
           x_lab = NULL,
           background_color = "moccasin",
           panel_grid_color = "grey",
           panel_grid_size = 0.3,
           contour_line_size = 0.1,
           axis_size = 0.3,
           axis_text_angle = 0,
           text_size = 8,
           fill_colors = c(
             "#E41A1C",
             "#377EB8",
             "#4DAF4A",
             "#984EA3",
             "#FF7F00",
             "#FFFF33",
             "#A65628",
             "#F781BF"
           ),
           legend_pos = "bottom",
           legend_labels = waiver(),
           label_breaks = waiver(),
           legend_background = "transparent",
           legend_row = NULL,
           legend_col = NULL) {

    # If y_var != NULL, no summarise is needed. -------------------------------

    show_legend <- TRUE

    if (is.character(y_var)) {
      names(df)[names(df) == y_var] <- 'y'

      if (!is.character(fill_var)) {
        df$y2 <- 1
        fill_var <- "fill_var"
        show_legend <- FALSE
      } else{
        # y2 used for style dodge ----------------------------------------------

        if (group_by_x_var) {
          df <- df %>% group_by_(x_var) %>% mutate(y2 = sum(y))

        } else{
          df <- df %>% group_by_(fill_var) %>% mutate(y2 = sum(y))

        }
      }

    } else{
      # Only one fill variabel used means no legend needed  --------------------

      if (!is.character(fill_var)) {
        fill_var <- "fill_var"
        show_legend <- FALSE
        df <- df %>% group_by_(x_var) %>% summarise(y = n())
        df$y2 <- 1
      } else{
        # Data transformations -------------------------------------------------

        if (group_by_x_var) {
          df <-
            df %>%
            group_by_(x_var, fill_var) %>%
            summarise(y = n()) %>%
            group_by_(x_var) %>%
            mutate(y2 = sum(y))

        } else{
          df <-
            df %>%
            group_by_(x_var, fill_var) %>%
            summarise(y = n()) %>%
            group_by_(fill_var) %>%
            mutate(y2 = sum(y))
        }

      }

    }

    # y2 used for style dodge

    # Ggplot ------------------------------------------------------------------

    if (!is.character(subtitle)) {
      title_margin <- 0.5 * title_size
    }


    bars <- ggplot(data = df) +
      theme_classic() +
      scale_fill_manual(
        values = fill_colors,
        labels = legend_labels,
        breaks = label_breaks,
        guide = guide_legend(nrow = legend_row, ncol = legend_col)
      ) +
      ylab(y_lab) +
      xlab(x_lab) +
      ggtitle(title, subtitle = subtitle) +
      theme(
        panel.background = element_rect(fill = background_color),
        panel.grid.major.y =
          element_line(colour = panel_grid_color, size = panel_grid_size),
        axis.line = element_line(size = axis_size),
        axis.ticks = element_line(size = axis_size),
        plot.title = element_text(
          hjust = 0.5,
          size = title_size,
          colour = "black",
          margin = margin(b = title_margin)
        ),
        plot.subtitle = element_text(
          hjust = 0.5,
          size = subtitle_size,
          colour = "black"
        ),
        axis.text = element_text(colour = "black", size = text_size),
        axis.text.x = element_text(angle = axis_text_angle),
        axis.title = element_text(size = text_size),
        legend.text = element_text(size = text_size),
        legend.background = element_rect(fill = legend_background),
        legend.title = element_blank(),
        legend.key.height = unit(text_size, "pt"),
        legend.key.width = unit(text_size, "pt"),
        legend.position = legend_pos,
        legend.justification = legend_pos
      )

    if (y_percent) {
      y_breaks <- y_breaks / 100

      if (is.vector(y_lim)) {
        y_lim <- y_lim / 100
      }

      if (style == "dodge") {
        bars <-
          bars + geom_bar(
            width = 0.5,
            mapping = aes_string(x = x_var, y = "y/y2", fill = fill_var),
            stat = "identity",
            show.legend = show_legend,
            position = position_dodge(width = 0.5),
            color = "black",
            size = contour_line_size
          ) +
          scale_y_continuous(
            labels = scales::percent,
            breaks = seq(0, 1, by = y_breaks),
            limits = y_lim
          )

      } else if (style == "fill") {
        bars <-
          bars + geom_bar(
            width = 0.5,
            mapping = aes_string(x = x_var, y = "y/sum(y)", fill = fill_var),
            stat = "identity",
            show.legend = show_legend,
            position = position_fill(vjust = 0.5, reverse = TRUE),
            color = "black",
            size = contour_line_size
          ) +
          scale_y_continuous(
            labels = scales::percent,
            breaks = seq(0, 1, by = y_breaks),
            limits = y_lim
          )
      } else{
        bars <-
          bars + geom_bar(
            width = 0.5,
            mapping = aes_string(x = x_var, y = "y/sum(y)", fill = fill_var),
            stat = "identity",
            show.legend = show_legend,
            position = position_stack(vjust = 0.5, reverse = TRUE),
            color = "black",
            size = contour_line_size
          ) +
          scale_y_continuous(
            labels = scales::percent,
            breaks = seq(0, 1, by = y_breaks),
            limits = y_lim
          )

      }

    } else if (style == "fill") {
      bars <-
        bars + geom_bar(
          width = 0.5,
          mapping = aes_string(x = x_var, y = "y", fill = fill_var),
          stat = "identity",
          show.legend = show_legend,
          position = position_fill(vjust = 0.5, reverse = TRUE),
          color = "black",
          size = contour_line_size
        ) +
        scale_y_continuous(breaks = seq(0, y_breaks_end, by = y_breaks),
                           limits = y_lim)

    } else if (style == "dodge") {
      bars <-
        bars + geom_bar(
          width = 0.5,
          mapping = aes_string(x = x_var, y = "y", fill = fill_var),
          stat = "identity",
          show.legend = show_legend,
          position = position_dodge(width = 0.5),
          color = "black",
          size = contour_line_size
        ) +
        scale_y_continuous(breaks = seq(0, y_breaks_end, by = y_breaks),
                           limits = y_lim)


    } else{
      bars <-
        bars + geom_bar(
          width = 0.5,
          mapping = aes_string(x = x_var, y = "y", fill = fill_var),
          stat = "identity",
          show.legend = show_legend,
          position = position_stack(vjust = 0.5, reverse = TRUE),
          color = "black",
          size = contour_line_size
        ) +
        scale_y_continuous(breaks = seq(0, y_breaks_end, by = y_breaks),
                           limits = y_lim)
    }

    if (is.numeric(df[[x_var]]) & !is.null(x_breaks)) {
      bars <-
        bars +
        scale_x_continuous(
          breaks = seq(floor(min(df[[x_var]])),
                       ceiling(max(df[[x_var]])),
                       by = x_breaks)
          )

    }
    bars
}
