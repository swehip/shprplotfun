#' Kaplan-Meier plot function
#'
#' Kaplan-Meier plot using ggplot2.
#' @param survfit_obj  Object returned from survfit function in survival
#'   package. Also works with data frame if year and surv variable exist. lower
#'   and upper variable needed if show_ci = TRUE. Specify strata variable needed
#'   if several curves wanted.
#' @param make_step If TRUE, step data will be created.
#' @param first_point If make_step = TRUE, first_point for KM is 1 and for
#'   competing risk 0.
#' @param one_level  Boolean indicating if there is only one level in the
#'   strata.
#' @param y_lim  Limit on y-axis.
#' @param percent_accuracy Set accuracy for scales::percent_format. See help. Default 1.
#' @param y_breaks  Length between each break on y-axis.
#' @param x_lim  Limit on x-axis.
#' @param x_breaks  Length between each break on x-axis.
#' @param n_points  Number of points to be plotted, useful to change if file is
#'   large because of too many points!
#' @param title  Plot title, NULL for no title.
#' @param subtitle  Small text under title, NULL for no subtitle.
#' @param title_size  Text size of title in pt.
#' @param subtitle_size  Text size of subtitle in pt.
#' @param title_margin  Space between title and subtitle in pt.
#' @param y_lab  Y-axis label.
#' @param x_lab  X-axis label.
#' @param background_color  Color of the panel background.
#' @param panel_grid_color  Color of the panel grid lines.
#' @param panel_grid_size  Size of the panel grid lines in plot, useful to
#'   change if large dpi!
#' @param axis_size  Size of the axis lines, useful to change if large dpi!
#' @param text_size  Size of the text in pt.
#' @param line_size  Size of the head lines.
#' @param show_ci  If TRUE, show confidence interval lines.
#' @param ci_line_size  Size of the confidence interval lines.
#' @param line_colors  Color of the different curves.
#' @param legend_pos  Position of the legend in plot.
#' @param legend_labels  Label for each legend key, default order as they appear
#'   in names(survfit_obj$strata).
#' @param label_breaks  Order of the legend keys.
#' @param legend_key_height_mult  Increase space between legend keys with a
#'   multiple.
#' @return Ggplot object containing Kaplan-Meier plot.
#' @examples
#' library(survival)
#'
#' # KM-plot with 2 levels
#' survfit_obj <- survfit(Surv(time/365.24, status) ~ sex, data = colon)
#' km_plot(survfit_obj, y_lim = c(40,100), y_breaks = 10, x_lim = c(0,9))
#'
#' # KM-plot with 6 levels
#' survfit_obj <-
#'   survfit(Surv(time/365.24, status) ~ sex + differ, data = colon)
#' km_plot(survfit_obj, y_lim = c(30,100), y_breaks = 10,
#' x_lim = c(0,9), line_colors =
#'   c('dodgerblue', 'red', 'green', 'black', 'yellow', 'chocolate'))
#' @export


km_plot <-
  function(survfit_obj,
           make_step = NULL,
           first_point = 1,
           one_level = FALSE,
           y_lim = NULL,
           percent_accuracy = 1,
           y_breaks = 5,
           x_lim = NULL,
           x_breaks = 1,
           n_points = NULL,
           n_risk_break = 50,
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
           axis_size = 0.3,
           text_size = 8,
           line_size = 0.5,
           show_ci = TRUE,
           ribbon_ci = TRUE,
           ribbon_alpha = 0.5,
           ci_line_size = 0.2,
           line_colors = c(
             "#377EB8",
             "#E41A1C",
             "#4DAF4A",
             "#984EA3",
             "#FF7F00",
             "#FFFF33",
             "#A65628",
             "#F781BF"
           ),
           legend_pos = c(0, 0),
           legend_labels = waiver(),
           label_breaks = waiver(),
           legend_key_height_mult = 1) {

    # Data suitable for ggplot ------------------------------------------------

    if (!is.data.frame(survfit_obj)) {
      if (is.null(x_lim)) {
        time_range <-
          # range of x axis, used if n_points is specified
          c(0, max(survfit_obj$time))

        ret_times <- c(0, survfit_obj$time)

      } else{
        time_range <- c(0, x_lim[2])

        ret_times <-
          c(0, survfit_obj$time[survfit_obj$time < x_lim[2]], x_lim[2])

      }
      # For one level strata, strata is created otherwise rest of
      # code does not work
      if (one_level) {
        strata <- "1"
      } else{
        if (is.numeric(n_points)) {
          # if n_points specified, time range is used
          strata <-
            summary(
              survfit_obj,
              times = seq(time_range[1], time_range[2], length.out = n_points),
              extend = TRUE
            )$strata

        } else{
          # otherwise all points are used, included the censored points
          strata <-
            summary(survfit_obj, times = ret_times, extend = TRUE)$strata
        }
      }

      if (is.numeric(n_points)) {
        sf <-
          summary(
            survfit_obj,
            times = seq(time_range[1], time_range[2], length.out = n_points),
            extend = TRUE
          )

        df <-
          data.frame(
            surv    = sf$surv,
            year    = sf$time,
            lower   = sf$lower,
            upper   = sf$upper,
            strata  = strata,
            n.risk  = sf$n.risk,
            n.event = sf$n.event
          ) %>%
          dplyr::filter(n.risk >= n_risk_break) %>%
          dplyr::group_by(strata) %>%
          dplyr::filter(
            n.event != 0 |
            n.risk == min(n.risk) | year == 0
          ) %>%
          ungroup()

      } else{
        # Creates data frame for ggplot

        sf <- summary(survfit_obj, times = ret_times, extend = TRUE)
        df <-
          data.frame(
            surv    = sf$surv,
            year    = sf$time,
            lower   = sf$lower,
            upper   = sf$upper,
            strata  = strata,
            n.event = sf$n.event,
            n.risk  = sf$n.risk
          ) %>%
          # removes points where number at risk are less than break
          dplyr::filter(n.risk >= n_risk_break) %>%
          dplyr::group_by(strata) %>%
          # removes all points where there are no events
          dplyr::filter(
            n.event != 0 |
            n.risk == min(n.risk) |
            year == 0
          ) %>%
          # but adds the last point for each curve
          ungroup()

      }

      # make step function data by adding all points from data but
      # with all the next values in year
      df2 <- dplyr::group_by(df, strata) %>%
        dplyr::mutate(year = lead(year)) %>%
        ungroup() %>%
        dplyr::filter(!is.na(year))

      df <- rbind(df, df2) %>%
        # bind together with original data
        dplyr::arrange(strata, year, desc(surv), desc(n.risk))

      # remove uggly strata=level and only keep level in legend

      strata <- strsplit(levels(df$strata), ", ") %>%
        lapply(gsub, pattern = ".*=", replacement = "") %>%
        lapply(paste0, collapse = ", ")

      df$strata <-
        factor(df$strata,
               levels = levels(df$strata),
               labels = strata)

    } else{
      df <- survfit_obj
      if (one_level) {df$strata <- "1"}
      if (make_step) {
        df2 <- data.frame(
          year = 0,
          surv = first_point,
          lower = first_point,
          upper = first_point,
          strata = unique(df$strata)
        )

        df <- dplyr::bind_rows(df, df2) %>%
          dplyr::arrange(strata, year, desc(surv))

        df2 <- dplyr::group_by(df, strata) %>%
          dplyr::mutate(year = lead(year)) %>%
          ungroup() %>%
          dplyr::filter(!is.na(year))

        if (first_point == 1) {
          df <- rbind(df, df2) %>%
            # bind together with original data
            dplyr::arrange(strata, year, desc(surv))
        } else{
          df <- rbind(df, df2) %>%
            # bind together with original data
            dplyr::arrange(strata, year, surv)
        }
      }
    }

    # Ggplot ------------------------------------------------------------------

    y_breaks <- y_breaks / 100

    if (!is.character(subtitle)) {title_margin <- 0.5 * title_size}
    if (is.null(x_lim)) {x_lim <- range(df$year)}
    if (is.null(y_lim)) {
      y_lim <- c(min(df$surv) - min(df$surv) %% y_breaks, 1)
    } else{
      y_lim <- y_lim / 100
    }

    km <- ggplot(df, aes(x = year, y = surv)) +
      theme_classic() +
      scale_colour_manual(values = line_colors,
                          labels = legend_labels,
                          breaks = label_breaks) +
      scale_y_continuous(
        breaks = seq(y_lim[1], y_lim[2], by = y_breaks),
        limits = y_lim,
        labels = scales::percent_format(accuracy = percent_accuracy)
      ) +
      scale_x_continuous(breaks = seq(x_lim[1], x_lim[2], by = x_breaks),
                         limits = x_lim) +
      geom_line(aes(colour = strata, group = strata), size = line_size) +
      ggtitle(title, subtitle = subtitle) +
      xlab(x_lab) +
      ylab(y_lab) +
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
        axis.title = element_text(size = text_size),
        legend.text = element_text(size = text_size, margin = margin(l = text_size/2,
                                                                     r = text_size/2)),
        legend.position = legend_pos,
        legend.justification = legend_pos,
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.key.height = unit(text_size * legend_key_height_mult, "pt")
      )

    if (show_ci) {
      if (ribbon_ci) {
        km <-
          km + geom_ribbon(aes(
            ymin = lower,
            ymax = upper,
            fill = strata,
            group = strata
          ),
          alpha = ribbon_alpha) +
          scale_fill_manual(values = line_colors,
                            labels = legend_labels,
                            breaks = label_breaks)

      } else{
        km <-
          km + geom_line(aes(
            y = upper,
            colour = strata,
            group = strata
          ), size = ci_line_size) +
          geom_line(aes(
            y = lower,
            colour = strata,
            group = strata
          ), size = ci_line_size)
      }
    }
  km
}
