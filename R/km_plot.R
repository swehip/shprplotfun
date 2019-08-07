#' Kaplan-Meier plot function
#'
#' Kaplan-Meier plot using ggplot2.
#'
#' @param survfit_obj             Object returned from survfit function in
#'                                  survival package. Also works with data frame
#'                                  if year and surv variable exist. lower
#'                                  and upper variable needed if show_ci = TRUE.
#'                                  Specify strata variable needed
#'                                  if several curves wanted.
#' @param make_step               If TRUE, step data will be created.
#' @param first_point             If make_step = TRUE, first_point for KM is
#'                                  1 and for competing risk 0.
#' @param one_level               Boolean indicating if there is only one
#'                                  level in the strata.
#' @param y_lim                   Limit on y-axis.
#' @param percent_accuracy        Set accuracy for \code{\link{percent_format}}.
#' @param y_breaks                Length between each break on y-axis.
#' @param x_lim                   Limit on x-axis.
#' @param x_breaks                Length between each break on x-axis.
#' @param n_points                Number of points to be plotted,
#'                                  useful to change if file is
#'                                  large because of too many points!
#' @param title                   Plot title, NULL for no title.
#' @param subtitle                Small text under title, NULL for no subtitle.
#' @param title_size              Text size of title in pt.
#' @param subtitle_size           Text size of subtitle in pt.
#' @param title_margin            Space between title and subtitle in pt.
#' @param y_lab                   Y-axis label.
#' @param x_lab                   X-axis label.
#' @param background_color        Color of the panel background.
#' @param panel_grid_color        Color of the panel grid lines.
#' @param panel_grid_size         Size of the panel grid lines in plot,
#'                                  useful to change if large dpi!
#' @param axis_size               Size of the axis lines,
#'                                  useful to change if large dpi!
#' @param text_size               Size of the text in pt.
#' @param line_size               Size of the head lines.
#' @param show_ci                 If TRUE, show confidence interval lines.
#' @param ci_line_size            Size of the confidence interval lines.
#' @param line_colors             Color of the different curves.
#' @param legend_pos              Position of the legend in plot.
#' @param legend_labels           Label for each legend key, default order as
#'                                they appear in names(survfit_obj$strata).
#' @param label_breaks            Order of the legend keys.
#' @param legend_key_height_mult  Increase space between legend keys with a
#'                                  multiple.
#' @param n_risk_break            Minimum number at risk to include
#' @param ribbon_ci               Show confidence interval
#' @param ribbon_alpha            Degree of transparency for confidence interval
#'
#' @return ggplot object containing Kaplan-Meier plot.
#'
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
           make_step              = NULL,
           first_point            = 1,
           one_level              = FALSE,
           y_lim                  = NULL,
           percent_accuracy       = 1,
           y_breaks               = 5,
           x_lim                  = NULL,
           x_breaks               = 1,
           n_points               = NULL,
           n_risk_break           = 50,
           title                  = NULL,
           subtitle               = NULL,
           title_size             = 14,
           subtitle_size          = 10,
           title_margin           = 1,
           y_lab                  = NULL,
           x_lab                  = NULL,
           background_color       = "moccasin",
           panel_grid_color       = "grey",
           panel_grid_size        = 0.3,
           axis_size              = 0.3,
           text_size              = 8,
           line_size              = 0.5,
           show_ci                = TRUE,
           ribbon_ci              = TRUE,
           ribbon_alpha           = 0.5,
           ci_line_size           = 0.2,
           line_colors            = c("#377EB8", "#E41A1C", "#4DAF4A",
                                      "#984EA3", "#FF7F00", "#FFFF33",
                                      "#A65628", "#F781BF"),
           legend_pos             = c(0, 0),
           legend_labels          = ggplot2::waiver(),
           label_breaks           = ggplot2::waiver(),
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
          dplyr::filter_(~n.risk >= n_risk_break) %>%
          dplyr::group_by(strata) %>%
          dplyr::filter_(~n.event != 0 | n.risk == min(n.risk) | year == 0) %>%
          dplyr::ungroup()

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
          dplyr::filter_(~n.risk >= n_risk_break) %>%
          dplyr::group_by(strata) %>%
          # removes all points where there are no events
          dplyr::filter_(~n.event != 0 | n.risk == min(n.risk) | year == 0) %>%
          # but adds the last point for each curve
          dplyr::ungroup()

      }

      # make step function data by adding all points from data but
      # with all the next values in year
      df2 <-
        dplyr::group_by(df, strata) %>%
        dplyr::mutate_(year = ~lead(year)) %>%
        dplyr::ungroup() %>%
        dplyr::filter_(~!is.na(year))

      df <- rbind(df, df2) %>%
        # bind together with original data
        dplyr::arrange_(~strata, ~year, ~dplyr::desc(surv), ~dplyr::desc(n.risk))

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
          year   = 0,
          surv   = first_point,
          lower  = first_point,
          upper  = first_point,
          strata = unique(df$strata)
        )

        df <-
          dplyr::bind_rows(df, df2) %>%
          dplyr::arrange_(~strata, ~year, ~dplyr::desc(surv))

        df2 <- dplyr::group_by(df, strata) %>%
          dplyr::mutate_(year = ~dplyr::lead(year)) %>%
          dplyr::ungroup() %>%
          dplyr::filter_(~!is.na(year))

        if (first_point == 1) {
          df <- rbind(df, df2) %>%
            # bind together with original data
            dplyr::arrange_(~strata, ~year, ~dplyr::desc(surv))
        } else{
          df <- rbind(df, df2) %>%
            # bind together with original data
            dplyr::arrange_(~strata, ~year, ~surv)
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

    km <- ggplot2::ggplot(df, ggplot2::aes_(x = ~year, y = ~surv)) +
      ggplot2::theme_classic() +
      ggplot2::scale_colour_manual(
        values = line_colors,
        labels = legend_labels,
        breaks = label_breaks
      ) +
      ggplot2::scale_y_continuous(
        breaks = seq(y_lim[1], y_lim[2], by = y_breaks),
        limits = y_lim,
        labels = scales::percent_format(accuracy = percent_accuracy),
      ) +
      ggplot2::scale_x_continuous(
        breaks = seq(x_lim[1], x_lim[2], by = x_breaks),
        limits = x_lim
      ) +
      ggplot2::geom_line(
        ggplot2::aes(colour = strata, group = strata), size = line_size
      ) +
      ggplot2::ggtitle(title, subtitle = subtitle) +
      ggplot2::xlab(x_lab) +
      ggplot2::ylab(y_lab) +
      ggplot2::theme(
        panel.background      = ggplot2::element_rect(fill = background_color),
        panel.grid.major.y    = ggplot2::element_line(
                                  colour = panel_grid_color,
                                  size   = panel_grid_size),
        axis.line             = ggplot2::element_line(size = axis_size),
        axis.ticks            = ggplot2::element_line(size = axis_size),
        plot.title            = ggplot2::element_text(
                                  hjust = 0.5,
                                  size = title_size,
                                  colour = "black",
                                  margin = ggplot2::margin(b = title_margin)
                                ),
        plot.subtitle         = ggplot2::element_text(
                                  hjust = 0.5,
                                  size = subtitle_size,
                                  colour = "black"
                                ),
        axis.text             = ggplot2::element_text(
                                   colour = "black", size = text_size),
        axis.title            = ggplot2::element_text(size = text_size),
        legend.text          = ggplot2::element_text(
          size = text_size,
          margin = ggplot2::margin(
            l = text_size / 2,
            r = text_size / 2)
        ),
        legend.position       = legend_pos,
        legend.justification  = legend_pos,
        legend.background     = ggplot2::element_rect(fill = "transparent"),
        legend.title          = ggplot2::element_blank(),
        legend.key.height     = ggplot2::unit(
                                 text_size * legend_key_height_mult, "pt")
      )

    if (show_ci) {
      if (ribbon_ci) {
        km <-
          km +
          ggplot2::geom_ribbon(
            ggplot2::aes_(
              ymin  = ~lower,
              ymax  = ~upper,
              fill  = ~strata,
              group = ~strata
            ),
            alpha = ribbon_alpha
          ) +
          ggplot2::scale_fill_manual(
            values = line_colors,
            labels = legend_labels,
            breaks = label_breaks
          )

      } else{
        km <-
          km +
          ggplot2::geom_line(
            ggplot2::aes_(
              y      = ~upper,
              colour = ~strata,
              group  = ~strata
            ),
            size = ci_line_size
          ) +
          ggplot2::geom_line(
            ggplot2::aes_(
              y      = ~lower,
              colour = ~strata,
              group  = ~strata
            ),
            size = ci_line_size
          )
      }
    }
  km
}
