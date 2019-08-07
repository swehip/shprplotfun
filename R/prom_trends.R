#' PROM-trends plot function
#'
#' Visualizing PROM-trends for each clinic using ggplot2.
#'
#' @param eq_vas_exp       Data frame with expected EQ VAS data.
#' @param eq_vas_obs       Data frame with observed EQ VAS data.
#' @param eq_vas_riket     Data frame with Swedish average EQ VAS data.

#' @param pain_exp         Data frame with expected pain VAS data.
#' @param pain_obs         Data frame with observed pain VAS data.
#' @param pain_riket       Data frame with Swedish average pain VAS data.
#'
#' @param satis_exp        Data frame with expected satisfaction VAS data.
#' @param satis_obs        Data frame with observed satisfaction VAS data.
#' @param satis_riket      Data frame with Swedish average satisfaction VAS data.
#' @param riket_name       Character to find what row is Swedish average,
#'                           usually named "Riket".
#' @param y_breaks         Y breaks in the three plots.
#' @param y_labs           Labels for y-axis
#' @param year             X-axis years, character vector works.
#'                           The year variables will be renamed as they appear
#'                           in the data set.
#' @param subset           Which plots should be generated, subset = 1
#'                           corresponds to the first plot in alphabetical order.
#' @param text_size        Text size in pt.
#' @param legend_labels    Labels for the legends in the plot.
#' @param line_colors      Colors of the lines.
#' @param background_color Color of the panel background.
#' @param panel_grid_color Color of the panel grid lines.
#' @param panel_grid_size  Size of the panel grid lines in plot,
#'                           useful to change if large dpi!
#' @param axis_size        Size of the axis lines, useful to change if large dpi
#' @param line_size        Line thickness of the lines in plot.
#' @param point_size       Point sizes in plot.
#' @param legend_pos       Position of the legend in plot, matrix where each row
#'                           corresponds to a legend position is recommended,
#'                           if length(subset) > 1.
#' @param n_row,n_col      Number of rows/columns for the three plots.
#'
#' @return List of several gtable objects where each gtable object is one clinic
#'
#' @examples
#' # Create trend plot for SU/Mölndal
#' # Look at the 9 data sets to see the required structure.
#' # Data sets are included in the package.
#'
#' example_plot <- prom_trends(
#'   eq_vas_exp, eq_vas_obs, eq_vas_riket, pain_exp,
#'   pain_obs, pain_riket, satis_exp, satis_obs, satis_riket,
#'   y_breaks = c(5, 0.1, 0.1), subset = 49)
#'
#' plot(example_plot)
#' @export
prom_trends <-
  function(eq_vas_exp,
           eq_vas_obs,
           eq_vas_riket,
           pain_exp,
           pain_obs,
           pain_riket,
           satis_exp,
           satis_obs,
           satis_riket,
           riket_name       = "Riket",
           y_labs           = c("EQ VAS", "Sm\u00E4rt VAS",
                                "Tillfredst\u00E4llelse"),
           y_breaks         = c(5, 5, 5),
           year             = c('2008/09', '2010/11', '2012/13', '2014/15'),
           subset           = 1,
           text_size        = 8,
           legend_labels    = c("F\u00F6rv\u00E4ntat", "Observerat", "Riket"),
           line_colors      = c("#377EB8", "#E41A1C", "black"),
           background_color = "moccasin",
           panel_grid_color = "grey",
           panel_grid_size  = 0.3,
           axis_size        = 0.3,
           line_size        = 0.5,
           point_size       = 1.5,
           legend_pos       = c(0, 0),
           n_row            = 1,
           n_col            = 3) {

    # Transforming data to be suitable for ggplot -----------------------------

    ggp_suit <- function(df, year, sheet) {
      df$hospital <- ordered(df$hospital, levels = df$hospital)
      names(df) <- c("hospital", year)

      y <- as.vector(apply(df[, year], 1, as.numeric))
      years <- rep(year, dim(df)[1])
      hospital <- sort(rep(df$hospital, dim(df)[2] - 1))

      df <- dplyr::tibble(hospital, years, y, sheet)

      df$years <- ordered(df$years, levels = year)
      df
    }

    ggsuit_df <- function(df1, df2, df3) {
      rbind(
        ggp_suit(df1, year = year, sheet = legend_labels[1]),
        ggp_suit(df2, year = year, sheet = legend_labels[2]),
        ggp_suit(df3, year = year, sheet = legend_labels[3])
      )
    }

    # Data transformation -----------------------------------------------------

    eq_vas       <- ggsuit_df(eq_vas_exp, eq_vas_obs, eq_vas_riket)
    pain         <- ggsuit_df(pain_exp,   pain_obs,   pain_riket  )
    satis        <- ggsuit_df(satis_exp,  satis_obs,  satis_riket )

    eq_vas$sheet <- ordered(eq_vas$sheet, levels = legend_labels)
    pain$sheet   <- ordered(pain$sheet,   levels = legend_labels)
    satis$sheet  <- ordered(satis$sheet,  levels = legend_labels)
    hospitals    <- unique(satis$hospital)

    # Get right ylim function -------------------------------------------------

    get_ylim <- function(y_vec, y_break) {
      top <- ((max(y_vec, na.rm = TRUE) + y_break) %/% y_break) * y_break

      bottom <- (min(y_vec, na.rm = TRUE) %/% y_break) * y_break

      if (top - bottom < 3 * y_break) {
        top <- (((max(y_vec, na.rm = TRUE) + min(y_vec, na.rm = TRUE)) /
                  2 + 2 * y_break) %/% y_break) * y_break

        bottom <- (((max(y_vec, na.rm = TRUE) + min(y_vec, na.rm = TRUE)) /
                      2 - y_break) %/% y_break) * y_break

      }

      return(c(bottom, top))

    }

    # Ggplots -----------------------------------------------------------------

    plot_list <- list()

    count <- 1

    if (is.vector(legend_pos)) {
      legend_pos <-
        matrix(rep(legend_pos, length(subset)), nrow = length(subset))

    }

     for (i in subset) {
      eq_vas_data <- dplyr::filter_(
          eq_vas, ~hospital == hospitals[i] | hospital == riket_name)
      pain_data <- dplyr::filter_(
          pain, ~hospital == hospitals[i] | hospital == riket_name)
      satis_data <- dplyr::filter_(
          satis, ~hospital == hospitals[i] | hospital == riket_name)

      ylim_eq <- get_ylim(eq_vas_data$y, y_breaks[1])
      ylim_sm <- get_ylim(pain_data$y, y_breaks[2])
      ylim_tf <- get_ylim(satis_data$y, y_breaks[3])

      eq_vas_plot <-
        ggplot2::ggplot(
          eq_vas_data,
          ggplot2::aes_( ~years, ~y, color = ~sheet, group = ~sheet)
        ) +
        ggplot2::scale_y_continuous(
          breaks = seq(ylim_eq[1], ylim_eq[2], by = y_breaks[1]),
          limits = ylim_eq) +
        ggplot2::scale_colour_manual(values = line_colors) +
        ggplot2::theme_classic() +
        ggplot2::geom_line(size = line_size, show.legend = TRUE) +
        ggplot2::geom_point(shape = 18,
                   size = point_size,
                   show.legend = TRUE) +
        ggplot2::ylab(y_labs[1]) +
        ggplot2::ggtitle(hospitals[i]) +
        ggplot2::theme(
          legend.text          = ggplot2::element_text(size = text_size),
          panel.grid.major.y   = ggplot2::element_line(
                                   colour = panel_grid_color,
                                   size = panel_grid_size),
          axis.line            = ggplot2::element_line(size = axis_size),
          axis.ticks           = ggplot2::element_line(size = axis_size),
          panel.background     = ggplot2::element_rect(fill = background_color),
          legend.title         = ggplot2::element_blank(),
          axis.text            = ggplot2::element_text(
                                   colour = "black", size = text_size),
          axis.title.x         = ggplot2::element_blank(),
          axis.title.y         = ggplot2::element_text(size = text_size),
          plot.title           = ggplot2::element_text(
                                   hjust = 0, size = text_size),
          legend.position      = legend_pos[count, ],
          legend.justification = legend_pos[count, ],
          legend.background    = ggplot2::element_rect(fill = "transparent"),
          legend.key.height    = ggplot2::unit(text_size, "pt"),
          legend.margin        = ggplot2::margin(0, 0, 0, 0, unit = "mm"),
          legend.box.margin    = ggplot2::margin(0, 0, 0, 0, unit = "mm"),
          plot.margin          = ggplot2::margin(0.2, 0.4, 0.2, 0.4, unit = "cm")
        )

      pain_plot <-
        ggplot2::ggplot(pain_data,
               ggplot2::aes_(~years, ~y, color = ~sheet, group = ~sheet)) +
        ggplot2::scale_y_continuous(
          breaks = seq(ylim_sm[1], ylim_sm[2], by = y_breaks[2]),
          limits = ylim_sm) +
        ggplot2::scale_colour_manual(values = line_colors) +
        ggplot2::theme_classic() +
        ggplot2::geom_line(size = line_size, show.legend = FALSE) +
        ggplot2::geom_point(shape = 18,
                   size = point_size,
                   show.legend = FALSE) +
        ggplot2::ggtitle("") +
        ggplot2::ylab(y_labs[2]) +
        ggplot2::theme(
          legend.text        = ggplot2::element_text(size = text_size),
          panel.grid.major.y = ggplot2::element_line(
                                 colour = panel_grid_color,
                                 size = panel_grid_size),
          axis.line          = ggplot2::element_line(size = axis_size),
          axis.ticks         = ggplot2::element_line(size = axis_size),
          panel.background   = ggplot2::element_rect(fill = background_color),
          legend.title       = ggplot2::element_blank(),
          axis.text          = ggplot2::element_text(
                                 colour = "black", size = text_size),
          axis.title.y       = ggplot2::element_text(size = text_size),
          axis.title.x       = ggplot2::element_blank(),
          plot.margin        = ggplot2::margin(0.2, 0.4, 0.2, 0.4, unit = "cm")
        )

      satis_plot <-
        ggplot2::ggplot(satis_data,
               ggplot2::aes_(~years, ~y, color = ~sheet, group = ~sheet)) +
        ggplot2::scale_y_continuous(
          breaks = seq(ylim_tf[1], ylim_tf[2], by = y_breaks[3]),
          limits = ylim_tf) +
        ggplot2::scale_colour_manual(values = line_colors) +
        ggplot2::theme_classic() +
        ggplot2::geom_line(size = line_size, show.legend = FALSE) +
        ggplot2::geom_point(shape = 18,
                   size = point_size,
                   show.legend = FALSE) +
        ggplot2::ggtitle("") +
        ggplot2::ylab(y_labs[3]) +
        ggplot2::theme(
          legend.text        = ggplot2::element_text(size = text_size),
          panel.grid.major.y = ggplot2::element_line(
                                 colour = panel_grid_color,
                                 size = panel_grid_size),
          axis.line          = ggplot2::element_line(size = axis_size),
          axis.ticks         = ggplot2::element_line(size = axis_size),
          panel.background   = ggplot2::element_rect(fill = background_color),
          legend.title       = ggplot2::element_blank(),
          axis.text          = ggplot2::element_text(
                                 colour = "black", size = text_size),
          axis.title.x       = ggplot2::element_blank(),
          axis.title.y       = ggplot2::element_text(size = text_size),
          plot.margin        = ggplot2::margin(0.2, 0.4, 0.2, 0.4, unit = "cm")
        )

      plot_list[[count]] <-
        gridExtra::arrangeGrob(
          eq_vas_plot,
          pain_plot,
          satis_plot,
          nrow = n_row,
          ncol = n_col)

      count <- count + 1
    }

    if (length(subset) < 2) {
      return(gridExtra::arrangeGrob(
        eq_vas_plot,
        pain_plot,
        satis_plot,
        nrow = n_row,
        ncol = n_col
      ))

    } else{
      return(plot_list)
    }
  }
