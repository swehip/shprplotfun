#' PROM-trends plot function
#'
#' Visualizing PROM-trends for each clinic using ggplot2.
#' @param eq_vas_exp  Data frame with expected EQ VAS data.
#' @param eq_vas_obs  Data frame with observed EQ VAS data.
#' @param eq_vas_riket  Data frame with Swedish average EQ VAS data.

#' @param pain_exp  Data frame with expected pain VAS data.
#' @param pain_obs  Data frame with observed pain VAS data.
#' @param pain_riket  Data frame with Swedish average pain VAS data.
#'
#' @param satis_exp  Data frame with expected satisfaction VAS data.
#' @param satis_obs  Data frame with observed satisfaction VAS data.
#' @param satis_riket  Data frame with Swedish average satisfaction VAS data.

#' @param riket_name Character to find what row is Swedish average, usually named "Riket".
#' @param y_breaks  Y breaks in the three plots.
#' @param year  X-axis years, character vector works. The year variables will be renamed as they appear in the data set.
#' @param subset  Which plots should be generated, subset = 1 corresponds to the first plot in alphabetical order.
#' @param text_size  Text size in pt.
#' @param legend_labels Labels for the legends in the plot.
#' @param line_colors  Colors of the lines.
#' @param background_color  Color of the panel background.
#' @param panel_grid_color  Color of the panel grid lines.
#' @param panel_grid_size  Size of the panel grid lines in plot, useful to change if large dpi!
#' @param axis_size  Size of the axis lines, useful to change if large dpi!
#' @param line_size  Line thickness of the lines in plot.
#' @param point_size  Point sizes in plot.
#' @param legend_pos  Position of the legend in plot, matrix where each row corresponds to a legend position is recommended, if length(subset) > 1.
#' @param n_row Number of rows for the three plots. Default is 1.
#' @param n_col Number of columns for the three plots. Default is 3.
#' @return List of several gtable objects where each gtable object is one clinic.
#' @examples
#' # Create trend plot for SU/Mölndal
#' # Look at the 9 data sets to see the required structure. Data sets are included in the package.
#'
#' example_plot <- prom_trends(eq_vas_exp, eq_vas_obs, eq_vas_riket, pain_exp,
#'                             pain_obs, pain_riket, satis_exp, satis_obs, satis_riket,
#'                             y_breaks = c(5, 0.1, 0.1), subset = 49)
#'
#' plot(example_plot)
#' @export

prom_trends <- function(eq_vas_exp, eq_vas_obs, eq_vas_riket, pain_exp,
                        pain_obs, pain_riket, satis_exp, satis_obs, satis_riket,
                        riket_name = "Riket", y_labs = c("EQ VAS", "Smärt VAS", "Tillfredställelse"),
                        y_breaks = c(5,5,5), year = c('2008/09', '2010/11', '2012/13', '2014/15'), subset=1, text_size = 8,
                        legend_labels = c("Förväntat", "Observerat", "Riket"), line_colors = c("#377EB8", "#E41A1C", "black"),
                        background_color = "moccasin", panel_grid_color = "grey",
                        panel_grid_size = 0.3, axis_size = 0.3, line_size = 0.5, point_size = 1.5, legend_pos = c(0, 0),
                        n_row = 1, n_col = 3){

  library(dplyr) # author Hadley Wickham
  library(ggplot2)
  library(gridExtra)

  # Transforming data to be suitable for ggplot -----------------------------

  ggp_suit <- function(df, year, sheet){
    df$hospital <- ordered(df$hospital, levels = df$hospital)
    # df <- arrange(select(df[,-1], -starts_with("n")), hospital)
    names(df) <- c("hospital", year)

    y <- as.vector(apply(df[,year],1,as.numeric))
    years <- rep(year, dim(df)[1])
    hospital <- sort(rep(df$hospital, dim(df)[2]-1))

    df <- tibble(hospital, years, y, sheet)

    df$years <- ordered(df$years, levels = year)

    return(df)

  }

  # Data transformation -----------------------------------------------------

  # eq_vas_riket <- cbind(eq_vas_riket, "Riket")
  # names(eq_vas_riket) <- c("", year, "hospital")
  #
  # pain_riket <- cbind(pain_riket, "Riket")
  # names(pain_riket) <- c("", year, "hospital")
  #
  # satis_riket <- cbind(satis_riket, "Riket")
  # names(satis_riket) <- c("", year, "hospital")

  eq_vas <- rbind(ggp_suit(eq_vas_exp, year = year, sheet = legend_labels[1]), ggp_suit(eq_vas_obs, year = year, sheet = legend_labels[2]),
                  ggp_suit(eq_vas_riket, year = year, sheet = legend_labels[3]))
  pain <- rbind(ggp_suit(pain_exp, year = year, sheet = legend_labels[1]), ggp_suit(pain_obs, year = year, sheet = legend_labels[2]),
                 ggp_suit(pain_riket, year = year, sheet = legend_labels[3]))
  satis <- rbind(ggp_suit(satis_exp, year = year, sheet = legend_labels[1]), ggp_suit(satis_obs, year = year, sheet = legend_labels[2]),
                    ggp_suit(satis_riket, year = year, sheet = legend_labels[3]))

  eq_vas$sheet <- ordered(eq_vas$sheet, levels = legend_labels)

  pain$sheet <- ordered(pain$sheet, levels = legend_labels)

  satis$sheet <- ordered(satis$sheet, levels = legend_labels)


  hospitals <- unique(satis$hospital)

  # Get right ylim function -------------------------------------------------

  get_ylim <- function(y_vec, y_break){

    top <- ((max(y_vec, na.rm = TRUE) + y_break) %/% y_break) * y_break

    bottom <- (min(y_vec, na.rm = TRUE) %/% y_break) * y_break

    if(top - bottom < 3*y_break){

      top <- (((max(y_vec, na.rm = TRUE) + min(y_vec, na.rm = TRUE))/2 + 2*y_break) %/% y_break) * y_break

      bottom <- (((max(y_vec, na.rm = TRUE) + min(y_vec, na.rm = TRUE))/2 - y_break) %/% y_break) * y_break

    }

    return(c(bottom, top))

  }

  # Ggplots -----------------------------------------------------------------

  plot_list <- list()

  count <- 1

  if(is.vector(legend_pos)){

    legend_pos <- matrix(rep(legend_pos, length(subset)), nrow = length(subset))

  }

  for(i in subset){

    eq_vas_data <- filter(eq_vas, hospital == hospitals[i] | hospital == riket_name)
    pain_data <- filter(pain, hospital == hospitals[i] | hospital == riket_name)
    satis_data <- filter(satis, hospital == hospitals[i] | hospital == riket_name)

    ylim_eq <- get_ylim(eq_vas_data$y, y_breaks[1])
    ylim_sm <- get_ylim(pain_data$y, y_breaks[2])
    ylim_tf <- get_ylim(satis_data$y, y_breaks[3])

    eq_vas_plot <- ggplot(data = eq_vas_data, mapping = aes(x = years, y = y, color = sheet, group = sheet)) +
      scale_y_continuous(breaks = seq(ylim_eq[1], ylim_eq[2], by = y_breaks[1]), limits = ylim_eq) +
      scale_colour_manual(values = line_colors) +
      theme_classic() +
      geom_line(size = line_size, show.legend = TRUE) +
      geom_point(shape = 18, size = point_size, show.legend = TRUE) +
      ylab(y_labs[1]) +
      ggtitle(hospitals[i]) +
      theme(legend.text = element_text(size = text_size),
            panel.grid.major.y = element_line(colour = panel_grid_color, size = panel_grid_size),
            axis.line = element_line(size = axis_size),
            axis.ticks = element_line(size = axis_size),
            panel.background = element_rect(fill = background_color),
            legend.title = element_blank(),
            axis.text = element_text(colour = "black", size = text_size),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = text_size),
            plot.title = element_text(hjust = 0, size = text_size),
            legend.position = legend_pos[count,],
            legend.justification = legend_pos[count,],
            legend.background = element_rect(fill = "transparent"),
            legend.key.height = unit(text_size, "pt"),
            legend.margin = margin(0,0,0,0, unit = "mm"),
            legend.box.margin = margin(0,0,0,0, unit = "mm"),
            plot.margin = margin(0.2,0.4,0.2,0.4, unit = "cm"))

    pain_plot <- ggplot(data = pain_data, mapping = aes(x = years, y = y, color = sheet, group = sheet)) +
      scale_y_continuous(breaks = seq(ylim_sm[1], ylim_sm[2], by = y_breaks[2]), limits = ylim_sm) +
      scale_colour_manual(values = line_colors) +
      theme_classic() +
      geom_line(size = line_size, show.legend = FALSE) +
      geom_point(shape = 18, size = point_size, show.legend = FALSE) +
      ggtitle("") +
      ylab(y_labs[2]) +
      theme(legend.text = element_text(size = text_size),
            panel.grid.major.y = element_line(colour = panel_grid_color, size = panel_grid_size),
            axis.line = element_line(size = axis_size),
            axis.ticks = element_line(size = axis_size),
            panel.background = element_rect(fill = background_color),
            legend.title = element_blank(),
            axis.text = element_text(colour = "black", size = text_size),
            axis.title.y = element_text(size = text_size),
            axis.title.x = element_blank(),
            plot.margin = margin(0.2,0.4,0.2,0.4, unit = "cm"))

    satis_plot <- ggplot(data = satis_data, mapping = aes(x = years, y = y, color = sheet, group = sheet)) +
      scale_y_continuous(breaks = seq(ylim_tf[1], ylim_tf[2], by = y_breaks[3]), limits = ylim_tf) +
      scale_colour_manual(values = line_colors) +
      theme_classic() +
      geom_line(size = line_size, show.legend = FALSE) +
      geom_point(shape = 18, size = point_size, show.legend = FALSE) +
      ggtitle("") +
      ylab(y_labs[3]) +
      theme(legend.text = element_text(size = text_size),
            panel.grid.major.y = element_line(colour = panel_grid_color, size = panel_grid_size),
            axis.line = element_line(size = axis_size),
            axis.ticks = element_line(size = axis_size),
            panel.background = element_rect(fill = background_color),
            legend.title = element_blank(),
            axis.text = element_text(colour = "black", size = text_size),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = text_size),
            plot.margin = margin(0.2,0.4,0.2,0.4, unit = "cm"))


    plot_list[[count]] <- arrangeGrob(eq_vas_plot, pain_plot, satis_plot, nrow = n_row, ncol = n_col)

    count <- count + 1

  }

  if(length(subset) < 2){
    return(arrangeGrob(eq_vas_plot, pain_plot, satis_plot, nrow = n_row, ncol = n_col))

  }else{
    return(plot_list)
  }
}
