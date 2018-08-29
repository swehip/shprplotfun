#' Compass plot function
#'
#' Radar plot using ggplot2.
#' @param df  Data frame.
#' @param riket_name  Character to find what row is Swedish average, usually
#'   named "Riket".
#' @param ncol  Number of plots on each row.
#' @param riket_color Color of Swedish average polygon.
#' @param hosp_color Color of hospital polygon.
#' @param alpha_riket Modify color transparency for Swedish average color. Use <
#'   1 if colors are to be mixed.
#' @param alpha_hosp Modify color transparency for hospital color. Use < 1 if
#'   colors are to be mixed.
#' @param titles  Character vector containing compass titles. If long titles,
#'   use '\n' to end line!
#' @param title_size  Text size of title in pt.
#' @param line_size  Size of the lines in the choord, useful to change if large
#'   dpi!
#' @return Ggplot object containing plot of compass or many compasses.
#' @examples
#' # Create 6 compasses with normalized data
#'
#' compass(norm_compass_data[c(1:6,68),])
#'
#' @export
compass <-
  function(df,
           riket_name = "Riket",
           ncol = 6,
           riket_color = "#D95F02",
           hosp_color = "#1B9E77",
           alpha_riket = 0.6,
           alpha_hosp = 0.6,
           titles = NULL,
           title_size = 8,
           line_size = 0.1) {

    # Data transformation -----------------------------------------------------
    # Creating new data frame to be able to use ggplot in a smooth way

    category <- vector('character')
    norm_value <- vector('numeric')
    enhet <- vector('character')

    for (i in 1:dim(df)[1]) {
      category <- c(category, names(df)[-1])
      norm_value <- c(norm_value, as.numeric(df[i, -1]))
      enhet <- c(enhet, as.character(rep(df[i, 1], dim(df)[2] - 1)))
    }

    new_df <- tibble(enhet, category, norm_value)

    new_df$category <-
      ordered(new_df$category, levels = names(df)[-1])

    filtered_df <- filter(new_df, enhet != riket_name)
    riket_df <- filter(new_df, enhet == riket_name)

    names(riket_df)[1] <- "riket"

    filtered_df$enhet <-
      ordered(filtered_df$enhet, levels = unique(filtered_df$enhet))


    if (is.character(titles)) {
      hospital_names <- titles

    } else{
      hospital_names <- unique(filtered_df$enhet)

      hospital_names <- ordered(hospital_names)

    }
    # Creating coord ----------------------------------------------------------

    coord_radar <- function (theta = "x",
                             start = 0,
                             direction = 1) {
      theta <- match.arg(theta, c("x", "y"))
      r <- if (theta == "x")
        "y"
      else
        "x"
      ggproto(
        "CordRadar",
        CoordPolar,
        theta = theta,
        r = r,
        start = start,
        direction = sign(direction),
        is_linear = function(coord)
          TRUE
      )
    }

    # Compass function --------------------------------------------------------

    ggplot(data = filtered_df, aes(x = category, y = norm_value)) +
      coord_radar(start = -pi / length(names(df)[-1])) +
      theme_minimal() +
      facet_wrap( ~ enhet,
                  ncol = ncol,
                  labeller = as_labeller(hospital_names)) +
      geom_polygon(
        data = riket_df,
        aes(group = riket),
        fill = riket_color,
        show.legend = FALSE,
        alpha = alpha_riket
      ) +
      geom_polygon(
        aes(group = enhet),
        fill = hosp_color,
        show.legend = FALSE,
        alpha = alpha_hosp
      ) +
      xlab(NULL) +
      ylab(NULL) +
      scale_y_continuous(limits = c(0, 1)) +
      theme(
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.text.x = element_blank(),
        strip.text.x = element_text(size = title_size),
        panel.grid.major = element_line(colour = "black", size = line_size),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(1, 1, 1, 1, unit = "mm")
      )

  }
