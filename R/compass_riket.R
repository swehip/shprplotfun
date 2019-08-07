#' Compass riket plot function
#'
#' Radar plot with axis names using ggplot2.
#'
#' @param df                  Data frame.
#' @param axis_titles         Titles on the axis, character vector works.
#' @param poly_color          Color of the polygon.
#' @param color_alpha         Modify color transparency for polygon color.
#'                            Should be the same as alpha_riket for function
#'                              compass.
#' @param title               Title of the compass, use NULL for no title.
#' @param subtitle            Small text under the title, use NULL for no
#'                              subtitle.
#' @param riket_name          Hos to adress the nation.
#' @param title_size          Text size of title in pt.
#' @param subtitle_size       Text size of subtitle in pt.
#' @param axis_text_size      Text size of axis titles in pt.
#' @param line_size           Size of the lines in the choord,
#'                              useful to change if large dpi!
#' @param axis_text_position  Where the text is on the axis.
#' @param v_just              Vertical adjustments to the text on the axis,
#'                              "outward" is default means text is aligned away
#'                              from the center.
#' @param h_just              Horizontal adjustments to the text on the axis,
#'                            "middle" is default.
#'
#' @return ggplot object containing compass plot of Swedish average with text on
#'   the axis.
#' @examples
#' # Create one Swedish average compass with normalized data
#'
#' axis_titles <- c(
#'   "Tillfredst\u00E4llelse",
#'   "Vinst i sm\u00E4rta\nefter 1 \u00E5r",
#'   "EQ5D-vinst\nefter 1 \u00E5r",
#'   "Adverse events\ninom 90 dagar",
#'   "T\u00E4ckningsgrad",
#'   "Reoperation\ninom 2 \u00E5r",
#'   "Implantat-\n     \u00F6verlevnad 5 \u00E5r",
#'   "Implantat-\n\u00F6verlevnad 10 \u00E5r"
#' )
#'
#' compass_riket(norm_compass_data, axis_titles = axis_titles)
#'
#' @export
compass_riket <-
  function(df,
           riket_name = "Riket",
           axis_titles = names(df)[-1],
           poly_color = "#D95F02",
           color_alpha = 0.6,
           title = "Kvalitetsindikatorer",
           subtitle = "v\u00E4rdekompass - riksgenomsnitt",
           title_size = 12,
           subtitle_size = 8,
           axis_text_size = 8,
           line_size = 0.1,
           axis_text_position = 1,
           v_just = "outward",
           h_just = "middle") {

    # Data transformation -----------------------------------------------------
    # Creating new data frame to be able to use ggplot in a smooth way

    riket_df <- df[df[, 1] == riket_name, ]

    category <- axis_titles
    norm_value <- as.numeric(riket_df[, -1])
    enhet <- "Riket"

    riket_df <- dplyr::tibble(enhet, category, norm_value)

    riket_df$category <-
      ordered(riket_df$category, levels = axis_titles)

    # Creating coord ----------------------------------------------------------

    coord_radar <- function(theta = "x", start = 0, direction = 1) {
      theta <- match.arg(theta, c("x", "y"))
      r <- if (theta == "x") "y" else "x"
      ggplot2::ggproto(
        "CordRadar",
        ggplot2::CoordPolar,
        theta = theta,
        r = r,
        start = start,
        direction = sign(direction),
        is_linear = function(coord)
          TRUE
      )
    }

    # Compass function --------------------------------------------------------

  ggplot2::ggplot(data = riket_df, ggplot2::aes(x = category, y = norm_value)) +
      ggplot2::geom_polygon(
        ggplot2::aes(group = enhet),
        fill = poly_color,
        show.legend = FALSE,
        alpha = color_alpha
      ) +
      ggplot2::geom_text(
        ggplot2::aes(x = category, y = axis_text_position),
        label = axis_titles,
        size = axis_text_size * 0.352777778,
        hjust = h_just,
        vjust = v_just
      ) +
      ggplot2::theme_minimal() +
      coord_radar(start = -pi / length(category))   +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(NULL) +
      ggplot2::ggtitle(title, subtitle = subtitle) +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::theme(
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y  = ggplot2::element_blank(),
        plot.title   = ggplot2::element_text(
          hjust = 0.5,
          size = title_size,
          colour = "black",
          margin = ggplot2::margin(t = 0, r = 0, b = 1, l = 0, unit = "pt")
        ),
        plot.subtitle = ggplot2::element_text(
          hjust = 0.5,
          size = subtitle_size,
          colour = "black",
          margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
        ),
        axis.text.x        = ggplot2::element_blank(),
        panel.grid.major   = ggplot2::element_line(colour = "black", size = line_size),
        panel.grid.major.y = ggplot2::element_blank(),
        plot.margin        = ggplot2::margin(0, 0, 0, 0, unit = "mm"),
        validate           = FALSE
      )
  }
