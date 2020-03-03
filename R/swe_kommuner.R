#' Sweden map data set with township included, compressed version
#'
#'
#' @format A data frame with 11488 rows and 9 variables. This is a filtered
#'   version of swe_kommuner_allpoints where all points with piece equal to "3"
#'   or larger are removed (small details) and only every thirtieth point being
#'   used (see examples). \describe{ \item{long}{longitude, numeric}
#'   \item{lat}{latitude, numeric} \item{order}{specifies the order for each
#'   point, integer} \item{piece}{"1" for the most essential, "2" and more for
#'   detailed points, factor} \item{group}{Each region or island in the map is a
#'   polygon where each level in this variable is a polygon, factor}
#'   \item{ID_1}{id of county, integer} \item{NAME_1}{name of county, character}
#'   \item{ID_2}{id of township, integer} \item{NAME_2}{name of township,
#'   character} }
#'
#' @examples
#' # How the data set was created
#'
#' swe_example <- dplyr::filter(swe_kommuner_allpoints,
#'   piece %in% c("1", "2")) %>%
#'   dplyr::filter(order %% 30 == 1) %>%
#'   droplevels()
#'
#' # Example on how to make map of Sweden using ggplot2.
#' # Note that coord_map() is essential for the map to be in actual scale.
#'
#' ggplot2::ggplot(data = swe_kommuner, ggplot2::aes(x=long, y=lat, group = group)) +
#'   ggplot2::geom_polygon(color = "white", size = 0, fill = "blue")  +
#'   ggplot2::coord_map() +
#'   ggplot2::theme_minimal()
#'
"swe_kommuner"
