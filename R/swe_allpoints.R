#' Sweden map data set
#'
#' A dataset used to create elegant figures of Sweden with ggplot2.
#'
#' @format A data frame with 407898 rows and 5 variables.
#' The negligible variables with just one type of observation
#' have been removed from the original data set:
#' \describe{
#'   \item{long}{longitude, numeric}
#'   \item{lat}{latitude, numeric}
#'   \item{order}{specifies the order for each point, integer}
#'   \item{piece}{"1" if point belongs to mainland,
#'
#'   "2" if point belongs to Gotland (largest island in Sweden),
#'
#'   "3" if point belongs to Öland,
#'
#'   "4" if point belongs to Orust or Tjörn,
#'
#'   "5" if point belongs to Fårö,
#'
#'   everything else if point belongs to any other smaller island, factor}
#'   \item{group}{Each region in the map is a polygon where
#'
#'   "1.1" if point belongs to mainland,
#'
#'   "1.2" if point belongs to Gotland (largest island in Sweden),
#'
#'   "1.3" if point belongs to Öland,
#'
#'   "1.4" if point belongs to Orust or Tjörn,
#'
#'   "1.5" if point belongs to Fårö,
#'
#'   everything else if point belongs to any other smaller island, factor}
#' }
#'
#' @examples
#' library(ggplot2)
#'
#' # Example on how to make map of Sweden using ggplot2.
#' # Note that coord_map() is essential for the map to be in actual scale.
#'
#' ggplot(data = swe_allpoints, aes(x=long, y=lat, group = group)) +
#'   geom_polygon(color = "transparent", fill = "blue")  +
#'   coord_map() +
#'   theme_minimal()
#'
"swe_allpoints"
