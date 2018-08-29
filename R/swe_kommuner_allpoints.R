#' Sweden map data set with township included
#'
#' @format A data frame with 521384 rows and 9 variables. The negligible variables have been removed from the original data set:
#' \describe{
#'   \item{long}{longitude, numeric}
#'   \item{lat}{latitude, numeric}
#'   \item{order}{specifies the order for each point, integer}
#'   \item{piece}{"1" for the most essential, "2" and more for detailed points, factor}
#'   \item{group}{Each region or island in the map is a polygon where
#'   each level in this variable is a polygon, factor}
#'   \item{ID_1}{id of county, integer}
#'   \item{NAME_1}{name of county, character}
#'   \item{ID_2}{id of township, integer}
#'   \item{NAME_2}{name of township, character}
#' }
#'
#' @examples
#' library(ggplot2)
#'
#' # Example on how to make map of Sweden using ggplot2.
#' # Note that coord_map() is essential for the map to be in actual scale.
#'
#' ggplot(data = swe_kommuner_allpoints, aes(x=long, y=lat, group = group)) +
#'   geom_polygon(color = "white", size = 0, fill = "blue")  +
#'   coord_map() +
#'   theme_minimal()
#'
"swe_kommuner_allpoints"
