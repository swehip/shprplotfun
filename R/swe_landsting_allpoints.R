#' Sweden map data set with county included
#'
#' @format A data frame with 434288 rows and 8 variables. The negligible
#'   variables have been removed from the original data set: \describe{
#'   \item{id}{id of county, character} \item{long}{longitude, numeric}
#'   \item{lat}{latitude, numeric} \item{order}{specifies the order for each
#'   point, integer} \item{piece}{"1" for the most essential, "2" and more for
#'   detailed points (Öland, Orust, and Tjörn included in "2"), factor}
#'   \item{group}{Each region or island in the map is a polygon where each level
#'   in this variable is a polygon, factor} \item{NAME_1}{name of county,
#'   character} \item{VARNAME_1}{alternative name of county, character} }
#'
#' @examples
#'
#' # Example on how to make map of Sweden using ggplot2.
#' # Note that coord_map() is essential for the map to be in actual scale.
#'
#' ggplot2::ggplot(data = swe_landsting_allpoints, ggplot2::aes(x=long, y=lat, group = group)) +
#'   ggplot2::geom_polygon(color = "white", size = 0, fill = "blue")  +
#'   ggplot2::coord_map() +
#'   ggplot2::theme_minimal()
"swe_landsting_allpoints"
