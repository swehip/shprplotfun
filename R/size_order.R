#' Make an ordered factor with most important level first
#'
#' Useful when making bar plots and have the most common level in the bottom.
#' Also possible to remove uncommon levels and create new level called other.
#' @param x Vector
#' @param other_count How many levels should be considered as other level
#' @param other_level What the new level should be called
#' @examples
#' library(ggplot2)
#' table(diamonds$cut)
#' in_size_order <- size_order(diamonds$cut)
#' table(in_size_order)
#' in_size_order <- size_order(diamonds$cut, 2, "Others")
#' table(in_size_order)
#' @export
size_order <- function(x, other_count = NULL, other_level = "Övriga") {
  ordning <- names(sort(table(x), decreasing = TRUE))
  x <- ordered(x, levels = ordning)

  if (!is.null(other_count)) {
    n_levels <- length(levels(x)) - other_count
    levels(x) <- c(levels(x)[1:n_levels], rep(other_level, other_count))
  }
  x
}
