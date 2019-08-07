#' Normalization for compasses
#'
#' Function used to create data frame for compasses.
#' @param x Vector, commonly a column in a data frame.
#' @param riket_index Index where 'riket'-row exist.
#' @return Normalized vector.
#' @examples
#' # Normalize data
#'
#' compass_data[,-1] <- apply(compass_data[,-1], 2, norm_compass)
#'
#' # Make sure that the lowest value is the worst value in every column.
#' # If it is reversed, do like this:
#'
#' compass_data$AE90 <- 1 - compass_data$AE90
#'
#' @export

norm_compass <- function(x, riket_index = length(x)){
  temp      <- x[-riket_index]

  max_riket <- max(temp, na.rm = TRUE)
  min_riket <- min(temp, na.rm = TRUE)

  min_korr  <- min_riket - stats::sd(temp, na.rm = TRUE)
  max_korr  <- max_riket + stats::sd(temp, na.rm = TRUE)
  maxmin    <- max_korr - min_korr

  (x - min_korr) / maxmin
}
