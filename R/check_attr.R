#' Check if object has attribute "map"
#'
#' Useful in mutate_if to be used with mutate_label
#' @param x Vector
#' @return TRUE or FALSE
#' @examples
#' df <- data.frame(cars = 1:3)
#' attr(df$cars, "map") <-
#' data.frame(levels = 1:3, labels = c("Volvo", "Saab", "Opel"))
#' check_attr(df$cars)
#' @export
check_attr <- function(x){
  !is.null(attr(x, "map"))
}
