#' Add attributes from one data frame to another
#'
#' Add "map" attributes from df2 to df1.
#' @param df1 Data frame to receive attributes.
#' @param df2 Data frame to give attributes.
#' @return Data frame.
#' @examples
#' df1 <- data.frame(cars = 1:3)
#' df2 <- data.frame(cars = 1:3)
#' attr(df2$cars, "map") <- data.frame(levels = 1:3,
#' labels = c("Volvo", "Saab", "Opel"))
#' df1 <- add_attr(df1, df2)
#' df1$cars
#' @export
add_attr <- function(df1, df2) {
  for (i in names(df1)) {
    attr(df1[[i]], "map") <- attr(df2[[i]], "map")
  }
  df1
}
