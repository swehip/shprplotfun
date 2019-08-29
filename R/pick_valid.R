#' Help function for add_prom
#'
#' If there are several PROM on same date, this function populate rows with existing PROM values.

pick_valid <- function(x) {
  x <- unique(x)
  y <- x[!is.na(x)]

  if (length(y) > 1) {
    print("This patient has several PROM-values, one is chosen randomly:")
    print(y)
    y <- sample(y, 1)
    x <- y

  } else if (length(x) > 1) {
    x <- y
  }


  return(x)


}
