#' Extract attribute "map" and mutate object to be labelled
#'
#' Labelled are used to get right labels when saving to spss using package haven
#' @param x Vector
#' @return Labelled
#' @examples
#' df <- data.frame(cars = 1:3)
#' attr(df$cars, "map") <- data.frame(levels = 1:3, labels = c("Volvo", "Saab", "Opel"))
#' df <- mutate_if(df,
#'                 check_attr,
#'                 mutate_label)
#' @export


mutate_label <- function(x){

  library(haven)

  if(is.numeric(x)){

    labels <- setNames(as.numeric(as.character(attr(x, "map")$levels)),
                       as.character(attr(x, "map")$labels))

  }else if(is.factor(x)){

    labels <- setNames(as.character(attr(x, "map")$levels),
                       as.character(attr(x, "map")$labels))

    x <- as.character(x)

  }else{

    labels <- setNames(attr(x, "map")$levels,
                       attr(x, "map")$labels)

  }

  x <- labelled(x,
                labels)

  return(x)

}
