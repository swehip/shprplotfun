.onLoad <- function(libname, pkgname) {

  if(packageVersion("ggplot2") < "3.2.0"){
    stop(
      "ggplot2 3.2.0 is required. Please update."
    )

  }

}
