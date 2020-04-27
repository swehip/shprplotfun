
# Exporting figures to PNG might be problematic since non-standard fonts are
# used by functions in the package
.onLoad <- function(libname, pkgname) {

  ask <- function(msg) utils::askYesNo(msg, prompts = "Y/N/C")

  # Just warn if package 'extrafont' is not installed
  # Thus, since it is already a suggested package, assume that the decision to
  # not install was actively considered
  if (!requireNamespace("extrafont", quietly = TRUE)) {
    warning("Package 'extrafont' not installed!
            This might cause problems with fonts if you export figures to PNG!")

  # If the package is installed, we can assume that the intention has been to
  # make relevant fonts available
  } else if (
      getOption("shprplotfun_read_fonts", default = interactive()) &&
      !"Arial Narrow" %in% extrafont::fonts() &&
      ask("Register 'Arial Narrow' for correct export to PNG?")) {
    extrafont::font_import()
    if (exists(".rs.restartR") &&
      ask("Restart R to make fonts available?")) {
      call(".rs.restartR")
    }
  }
}
