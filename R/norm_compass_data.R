#' Normalized compass dataset
#'
#' A dataset containing normalized hospital means for 9 different variables.
#' This dataset was used in 2016 annual report. The dataset is the normalized
#' version of compass_data, normalized with norm_compass(). Dataset is used for
#' compass() and compass_riket() examples.
#'
#' @format A data frame with 68 rows and 9 variables:
#'   \describe{
#'     \item{Unit}{hospital name, ordered factor}
#'     \item{Satisfaction}{normalized satisfaction mean, numeric}
#'   \item{Pain reduction}{normalized pain reduction before operation and
#'     1 year after (numeric)}
#'   \item{Gain in EQ5D}{normalized gain in EQ5D-index before operation and 1 year after (numeric)}
#'   \item{AE90}{normalized adverse events within 90 days, numeric}
#'   \item{Coverage}{normalized coverage, numeric}
#'   \item{Reop 2 years}{normalized reoperation within 2 years, numeric}
#'   \item{Implant 5 years}{normalized implant survival within 5 years, numeric}
#'   \item{Implant 10 years}{normalized implant survival within 10 years, numeric}
#'}
"norm_compass_data"
