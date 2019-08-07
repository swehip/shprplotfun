#' Compass dataset
#'
#' A dataset containing hospital means for 9 different variables. This dataset
#' was used in 2016 annual report. Used for norm_compass() example, where
#' outcome is used for compass() and compass_riket() examples.
#'
#' @format A data frame with 68 rows and 9 variables:
#' \describe{
#'   \item{Unit}{hospital name (ordered factor)}
#'   \item{Satisfaction}{satisfaction mean (num)}
#'   \item{Pain reduction}{pain reduction before and 1 year after op (num)}
#'   \item{Gain in EQ5D}{gain in EQ5D-index before and 1 year after op (num)}
#'   \item{AE90}{adverse events within 90 days (num)}
#'   \item{Coverage}{coverage (num)}
#'   \item{Reop 2 years}{reoperation within 2 years (num)}
#'   \item{Implant 5 years}{implant survival within 5 years (num)}
#'   \item{Implant 10 years}{implant survival within 10 years (num)}
#' }
"compass_data"
