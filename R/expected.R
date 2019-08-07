#' Expected PROM
#'
#' Expected PROM using linear model. Used to create expected data frame which is
#' used in \code{\link{prom_trends}}.
#'
#' The following variables MUST be included in the data sets
#' function:
#'
#' "PREP_HipPain", "PREP_VASHealth", "eq5d0swe",
#' "eq10", "eq20", "eq30", "eq40", "eq50",
#' "P_DiaGrp", "P_Gender", "P_Age", "PREP_Charnley"
#'
#' where "eq10", "eq20", "eq30", "eq40" and "eq50" are the five numbers
#' extracted using \code{\link{eq5d_3l}} and "eq5d0swe" is obtained using
#'   "eq10", "eq20", "eq30", "eq40", and "eq50" in \code{\link{eqindSwed}}.
#'
#' @param outcome Linear model outcome variable.
#' @param data1,data2,data3,data4 Data frames containing data for period 1/2/3/4
#' @param year_period1,year_period2,year_period3,year_period4 Period 1/2/3/4.
#' @param mean_group Group to take mean on.
#' @return Data frame containing expected PROM for four year periods.
#' @export
expected <- function(outcome, data1, data2, data3, data4,
                     year_period1 = "2008_2009",
                     year_period2 = "2010_2011",
                     year_period3 = "2012_2013",
                     year_period4 = "2014_2015",
                     mean_group   = "Unit") {

  ## EQ5D ---------------------------------------------------------------------
  frml <- stats::as.formula(
    sprintf(
      "%s ~   PREP_HipPain    +
      PREP_VASHealth    +
      eq5d0swe   +
      eq10 + eq20 + eq30 + eq40 + eq50 +
      P_DiaGrp +
      P_Age + P_Gender + PREP_Charnley",
      outcome
    )
  )

  fit1 <- stats::lm(frml, data = data1)
  fit2 <- stats::lm(frml, data = data2)
  fit3 <- stats::lm(frml, data = data3)
  fit4 <- stats::lm(frml, data = data4)

  #### Extract the means by hospital

  res <- function(fit, data, f) tapply(fit$fitted.values, data[[mean_group]], f)

  fited.means  <-
    data.frame(
      res2008_2009 = res(fit1, data1, mean),
      n2008_2009   = res(fit1, data1, length),

      res2010_2011 = res(fit2, data2, mean),
      n2010_2011   = res(fit2, data2, length),

      res2012_2013 = res(fit3, data3, mean),
      n2012_2013   = res(fit3, data3, length),

      res2014_2015 = res(fit4, data4, mean),
      n2014_2015   = res(fit4, data4, length),
    )

  names(fited.means) <- c(
    paste0("res", year_period1),
    paste0("n",   year_period1),
    paste0("res", year_period2),
    paste0("n",   year_period2),
    paste0("res", year_period3),
    paste0("n",   year_period3),
    paste0("res", year_period4),
    paste0("n",   year_period4)
  )

  fited.means$hospital <- rownames(fited.means)
  fited.means
}
