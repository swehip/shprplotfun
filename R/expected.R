#' Expected PROM
#'
#' Expected PROM using linear model. Used to create expected data frame which is
#' used in prom_trends().
#'
#' Following variables MUST be included in the data set to be able to use the
#' function:
#'
#' PREP_HipPain, PREP_VASHealth, eq5d0swe, eq10, eq20, eq30, eq40, eq50,
#' P_DiaGrp, P_Gender, P_Age, PREP_Charnley
#'
#' where eq10, eq20, eq30, eq40 and eq50 are the five numbers extracted using
#' eq5d_3l() and eq5d0swe is obtained using eq10, eq20, eq30, eq40, and eq50 in
#' eqindSwed().
#' @param outcome Linear model outcome variable.
#' @param data1,data2,data3,data4 Data frames containing data for the
#' first/second/third/forth year period.
#' @param year_period1,year_period1,year_period1,year_period1
#'   First/second/third/fourth year period.
#' @param mean_group Group to take mean on.
#' @return Data frame containing expected PROM for four year periods.
#' @export
expected <- function(outcome,
                     data1,
                     data2,
                     data3,
                     data4,
                     year_period1 = "2008_2009",
                     year_period2 = "2010_2011",
                     year_period3 = "2012_2013",
                     year_period4 = "2014_2015",
                     mean_group = "Unit") {

  ## EQ5D ---------------------------------------------------------------------
  frml <- as.formula(
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

  fit1 <- lm(frml, data = data1)
  fit2 <- lm(frml, data = data2)
  fit3 <- lm(frml, data = data3)
  fit4 <- lm(frml, data = data4)

  #### Extract the means by hospital

  fited.means  <-
    data.frame(
      res2008_2009 = tapply(fit1$fitted.values, data1[[mean_group]], mean),
      n2008_2009 = tapply(fit1$fitted.values, data1[[mean_group]], length),

      res2010_2011 = tapply(fit2$fitted.values, data2[[mean_group]], mean),
      n2010_2011 = tapply(fit2$fitted.values, data2[[mean_group]], length),

      res2012_2013 = tapply(fit3$fitted.values, data3[[mean_group]], mean),
      n2012_2013 = tapply(fit3$fitted.values, data3[[mean_group]], length),

      res2014_2015 = tapply(fit4$fitted.values, data4[[mean_group]], mean),
      n2014_2015 = tapply(fit4$fitted.values, data4[[mean_group]], length)
    )

  names(fited.means) <- c(
    paste0("res", year_period1),
    paste0("n", year_period1),
    paste0("res", year_period2),
    paste0("n", year_period2),
    paste0("res", year_period3),
    paste0("n", year_period3),
    paste0("res", year_period4),
    paste0("n", year_period4)
  )

  fited.means$hospital <- rownames(fited.means)
  fited.means
}
