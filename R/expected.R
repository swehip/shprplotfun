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


  require(dplyr)

  ## EQ5D ---------------------------------------------------------------------
  frml <- stats::as.formula(
    sprintf(
      "%s ~   PREP_HipPain    +
      PREP_VASHealth    +
      P_DiaGrp +
      P_Age + P_Gender + PREP_Charnley",
      outcome
    )
  )

  # eq5d0swe   +
  #   eq10 + eq20 + eq30 + eq40 + eq50 +

  add_expected <- function(df, frml, year_period, mean_group, outcome){

    df_names <- c(mean_group, paste0("res", year_period), paste0("n", year_period))

    year_period <- as.symbol(year_period)
    mean_group <- as.symbol(mean_group)
    outcome <- as.symbol(outcome)


    fit <- stats::lm(frml, data = df)
    df$fit_values <- fit$fitted.values

    df <- df %>%
      dplyr::group_by(!!mean_group) %>%
      dplyr::summarise(res = mean(fit_values, na.rm = TRUE),
                       n = sum(!is.na(!!outcome)))

    names(df) <- df_names

    df
  }

  data1 <- add_expected(data1, frml, year_period1, mean_group, outcome)
  data2 <- add_expected(data2, frml, year_period2, mean_group, outcome)
  data3 <- add_expected(data3, frml, year_period3, mean_group, outcome)
  data4 <- add_expected(data4, frml, year_period4, mean_group, outcome)

  mean_group <- as.symbol(mean_group)

  dplyr::full_join(data1, data2) %>%
    dplyr::full_join(data3) %>%
    dplyr::full_join(data4) %>%
    dplyr::arrange(!!mean_group)
}
