#' Observed PROM
#'
#' Observed PROM using mean for each mean_group. Used to create observed data frame which is
#' used in \code{\link{prom_trends}}.
#'
#' @param outcome PROM outcome variable.
#' @param data1,data2,data3,data4 Data frames containing data for period 1/2/3/4
#' @param year_period1,year_period2,year_period3,year_period4 Period 1/2/3/4.
#' @param mean_group Group to take mean on.
#' @return Data frame containing observed PROM for four year periods.
#' @export
observed <- function(outcome, data1, data2, data3, data4,
                     year_period1 = "2008_2009",
                     year_period2 = "2010_2011",
                     year_period3 = "2012_2013",
                     year_period4 = "2014_2015",
                     mean_group   = "Unit") {


  add_observed <- function(df, year_period, mean_group, outcome){

    df_names <- c("hospital", paste0("obs", year_period))

    year_period <- as.symbol(year_period)
    mean_group <- as.symbol(mean_group)
    outcome <- as.symbol(outcome)

    df <- df %>%
      dplyr::group_by(!!mean_group) %>%
      dplyr::summarise(obs = mean(!!outcome, na.rm = TRUE))

    names(df) <- df_names

    df
  }

  data1 <- add_observed(data1, year_period1, mean_group, outcome)
  data2 <- add_observed(data2, year_period2, mean_group, outcome)
  data3 <- add_observed(data3, year_period3, mean_group, outcome)
  data4 <- add_observed(data4, year_period4, mean_group, outcome)

  mean_group <- as.symbol(mean_group)

  dplyr::full_join(data1, data2) %>%
    dplyr::full_join(data3) %>%
    dplyr::full_join(data4) %>%
    dplyr::arrange(dplyr::.data$hospital)
}
