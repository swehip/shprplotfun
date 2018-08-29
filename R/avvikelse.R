#' Calculate deviation (avvikelse) for each hospital using a linear model
#'
#' Calculates number of patients, preoperative values, postoperative values, and devation for each hospital. Used to make some tables in the annual report.
#'
#' Following variables MUST be included in the data set to be able to use the function (used to calculate deviation):
#'
#' PREP_HipPain
#'
#' PREP_VASHealth
#'
#' PREP_EQ5D3Lindex
#'
#' eq10
#'
#' eq20
#'
#' eq30
#'
#' eq40
#'
#' eq50
#'
#' P_DiaGrp
#'
#' P_Gender
#'
#' P_Age
#'
#' PREP_Charnley
#'
#' where eq10, eq20, eq30, eq40 and eq50 are the five numbers extracted using eq5d_3l().
#' @param prom_dat PROM-data frame.
#' @param out Linear model outcome variable. Use string.
#' @param preop Preoperative variable. Use string.
#' @param postop Postoperative variable. Use string.
#' @param decimals Number of decimals.
#' @return Data frame including hospitals, number of patients, preoperative values (if preop not NULL), postoperative values, and devation.
#' @export




avvikelse <- function(prom_dat, out, preop = NULL, postop, decimals = 2){


  prom_dat <- na.omit(prom_dat)
  prom_dat <- droplevels(prom_dat)

  frml <- as.formula(sprintf("%s ~   PREP_HipPain +
                                        PREP_VASHealth +
                                        PREP_EQ5D3Lindex +
                                        eq10 + eq20 + eq30 + eq40 + eq50 +
                                        P_DiaGrp +
                                        P_Gender + P_Age + PREP_Charnley", out))

  fit <- lm(frml, data = prom_dat)


  #### Extract the means by hospital

  if(is.null(preop)){
    fited.means <- data.frame(antal     = tapply(prom_dat[,postop],  prom_dat$Unit, length),
                              postop    = tapply(prom_dat[,postop], prom_dat$Unit, mean),
                              avvikelse = tapply(fit$resid, prom_dat$Unit, mean))
  }else{
    fited.means <- data.frame(antal     = tapply(prom_dat[,preop],  prom_dat$Unit, length),
                              preop     = tapply(prom_dat[,preop],  prom_dat$Unit, mean),
                              postop    = tapply(prom_dat[,postop], prom_dat$Unit, mean),
                              avvikelse = tapply(fit$resid, prom_dat$Unit, mean))
  }

  fited.means <- round(fited.means, decimals)


  return(fited.means)
}

