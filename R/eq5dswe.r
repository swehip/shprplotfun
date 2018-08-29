#' Simple function to extract dimensions of EQ-5D profile
#'
#' Extracts a specific one-digit number from a five-digit number.
#' @param input EQ-5D profile, usually a five-digit number.
#' @param place Number from 1 to 5 indicating which number to extract.
#' @return Number if input is a five-digit number, else NA.
#' @examples
#' eq5d_3l(input = 12312, 3)
#' # [1] 3
#'
#' eq5d_3l(input = NA, 3)
#' # [1] NA
#'
#' eq5d_3l(input = 1231, 3)
#' # [1] NA
#'
#' eq5d_3l(input = c(12312, NA, 1231, 11121), 3)
#' # [1]  3 NA NA  1
#' @export

eq5d_3l <- function(input, place){
  if(is.factor(input)){
    input <- as.character(input)
  }
    ret <- ifelse(nchar(input)!=5| is.na(input), NA, substr(input, place, place))
    ret <- as.numeric(ret)
    return(ret)
}

#' Calculate Swedish EQ-5D-index using all five values extracted from eq5d_3l()
#'
#' Calculates Swedish EQ-ED-index using a known linear model (Model 4).
#' @param Mobility First number extracted from eq5d_3l().
#' @param Selfcare Second number extracted from eq5d_3l().
#' @param Usual Third number extracted from eq5d_3l().
#' @param Pain Fourth number extracted from eq5d_3l().
#' @param Anxiety Fifth number extracted from eq5d_3l().
#' @return Number, EQ-5D-index.
#' @examples
#' # Returns intercept
#' eqindSwed(1,1,1,1,1)
#' # [1] 0.9694
#'
#' eqindSwed(NA,1,1,1,1)
#' # [1] NA
#'
#' eqindSwed(5,1,1,1,1)
#' # [1] NA
#' @export

eqindSwed <- function(Mobility, Selfcare, Usual, Pain, Anxiety) {

  Mobility[!Mobility %in% 1:3] <- NA
  Selfcare[!Selfcare %in% 1:3] <- NA
  Usual[!Usual %in% 1:3] <- NA
  Pain[!Pain %in% 1:3] <- NA
  Anxiety[!Anxiety %in% 1:3] <- NA

  mobility <- ifelse (Mobility==1, 0, ifelse(Mobility == 2, -0.0666, -0.1247))
  selfcare <- ifelse (Selfcare==1, 0, -0.0276)
  usual    <- ifelse (Usual==1,    0, ifelse(Usual    == 2, -0.1012, -0.1355))
  pain     <- ifelse (Pain==1,     0, ifelse(Pain     == 2, -0.0345, -0.0904))
  anxiety  <- ifelse (Anxiety==1,  0, ifelse(Anxiety  == 2, -0.0552, -0.2077))
  N3       <- ifelse((Mobility==3 |
                      Selfcare==3 |
                      Usual==3    |
                      Pain==3     | Anxiety==3), -0.0433, 0)

  EQind <- 0.9694 + mobility + selfcare + usual + pain + anxiety + N3

                  return(EQind)
}


#' Calculate Brittish EQ-5D-index using all five values extracted from eq5d_3l()
#'
#' Calculates Brittish EQ-ED-index using a known linear model (Dolan).
#' @param Mobility First number extracted from eq5d_3l().
#' @param Selfcare Second number extracted from eq5d_3l().
#' @param Usual Third number extracted from eq5d_3l().
#' @param Pain Fourth number extracted from eq5d_3l().
#' @param Anxiety Fifth number extracted from eq5d_3l().
#' @return Number, EQ-5D-index.
#' @examples
#' # Returns intercept
#' eqindBrit(1,1,1,1,1)
#' # [1] 1
#'
#' eqindBrit(NA,1,1,1,1)
#' # [1] NA
#'
#' eqindBrit(5,1,1,1,1)
#' # [1] NA
#' @export

eqindBrit <- function(Mobility, Selfcare, Usual, Pain, Anxiety) {

  Mobility[!Mobility %in% 1:3] <- NA
  Selfcare[!Selfcare %in% 1:3] <- NA
  Usual[!Usual %in% 1:3] <- NA
  Pain[!Pain %in% 1:3] <- NA
  Anxiety[!Anxiety %in% 1:3] <- NA

  mobility <- ifelse (Mobility==1, 0, ifelse(Mobility == 2, -0.069, -0.314))
  selfcare <- ifelse (Selfcare==1, 0, ifelse(Selfcare == 2, -0.104, -0.214))
  usual    <- ifelse (Usual==1,    0, ifelse(Usual    == 2, -0.036, -0.094))
  pain     <- ifelse (Pain==1,     0, ifelse(Pain     == 2, -0.123, -0.386))
  anxiety  <- ifelse (Anxiety==1,  0, ifelse(Anxiety  == 2, -0.071, -0.236))
  N2       <- ifelse((Mobility %in% c(2, 3)|
                        Selfcare %in% c(2, 3)|
                        Usual    %in% c(2, 3)|
                        Pain     %in% c(2, 3)|
                        Anxiety  %in% c(2, 3)), -0.081, 0)

  N3       <- ifelse((Mobility==3 |
                        Selfcare==3 |
                        Usual==3    |
                        Pain==3     | Anxiety==3), -0.269, 0)

  EQind <- 1 + mobility +selfcare + usual + pain + anxiety + N2+N3

  return(EQind)
}

