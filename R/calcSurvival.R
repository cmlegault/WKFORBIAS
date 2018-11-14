#' Cohort Survival
#' 
#' Calculate survival of a cohort with optional plus group calculation given PopN, M, and F.
#' @param PopN population abundance, can be number or vector
#' @param Mval natural mortality rate, can be number or same dimension as PopN
#' @param Fval fishing mortality rate, can be number or same dimension as PopN
#' @param plusgroupflag means last element in vector or matrix is a plus group, default = TRUE
#' @export
 
calcSurvival <- function(PopN, Mval, Fval, plusgroupflag=TRUE){
  Zval <- Mval + Fval
  if (any(is.na(PopN)) || any(is.na(Zval))){
    Survivors <- NA
  }else{
    Survivors <- PopN * exp(-Zval)
    maxdim <- length(PopN)
    if (maxdim > 1 && plusgroupflag == TRUE){
      if (length(Zval) == 1){
        Zuse <- Zval
      }else{
        Zuse <- Zval[(maxdim - 1)]
      }
      Survivors[maxdim] <- Survivors[maxdim] + PopN[(maxdim - 1)] * exp(-Zuse)
    }
  }
  return(Survivors)
}
