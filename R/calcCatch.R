#' Catch Equation
#' 
#' Compute expected catch given population abundance, natural mortality, and fishing mortality.
#' @param PopN population abundance, can be number, vector, or matrix
#' @param Mval natural mortality rate, can be number or same dimension as PopN
#' @param Fval fishing mortality rate, can be number or same dimension as PopN
#' @export
 
calcCatch <- function(PopN, Mval, Fval){
  Zval <- Mval + Fval
  if (any(is.na(PopN)) || any(is.na(Zval)) || any(Zval == 0)){
    Catch <- NA
  }else{
    Catch <- PopN * Fval * (1 - exp(-Zval)) / Zval
  }
  return(Catch)
}
