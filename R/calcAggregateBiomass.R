#' Calculate Aggregate Biomass
#' 
#' Multiplies vector of numbers at age by weights at age and adjusts for time of year.
#' @param NAA vector of numbers at age (e.g., PopN, Catch, Index)
#' @param WAA vector of weights at age
#' @param Mval vector of natural mortality at age or single value applied to all ages, default = 0
#' @param Fval vector of fishing mortality at age or single value applied to all ages, default = 0
#' @param indextime proportion of year that occurs before index generated, default = 0 
#' @export

calcAggregateBiomass <- function(NAA, WAA, Mval=0, Fval=0, indextime=0){
  if (indextime == 0){
    Zval <- 0
  }else{
    Zval <- Mval + Fval  
  }
  if (any(is.na(NAA)) || any(is.na(Zval)) || any(is.na(WAA))){
    B <- NA
  }else{
    B <- sum(NAA * WAA * exp(-Zval * indextime))
  }
  return(B)
  
}