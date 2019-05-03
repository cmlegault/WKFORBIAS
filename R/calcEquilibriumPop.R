#' Equilibrium Population
#' 
#' Computes equilibrium population at age given Recruits, M at age, and F at age.
#' @param eqR number of recruits in equilibrium population
#' @param nAge number of ages in population
#' @param Mval vector of natural mortality at age or single value applied to all ages
#' @param Fval vector of fishing mortality at age or single value applied to all ages
#' @param plusgroupflag means last element in vector or matrix is a plus group, default = TRUE
#' @export

calcEquilibriumPop <- function(eqR, nAge, Mval, Fval, plusgroupflag=TRUE){
  if (length(Mval) == 1){
    Mval <- rep(Mval, nAge)
  }
  if (length(Fval) == 1){
    Fval <- rep(Fval, nAge)
  }
  Zval <- Mval + Fval
  if (any(is.na(c(eqR, Zval))) == TRUE){
    eqNAA <- rep(NA, nAge)
  }else{
    eqNAA <- rep(eqR, nAge)
    for (i in 2:nAge){
      eqNAA[i] <- eqNAA[i-1] * exp(-Zval[i-1])
    }
    if (plusgroupflag == TRUE){
      eqNAA[nAge] <- eqNAA[nAge] / (1 - exp(-Zval[nAge]))
    }
  }
  return(eqNAA)
}
