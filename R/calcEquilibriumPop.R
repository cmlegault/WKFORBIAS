#' Equilibrium Population
#' 
#' Computes equilibrium population at age given Recruits, M at age, and F at age.
#' @param eqR number of recruits in equilibrium population
#' @param nages number of ages in population
#' @param Mval vector of natural mortality at age or single value applied to all ages
#' @param Fval vector of fishing mortality at age or single value applied to all ages
#' @param plusgroupflag means last element in vector or matrix is a plus group, default = TRUE
#' @export

calcEquilibriumPop <- function(eqR, nages, Mval, Fval, plusgroupflag=TRUE){
  if (length(Mval) == 1){
    Mval <- rep(Mval, nages)
  }
  if (length(Fval) == 1){
    Fval <- rep(Fval, nages)
  }
  Zval <- Mval + Fval
  if (any(is.na(c(eqR, Zval))) == TRUE){
    eqNAA <- rep(NA, nages)
  }else{
    eqNAA <- rep(eqR, nages)
    for (i in 2:nages){
      eqNAA[i] <- eqNAA[i-1] * exp(-Zval[i-1])
    }
    if (plusgroupflag == TRUE){
      eqNAA[nages] <- eqNAA[nages] / (1 - exp(-Zval[nages]))
    }
  }
  return(eqNAA)
}
