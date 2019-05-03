#' Generate observations from true OM
#' 
#' @param om operating model list from fillTrueOM
#' @export

genObs <- function(om, controlObs){
  
  # apply multinomial to true catch at age using ESSCAA
  if (length(controlObs$ESSCAA) == 1){
    ess <- rep(controlObs$ESSCAA, om$nYear)
  } else if(length(controlObs$ESSCAA == om$nYear)){
    ess <- controlObs$ESSCAA
  } else {
    return("Error: ESSCAA must have length = 1 or nYear")
  }
  om$CAAObs <- om$CAA
  for (iyear in 1:om$nYear){
    om$CAAObs[iyear, ] <- addMultinomialError(om$CAA[iyear, ], ess[iyear])
  }
  
  # apply lognormal distribution to generate observed catch in total weight
  sigma <- sqrt(log(1 + controlObs$CtotCV ^ 2))
  om$CtotObs <- addLognormalError(om$Ctot, sigma)
  return(om)
}
