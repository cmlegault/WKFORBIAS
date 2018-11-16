#' Beverton Holt Stock Recruitment
#' 
#' Calculates expected value from BH SR curve for given SSB.
#' @param BHalpha Bev-Holt alpha parameter in R = alpha * SSB / (beta + SSB)
#' @param BHbeta Bev-Holt beta parameter in R = alpha * SSB / (beta + SSB)
#' @param SSB one or more SSB values
#' @export

predBevHoltSR <- function(BHalpha, BHbeta, SSB){
  if (any(is.na(c(BHalpha, BHbeta, SSB))) == TRUE){
    BHR <- NA
  }else{
    BHR <- BHalpha * SSB / (BHbeta + SSB)
  }
  return(BHR)
}
