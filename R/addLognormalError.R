#' Add lognormal error
#' 
#' Multiply an observation by exp(sigma) with optional bias adjustment of -sigma^2/2.
#' @param obs a numer or vector of starting value(s)
#' @param sigma lognormal standard deviation to apply, can be single value or vector of same length as obs
#' @param biasadjustflag TRUE means to apply bias adjustment, default = FALSE
#' @param randomval number of vector of predetermined random values to use, default = NULL
#' @export


addLognormalError <- function(obs, sigma, biasadjustflag=FALSE, randomval=NULL){
  if (any(is.na(obs)) == TRUE || any(is.na(sigma)) == TRUE){
    applied <- NA
  }else{
    if (is.null(randomval) == TRUE){
      randomval <- rnorm(length(obs))
    }
    applied <- obs * exp(randomval * sigma)
    if (biasadjustflag == TRUE){
      applied <- applied * exp(-0.5 * sigma * sigma)
    }
  }
  return(applied)
}
