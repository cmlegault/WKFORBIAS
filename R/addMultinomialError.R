#' Add multinomial error
#' 
#' Randomly sample from vector that is converted to proportions with input effective sample size.
#' @param obs a vector of starting values
#' @param ess effective sample size
#' @param randomval seed value for random selection, default = NULL
#' @export

addMultinomialError <- function(obs, ess, randomval=NULL){
  if (any(is.na(obs)) == TRUE || any(is.na(ess)) == TRUE || sum(obs) <= 0){
    applied <- NA
  }else{
    if (is.null(randomval) == TRUE){
      # don't know how to use repeatable random draws for multinomial
    }
    sumobs <- sum(obs)
    rmultdraw <- stats::rmultinom(1, ess, prob = (obs / sumobs))
    applied <- rmultdraw * sumobs / ess
  }
  return(applied)
}
