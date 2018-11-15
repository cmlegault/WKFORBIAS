#' Create Index at Age
#' 
#' Create an age-specific index of abundance in numbers.
#' @param PopN vector of population abundance at age
#' @param Mval vector of natural mortality at age or single value applied to all ages
#' @param Fval vector of fishing mortality at age or single value applied to all ages
#' @param indextime proportion of year that occurs before index generated
#' @param indexSelx selectivity at age for gear used to generate index or single value applied to all ages
#' @param indexq catchability of index
#' @export

genIndexAtAge <- function(PopN, Mval, Fval, indextime, indexSelx, indexq){
  Zval <- Mval + Fval
  if (any(is.na(PopN)) || any(is.na(Zval))){
    IAA <- NA
  }else{
    IAA <- PopN * exp(-Zval * indextime) * indexSelx * indexq
  }
  return(IAA)
}
