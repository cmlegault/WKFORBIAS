#' Create control list for observations (determines error structures, CVs, samples sizes, etc.)
#' 
#' @param ESSCAA effective sample size for multinomial distribution of catch at age (default=100)
#' @param CtotCV coefficient of variation for total catch in weight (default=0.05)
#' @export

createControlObs <- function(ESSCAA=100, CtotCV=0.05){
  controlObs <- list()
  controlObs$ESSCAA <- ESSCAA
  controlObs$CtotCV <- CtotCV
  return(controlObs)
}
