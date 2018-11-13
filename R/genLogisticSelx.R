#' Logistic selectivity
#' 
#' Create selectivity at age given parameters of a logistic function.
#' @param logisticA50 age at 50 percent selectivity
#' @param logisticSlope slope at 50 percent selectivity
#' @param ages vector of ages to have selectivity calculated
#' @export
 
genLogisticSelx <- function(logisticA50, logisticSlope, ages){
  selx <- 1 / (1 + exp(-logisticSlope * (ages - logisticA50)))
  return(selx)
}
