#' Check Operating Model Dimensions
#' 
#' Check that all dimensions of the operating model are consistent (e.g., number of years, ages, indices). Use all(unlist(checklist) == TRUE) to see if all checks passed.
#' @param years vector of years in operating model (OM)
#' @param nAge number of ages in OM (assumed to start at age 1)
#' @param nInd number of indices
#' @param nyear1list N in first year list
#' @param Rlist recruitment list
#' @param Mlist natural mortality list
#' @param Flist fishing mortality list
#' @param Wlist weights at age list
#' @param indexlist list of index lists
#' @param catcherrorlist list for observation errors in catch
#' @param indexerrorlist list of lists for observation errors in the indices
#' @param processerrorlist list for process error in survival equation, default=FALSE
#' @export

checkOMdims <- function(years, nAge, nInd, nyear1list, Rlist, Mlist, Flist, Wlist, indexlist, catcherrorlist, indexerrorlist, processerrorlist=FALSE){

  nYear <- length(years)
  
  checklist <- list()
  checklist$nyear1 <- ifelse(length(nyear1list$values) == nAge, TRUE, FALSE)
  checklist$R <- ifelse(length(Rlist$values) == nYear, TRUE, FALSE)
  checklist$M <- ifelse(dim(Mlist$values) == c(nYear, nAge), TRUE, FALSE)
  checklist$F <- ifelse(dim(Flist$values) == c(nYear, nAge), TRUE, FALSE)
  checklist$W <- ifelse(dim(Wlist$values) == c(nYear, nAge), TRUE, FALSE)
  checklist$nInd <- ifelse(length(indexlist) == nInd, TRUE, FALSE)
  checklist$index <- list()
  for (ind in 1:nInd){
    checklist$index[[ind]] <- ifelse(dim(indexlist$index[[ind]]$values) == c(nYear, nAge), TRUE, FALSE)
  }
  checklist$catcherror <- ifelse(dim(catcherrorlist$values) == c(nYear, nAge), TRUE, FALSE)
  checklist$nindexerror <- ifelse(length(indexerrorlist) == nInd, TRUE, FALSE)
  for (ind in 1:nInd){
    checklist$indexerror[[ind]] <- ifelse(dim(indexerrorlist$index[[ind]]$values) == c(nYear, nAge), TRUE, FALSE)
  }
  checklist$processerror <- ifelse(dim(processerrorlist$values) == c(nYear, nAge), TRUE, FALSE)
  
  return(checklist)
}



