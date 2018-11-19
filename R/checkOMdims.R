#' Check Operating Model Dimensions
#' 
#' Check that all dimensions of the operating model are consistent (e.g., number of years, ages, indices).
#' @param years vector of years in operating model (OM)
#' @param nages number of ages in OM (assumed to start at age 1)
#' @param nindices number of indices
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

checkOMdims <- function(years, nages, nindices, nyear1list, Rlist, Mlist, Flist, Wlist, indexlist, catcherrorlist, indexerrorlist, processerrorlist=FALSE){

  nyears <- length(years)
  
  checklist <- list()
  checklist$nyear1 <- ifelse(length(nyear1list$values) == nages, TRUE, FALSE)
  checklist$R <- ifelse(length(Rlist$values) == nyears, TRUE, FALSE)
  checklist$M <- ifelse(dim(Mlist$values) == c(nyears, nages), TRUE, FALSE)
  checklist$F <- ifelse(dim(Flist$values) == c(nyears, nages), TRUE, FALSE)
  checklist$W <- ifelse(dim(Wlist$values) == c(nyears, nages), TRUE, FALSE)
  checklist$nindices <- ifelse(length(indexlist) == nindices, TRUE, FALSE)
  checklist$index <- list()
  for (ind in 1:nindices){
    checklist$index[[ind]] <- ifelse(dim(indexlist$index[[ind]]$values) == c(nyears, nages), TRUE, FALSE)
  }
  checklist$catcherror <- ifelse(dim(catcherrorlist$values) == c(nyears, nages), TRUE, FALSE)
  checklist$nindexerror <- ifelse(length(indexerrorlist) == nindices, TRUE, FALSE)
  for (ind in 1:nindices){
    checklist$indexerror[[ind]] <- ifelse(dim(indexerrorlist$index[[ind]]$values) == c(nyears, nages), TRUE, FALSE)
  }
  checklist$processerror <- ifelse(dim(processerrorlist$values) == c(nyears, nages), TRUE, FALSE)
  
  return(checklist)
}

# use all(unlist(checklist) == TRUE) to see if all checks passed
# need to add tests!

