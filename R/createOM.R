#' Create Operating Model
#' 
#' Define all necessary settings for creating true population values and observations with noise.
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

createOM <- function(years, nAge, nInd, nyear1list, Rlist, Mlist, Flist, Wlist, indexlist, catcherrorlist, indexerrorlist, processerrorlist=FALSE){
  om <- list()
  om$inputs <- list()
  # save the input values
  om$inputs$years <- years
  om$inputs$nAge <- nAge
  om$inputs$nInd <- nInd
  om$inputs$nyear1list <- nyear1list
  om$inputs$Rlist <- Rlist
  om$inputs$Mlist <- Mlist
  om$inputs$Flist <- Flist
  om$inputs$Wlist <- Wlist
  om$inputs$indexlist <- indexlist
  om$inputs$catcherrorlist <- catcherrorlist
  om$inputs$indexerrorlist <- indexerrorlist
  om$inputs$processerrorlist <- processerrorlist
  
  # create blank matrices
  nYear <- length(years)
  blankmat <- matrix(NA, nrow=nYear, ncol=nAge)
  om$NAA <- blankmat
  om$FAA <- blankmat
  om$MAA <- blankmat
  om$WAA <- blankmat
  om$indices <- list()
  for (ind in 1:nInd){
    om$indices[[ind]] <- list()
    om$indices[[ind]]$IAA <- blankmat
  }
  
  return(om)
}

# example call createOM(1994:2018, 6, 3, 1, 1, 1, 1, 1, 1, 1, 1)