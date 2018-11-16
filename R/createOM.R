#' Create Operating Model
#' 
#' Define all necessary settings for creating true population values and observations with noise.
#' @param years vector of years in operating model (OM)
#' @param nages number of ages in OM (assumed to start at age 1)
#' @param nindices number of indices
#' @param nyear1flag N in first year option
#' @param Rflag recruitment option
#' @param Mflag natural mortality option
#' @param Fflag fishing mortality option
#' @param Wflag weights at age option
#' @param indexflag list of index options
#' @param catcherrorflag option for observation errors in catch
#' @param indexerrorflag list of options for observation errors in the indices
#' @param processerrorflag option for process error in survival equation, default=FALSE
#' @export

createOM <- function(years, nages, nindices, nyear1flag, Rflag, Mflag, Fflag, Wflag, indexflag, catcherrorflag, indexerrorflag, processerrorflag=FALSE){
  om <- list()
  om$inputs <- list()
  # save the input values
  om$inputs$years <- years
  om$inputs$nages <- nages
  om$inputs$nindices <- nindices
  om$inputs$nyear1flag <- nyear1flag
  om$inputs$Rflag <- Rflag
  om$inputs$Mflag <- Mflag
  om$inputs$Fflag <- Fflag
  om$inputs$Wflag <- Wflag
  om$inputs$indexflag <- indexflag
  om$inputs$catcherrorflag <- catcherrorflag
  om$inputs$indexerrorflag <- indexerrorflag
  om$inputs$processerrorflag <- processerrorflag
  
  # create blank matrices
  nyears <- length(years)
  blankmat <- matrix(NA, nrow=nyears, ncol=nages)
  om$NAA <- blankmat
  om$FAA <- blankmat
  om$MAA <- blankmat
  om$WAA <- blankmat
  om$indices <- list()
  for (ind in 1:nindices){
    om$indices[[ind]] <- list()
    om$indices[[ind]]$IAA <- blankmat
  }
  
  return(om)
}

# example call createOM(1994:2018, 6, 3, 1, 1, 1, 1, 1, 1, 1, 1)