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

createOM <- function(years, nages, nindices, nyear1flag, Rflag, Mflag, Wflag, indexflag, catcherrorflag, indexerrorflag, processerrorflag=FALSE){
  OM <- list()
  return(OM)
}
