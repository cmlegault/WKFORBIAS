#' Read ASAP as input to OM
#' 
#' Uses an ASAP input file and associated rdat file to set up an operating model.
#' @param asap.name filename without extension of the ASAP run
#' @param wd directory where the ASAP run is located
#' @export
 
readASAP <- function(asap.name, wd){
  
  asapdatname <- paste0(wd, "\\", asap.name, ".dat")
  asaprdatname <- paste0(wd, "\\", asap.name, ".rdat")
  
  if (!file.exists(asapdatname) | !file.exists(asaprdatname) == TRUE){
    return("Missing ASAP file")
  }
  
  asapdat <- readLines(asapdatname)
  asap <- dget(asaprdatname)
  
  # create operating model list
  OM <- list()
  OM$source <- "readASAP"
  OM$readASAP <- list()
  OM$readASAP$asap.name <- asap.name
  OM$readASAP$wd <- wd
  
  
  return(OM)
}
