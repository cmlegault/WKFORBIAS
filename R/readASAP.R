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
  OM$sourcedetails <- list()
  OM$sourcedetails$asap.name <- asap.name
  OM$sourcedetails$wd <- wd
  
  OM$years <- seq(asap$parms$styr, asap$parms$endyr)
  OM$nYear <- length(OM$years)
  OM$nAge <- asap$parms$nages
  OM$nInd <- asap$parms$nindices
  OM$nFleet <- asap$parms$nfleets
  if (OM$nFleet > 1){
    return("Error: nFleet > 1 not supported yet")
  }
  
  OM$nyear1list <- list()
  OM$nyear1list$type <- "constant"
  OM$nyear1list$values <- asap$N.age[1, ]
  
  OM$Rlist <- list()
  OM$Rlist$type <- "constant"
  OM$Rlist$values <- asap$N.age[, 1]
  
  OM$Mlist <- list()
  OM$Mlist$type <- "constant"
  OM$Mlist$values <- asap$M.age
  
  OM$Flist <- list()
  OM$Flist$type <- "constant"
  OM$Flist$values <- asap$F.age
  
  OM$maturitylist <- list()
  OM$maturitylist$type <- "constant"
  OM$maturitylist$values <- asap$maturity
  
  OM$Wlist <- list()
  OM$Wlist$type <- "constant"
  OM$Wlist$values <- asap$WAA.mats$WAA.catch.all # note: doesn't handle discards
  
  OM$indexlist <- list()
  for (ind in 1:OM$nInd){
    OM$indexlist[[ind]] <- list()
    OM$indexlist[[ind]]$selxtype <- "constant"
    OM$indexlist[[ind]]$selxvalues <- asap$index.sel[ind, ]
    OM$indexlist[[ind]]$qtype <- "constant"
    OM$indexlist[[ind]]$qvalues <- asap$q.indices[ind]
  }
  
  OM$catcherrorlist <- list()
  OM$catcherrorlist$type <- "observation error only"
  OM$catcherrorlist$CVvalues <- as.numeric(asap$control.parms$catch.tot.cv)  # assumes only one fleet
  OM$catcherrorlist$ESSvalues <- as.numeric(asap$fleet.catch.Neff.init)
  
  OM$indexerrorlist <- list()
  OM$indexerrorlist$type <- "observation error only"
  for (ind in 1:OM$nInd){
    OM$indexerrorlist[[ind]] <- list()
    OM$indexerrorlist[[ind]]$CVvalues <- asap$index.cv[[ind]] # assumes all years have index
    OM$indexerrorlist[[ind]]$ESSvalues <- asap$index.Neff.init[ind, ]
  }
  
  OM$processerrorlist <- FALSE
  
  # create list of ASAP options
  ASAPoptions <- list()
  ASAPoptions$nselblocks <- asap$parms$nselblocks
  ASAPoptions$fleet_sel_blocks <- as.numeric(asap$fleet.sel.blocks)
  ASAPoptions$fleet_sel_option <- asap$fleet.sel.option
  ASAPoptions$fleet_sel_ini <- asap$sel.input.mats$fleet.sel.ini
  ASAPoptions$fleet_sel_start_age <- asap$fleet.sel.start.age
  ASAPoptions$fleet_sel_end_age <- asap$fleet.sel.end.age
  ASAPoptions$Freport_agemin <- asap$options$Freport.agemin
  ASAPoptions$Freport_agemax <- asap$options$Freport.agemax
  ASAPoptions$Freport_wtopt <- asap$options$Freport.wtopt
  
  return(list(OM=OM, ASAPoptions=ASAPoptions))
}

# example call
# myom <- readASAP("simple", "C:\\Users\\chris.legault\\Desktop\\testplots")
# names(myom)
