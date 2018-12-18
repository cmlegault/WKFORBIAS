#' Fill True OM values
#' 
#' @param om operating model list from createOM
#' @export

fillTrueOM <- function(om){
  
  blankmat <- matrix(NA, nrow=om$nyears, ncol=om$nages)
  if (is.null(om$NAA)) om$NAA <- blankmat
  if (is.null(om$FAA)) om$FAA <- blankmat
  if (is.null(om$MAA)) om$MAA <- blankmat
  if (is.null(om$WAA)) om$WAA <- blankmat
  if (is.null(om$indices)){ 
    om$indices <- list()
    for (ind in 1:om$nindices){
      om$indices[[ind]] <- list()
      om$indices[[ind]]$IAA <- blankmat 
    }
  }
   
  if (om$Flist$type == "constant"){
    om$FAA <- om$Flist$values
  }
  
  if (om$Mlist$type == "constant"){
    om$MAA <- om$Mlist$values
  }
  
  if (om$Wlist$type == "constant"){
    om$WAA <- om$Wlist$values
  }
  
  if (om$nyear1list$type == "constant"){
    om$NAA[1, ] <- om$nyear1list$values
  }
  
  if (om$Rlist$type == "constant"){
    om$NAA[, 1] <- om$Rlist$values
  }
  
  # fill in NAA matrix using F and M matrices
  for (iyear in 1:(om$nyears-1)){
    survs <- calcSurvival(om$NAA[iyear, ], om$MAA[iyear, ], om$FAA[iyear], TRUE)
    om$NAA[(iyear+1), 2:(om$nages)] <- survs[-(om$nages - 1)]
  }
  
  return(om)  
}

# example call 
# myomTrue <- fillTrueOM(myom)
# names(myomTrue)