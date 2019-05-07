#' Write ASAP data file 
#' 
#' Writes an ASAP input based on operating model OM.
#' @param om operating model list containing data
#' @param ASAPoptions list of options to use in the ASAP input file
#' @param ASAPinputFileName name for ASAP input file
#' @param wd working directory to write ASAP input file to
#' @export

writeASAP <- function(om, ASAPoptions, ASAPinputFileName, wd){
  filewriteOK <- FALSE
  
  ifile <- paste0(wd, "\\", ASAPinputFileName, ".dat")
  write("# ASAP VERSION 3.0", file=ifile, append=FALSE)
  write("# File created by writeASAP function", file=ifile, append=TRUE)
  write("# Number of Years", file=ifile, append=TRUE)
  write(om$nYear, file=ifile, append=TRUE)
  write("# First Year", file=ifile, append=TRUE)
  write(om$years[1], file=ifile, append=TRUE)
  write("# Number of Ages", file=ifile, append=TRUE)
  write(om$nAge, file=ifile, append=TRUE)
  write("# Number of Fleets", file=ifile, append=TRUE)
  write("1", file=ifile, append=TRUE)  # assume only one fleet
  write("# Number of Selectivity Blocks", file=ifile, append=TRUE)
  write(ASAPoptions$nselblocks, file=ifile, append=TRUE)
  write("# Number of Available Indices", file=ifile, append=TRUE)
  write(om$nInd, file=ifile, append=TRUE)
  write("# Natural Mortality", file=ifile, append=TRUE)
  write(t(om$Mlist$values), file=ifile, append=TRUE, ncolumns=om$nAge) # rem transpose matrix
  write("# Fecundity Option", file=ifile, append=TRUE)
  write("0", file=ifile, append=TRUE) # assumes SSB is used for fecundity
  write("# Maturity", file=ifile, append=TRUE)
  write(t(om$maturitylist$values), file=ifile, append=TRUE, ncolumns=om$nAge)
  write("# Number of Weights at Age Matrices", file=ifile, append=TRUE)
  nwtmats <- 1  # assume only one WAA matrix for now, need to update later
  write(nwtmats, file=ifile, append=TRUE)  
  for (i in 1:nwtmats){
    write(paste0("# Weight Matrix - ", i), file=ifile, append=TRUE)
    write(t(om$Wlist$values), file=ifile, append=TRUE, ncolumns=om$nAge)
  }
  write("# Weights at Age Pointers", file=ifile, append=TRUE) # for now use single WAA for all pointers
  write("1  # catch fleet 1", file=ifile, append=TRUE) 
  write("1  # discards fleet 1", file=ifile, append=TRUE) 
  write("1  # catch all fleets", file=ifile, append=TRUE) 
  write("1  # discards all fleets", file=ifile, append=TRUE) 
  write("1  # SSB", file=ifile, append=TRUE) 
  write("1  # Jan-1", file=ifile, append=TRUE) 
  write("# Selectivity Block Assignment", file=ifile, append=TRUE)
  write("# Fleet 1 Selectivity Block Assignment", file=ifile, append=TRUE)
  for (iyear in 1:om$nYear){
    write(ASAPoptions$fleet_sel_blocks[iyear], file=ifile, append=TRUE)
  }
  write("# Selectivity Options for each block 1=by age, 2=logisitic, 3=double logistic", file=ifile, append=TRUE)
  write(ASAPoptions$fleet_sel_option, file=ifile, append=TRUE, ncolumns=ASAPoptions$nselblocks)
  for (iblock in 1:ASAPoptions$nselblocks){
    startrow <- (iblock - 1) * (om$nAge + 6) + 1
    endrow <- startrow + om$nAge + 6 - 1
    write(paste0("# Selectivity Block #", iblock, " Data"), file=ifile, append=TRUE)
    write(ASAPoptions$fleet_sel_ini[startrow:endrow, ], file=ifile, append=TRUE, ncolumns=4)
  }
  write("# Fleet Start Age", file=ifile, append=TRUE)
  write(ASAPoptions$fleet_sel_start_age, file=ifile, append=TRUE, ncolumns=om$nFleet)
  write("# Fleet End Age", file=ifile, append=TRUE)
  write(ASAPoptions$fleet_sel_end_age, file=ifile, append=TRUE, ncolumns=om$nFleet)
  write("# Age Range for Average F", file=ifile, append=TRUE)
  write(c(ASAPoptions$Freport_agemin, ASAPoptions$Freport_agemax), file=ifile, append=TRUE, ncolumns=2)
  write("# Average F report option (1=unweighted, 2=Nweighted, 3=Bweighted)", file=ifile, append=TRUE)
  write(ASAPoptions$Freport_wtopt, file=ifile, append=TRUE)
  write("# Use Likelihood constants? (1=yes)", file=ifile, append=TRUE)
  write(ASAPoptions$use_likelihood_constants, file=ifile, append=TRUE)
  write("# Release Mortality by Fleet", file=ifile, append=TRUE)
  write(ASAPoptions$release_mort, file=ifile, append=TRUE, ncolumns=om$nFleet)
  write("# Catch Data", file=ifile, append=TRUE) ### note: not yet generalized to nFleet > 1
  write("# Fleet-1 Catch Data", file=ifile, append=TRUE)
  mymat <- cbind(ASAPoptions$catch_comp_mats$catch.fleet1.ob, ASAPoptions$catch_obs[1, ])
  write(t(mymat), file=ifile, append=TRUE, ncolumns=(om$nAge+1))
  write("# Discards", file=ifile, append=TRUE) ### note: not yet generalized to nFleet > 1
  write("# Fleet-1 Discards Data", file=ifile, append=TRUE)
  mymat <- cbind(ASAPoptions$catch_comp_mats$discard.fleet1.ob, ASAPoptions$discard_obs[1, ])
  write(t(mymat), file=ifile, append=TRUE, ncolumns=(om$nAge+1))
  write("# Release Proportions", file=ifile, append=TRUE) ### note: not yet generalized to nFleet > 1
  write("# Fleet-1 Release Data", file=ifile, append=TRUE)
  write(t(ASAPoptions$fleet_prop_release[[1]]), file=ifile, append=TRUE, ncolumns=om$nAge)
  write("# Survey Index Data", file=ifile, append=TRUE)
  write("# Aggregate Index Units", file=ifile, append=TRUE)
  write(ASAPoptions$index_units_aggregate, file=ifile, append=TRUE, ncolumns=om$nInd)
  write("# Age Proportion Index Units", file=ifile, append=TRUE)
  write(ASAPoptions$index_units_proportions, file=ifile, append=TRUE, ncolumns=om$nInd)
  write("# Weight at Age Matrix", file=ifile, append=TRUE)
  write(ASAPoptions$index_WAA_point, file=ifile, append=TRUE, ncolumns=om$nInd)
  write("# Index Month", file=ifile, append=TRUE)
  write(ASAPoptions$index_month, file=ifile, append=TRUE, ncolumns=om$nInd)
  write("# Index Selectivity Link to Fleet", file=ifile, append=TRUE)
  write(ASAPoptions$index_sel_choice, file=ifile, append=TRUE, ncolumns=om$nInd)
  write("# Index Selectivity Options 1=by age, 2=logisitic, 3=double logistic", file=ifile, append=TRUE)
  write(ASAPoptions$index_sel_option, file=ifile, append=TRUE, ncolumns=om$nInd)
  write("# Index Start Age", file=ifile, append=TRUE)
  write(ASAPoptions$index_sel_start_age, file=ifile, append=TRUE, ncolumns=om$nInd)
  write("# Index End Age", file=ifile, append=TRUE)
  write(ASAPoptions$index_sel_end_age, file=ifile, append=TRUE, ncolumns=om$nInd)
  write("# Estimate Proportion (Yes=1)", file=ifile, append=TRUE)
  write(ASAPoptions$index_age_comp_flag, file=ifile, append=TRUE, ncolumns=om$nInd)
  write("# Use Index (Yes=1)", file=ifile, append=TRUE)
  write(ASAPoptions$index_use_flag, file=ifile, append=TRUE, ncolumns=om$nInd)
  
  
  #write("", file=ifile, append=TRUE)
  
  # add filewriteOK <- TRUE when ASAP input file successfully written
  return(filewriteOK)
}
