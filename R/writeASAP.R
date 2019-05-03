#' Write ASAP data file 
#' 
#' Writes an ASAP input based on operating model OM.
#' @param om operating model list containing data
#' @param ASAPoptions list of options to use in the ASAP input file
#' @param ASAPinputFileName name for ASAP input file
#' @param wd working directory to write ASAP input file to
#' @export

## note to self: look into using Tim's write asap function as a starting point

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
  write(om$nindices, file=ifile, append=TRUE)
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

  # add filewriteOK <- TRUE when ASAP input file successfully written
  return(filewriteOK)
}
