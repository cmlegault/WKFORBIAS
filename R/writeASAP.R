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
  write(om$nyears, file=ifile, append=TRUE)
  write("# First Year", file=ifile, append=TRUE)
  write(om$years[1], file=ifile, append=TRUE)
  write("# Number of Ages", file=ifile, append=TRUE)
  write(om$nages, file=ifile, append=TRUE)
  write("# Number of Fleets", file=ifile, append=TRUE)
  write("1", file=ifile, append=TRUE)  # assume only one fleet
  write("# Number of Selectivity Blocks", file=ifile, append=TRUE)
  write(ASAPoptions$nselblocks, file=ifile, append=TRUE)
  write("# Number of Available Indices", file=ifile, append=TRUE)
  write(om$nindices, file=ifile, append=TRUE)
  write("# Natural Mortality", file=ifile, append=TRUE)
  write(t(om$Mlist$values), file=ifile, append=TRUE, ncolumns=om$nages) # rem transpose matrix
  
  # add filewriteOK <- TRUE when ASAP input file successfully written
  return(filewriteOK)
}
