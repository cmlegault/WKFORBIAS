# example workflow
# just a placeholder for testing
# convert to vignette later

# don't forget to Build>Install and Restart first
# run devtools::check() to make sure package passes
# run devtools::test() to make sure functions still working

# Example Workflow
# get data from an ASAP run
my.dir <- "C:\\Users\\chris.legault\\Desktop\\testplots"
wkforbias.dir <- find.package("WKFORBIAS")
file.copy(from = file.path(wkforbias.dir, "extdata", "Simple.dat"), to = my.dir, overwrite = TRUE)
# need to run ASAP GUI with Simple.dat to get correct output filenames
# alternatively, shell ASAP3.exe -ind Simple.dat then file.copy(asap3.rdat, Simple.rdat)
# but the latter won't get the rest of the files copied over, so won't be able to open in GUI later
ASAPlist <- readASAP("Simple", my.dir)
myom <- ASAPlist$OM
ASAPoptions <- ASAPlist$ASAPoptions
names(myom)
names(ASAPoptions)

# fill in true values
myomTrue <- fillTrueOM(myom)
names(myomTrue)
myomTrue$NAA

# create observed data
controlObs <- createControlObs()
myomTrue <- genObs(myomTrue, controlObs)

# set some default ASAPoptions
ASAPoptions$use_likelihood_constants <- 0 # should not be using likelihood constants
ASAPoptions$index_sel_option[1] <- 2 # for Simple.dat because index_sel_option not in rdat file

# write ASAP input file
writeASAP(myomTrue, ASAPoptions, "test", "C:\\Users\\chris.legault\\Desktop\\testwriteASAP")
