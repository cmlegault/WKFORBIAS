# example workflow
# just a placeholder for testing
# convert to vignette later
# don't forget to Build>Install and Restart first
# run devtools::check()
# also good to run devtools::test() to make sure everything still working

# Example Workflow
# get data from an ASAP run
my.dir <- "C:\\Users\\chris.legault\\Desktop\\testplots"
file.copy(from = "./examples/Simple.dat", to = my.dir, overwrite = TRUE)
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
