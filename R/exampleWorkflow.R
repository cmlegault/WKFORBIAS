# example workflow
# just a placeholder for testing
# convert to vignette later
# don't forget to Build>Install and Restart first
# also good to run devtools::test() to make sure everything still working
# should be able to have R ignore this file when making package - figure out how
# have to comment code to get it to compile in package, just uncomment block below and run
# instead of comment/uncomment, can use .Rbuildignore (see http://r-pkgs.had.co.nz/package.html)

## uncomment starting here ##

# # get data from an ASAP run
# ASAPlist <- readASAP("simple", "C:\\Users\\chris.legault\\Desktop\\testplots")
# myom <- ASAPlist$OM
# ASAPoptions <- ASAPlist$ASAPoptions
# names(myom)
# names(ASAPoptions)
# 
# # fill in true values
# myomTrue <- fillTrueOM(myom)
# names(myomTrue)
# myomTrue$NAA
# 
# # create observed data
# controlObs <- createControlObs()
# myomTrue <- genObs(myomTrue, controlObs)
# 
# # set some default ASAPoptions
# ASAPoptions$use_likelihood_constants <- 0 # should not be using likelihood constants
# ASAPoptions$index_sel_option[1] <- 2 # for Simple.dat because index_sel_option not in rdat file
# 
# # write ASAP input file
# writeASAP(myomTrue, ASAPoptions, "test", "C:\\Users\\chris.legault\\Desktop\\testwriteASAP")
