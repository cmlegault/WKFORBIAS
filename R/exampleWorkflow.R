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
# ASAPoptions <- list()
# ASAPoptions$nselblocks <- 1
# 
# # write ASAP input file
# writeASAP(myomTrue, ASAPoptions, "test", "C:\\Users\\chris.legault\\Desktop\\testwriteASAP")
# need to figure out if this is the way to create ASAP input file, or if better to use write_ASAP3_dat_file.R
# pro of 1: easy to use with other input (e.g., SAM) and modified ASAP input
# pro of 2: already written and ready to go
# con of 1: need to finish writing the full function
# con of 2: need to translate from om into asap.dat object, may not be easy/possible if start with something other than ASAP
