# example workflow
# just a placeholder for testing
# convert to vignette later
# don't forget to Build>Install and Restart first
# also good to run devtools::test() to make sure everything still working
# have to comment code to get it to compile in package, just uncomment block below and run

## uncomment starting here ##

# # get data from an ASAP run
# myom <- readASAP("simple", "C:\\Users\\chris.legault\\Desktop\\testplots")
# names(myom)
# 
# # fill in true values
# myomTrue <- fillTrueOM(myom)
# names(myomTrue)
# myomTrue$NAA
# myomTrue$Mlist$values[,1] <- 0.9 # age 1 changed to make sure matrix writing is correct
# 
# # set some default ASAPoptions
# ASAPoptions <- list()
# ASAPoptions$nselblocks <- 1
# 
# # write ASAP input file
# writeASAP(myomTrue, ASAPoptions, "test", "C:\\Users\\chris.legault\\Desktop\\testwriteASAP")
