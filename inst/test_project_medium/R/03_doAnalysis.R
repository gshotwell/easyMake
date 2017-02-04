

##
## read the formerly save csv-file
read.csv("Data/Step2_csv2_output.csv")


##
## do some very important analysis here
##
## and here



##
## and here too
##


##
## I don't know why we save two files and not just one
save(results.1, file = "Data/Analysis_output1.RData")
save(results.2, file = "Data/Analysis_output2.RData")

## better would probably be
## save(list(results.1, results.2), file = "Data/Analysis_output.RData")

