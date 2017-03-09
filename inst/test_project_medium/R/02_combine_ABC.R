

##
## read stuff from previous scripts
##
load("Data/partA.RData")
load("Data/partB.RData")

partC <- readRDS("Data/partC.rds")



##
## do some magic combination of the inputs

## !!!



##
## Tadaaa...
##
## here we have some final output
write.csv2(combined.ABC, file = "Data/Step2_csv2_output.csv")
