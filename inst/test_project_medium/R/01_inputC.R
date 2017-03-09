

##
## this file will read something in a for-loop
## we do not expect these file to show up in the Makefile
## since this is hard to determine

csv.files <- list.files(path = "Data", pattern = "csv$")
csv.list <- list()

for(this.file in csv.files) {
    csv.list[[length(csv.list) + 1]] <- read.csv(this.file)
}
## these files are all prerequisites for this file...


##
## operate on the list
##

## write some bogus output that we will not need afterwards
write.csv("Data/bogus_output1.csv")

## more:
fwrite("Data/bogus_output2.csv")

## even more
saveRDS("Data/bogus_output3.rds")


##
## but here the stuff we will use in the next script
saveRDS("Data/partC.rds")
