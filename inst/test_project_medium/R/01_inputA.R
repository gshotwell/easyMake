

library(haven)
library(data.table)

## read in some data from stata
a1 <- as.data.table(read_dta("Data/stata_input.dta"))

## read in some data from a csv file
a2 <- fread(input = "Data/csv_input.csv")

## and finally something from an xlsx file
a3 <- read.xlsx("Data/excel_input.xlsx")

##
## combine them somehow
##

## is there a lorem ipsum for code?

##
## more comments
##

##
## finally save something
save(a.combined, file = "Data/partA.RData")



