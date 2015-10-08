io <- read.csv("data/data_raw/R I-O function .csv")
input  <- as.character(io[ io$I.O == "input", "Name"])
output <- as.character(io[ io$I.O == "output", "Name"])
save( list = c("input", "output"), file = "data/io.RData")
