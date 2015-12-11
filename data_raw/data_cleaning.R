io <- read.csv("data_raw/import_export_function.csv",
							 stringsAsFactors = FALSE)
input  <- as.character(io[ io$I.O == "input", "Name"])
output <- as.character(io[ io$I.O == "output", "Name"])
save( list = c("input", "output"), file = "R/sysdata.rda")
