all: script2.R
.DELETE_ON_ERROR: 
data.RData: script2.R script2.R
	Rscript script2.R script2.R
 
report.Rmd: data.RData
	Rscript -e 'rmarkdown::render("report.Rmd")'
 
script2.R: mtcars1.csv
	Rscript script2.R
 
