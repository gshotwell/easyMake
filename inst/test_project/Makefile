all: report.Rmd
.DELETE_ON_ERROR: 
data.RData: script2.R
	Rscript script2.R
 
mtcars1.csv: script1.R
	Rscript script1.R
 
report.Rmd: data.RData
	Rscript -e 'rmarkdown::render("report.Rmd")'
 
script2.R: mtcars1.csv
	--touch script2.R
 
