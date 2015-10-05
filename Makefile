analysis/file2.R: mtcars.csv R/hello.R	RScript analysis/file2.R 
analysis/markdown.Rmd: mtcars.RData	Rscript -e 'rmarkdown::render(analysis/markdown.Rmd)' 
mtcars.csv: analysis/file1.R	RScript analysis/file1.R 
mtcars.RData: analysis/file2.R	RScript analysis/file2.R 
