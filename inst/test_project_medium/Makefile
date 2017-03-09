all: R/04_Report.Rmd R/04_Report.Rmd Data/bogus_output2.csv Data/bogus_output3.rds Data/bogus_output1.csv
.DELETE_ON_ERROR: 
Data/Analysis_output1.RData: R/03_doAnalysis.R
	Rscript R/03_doAnalysis.R
 
Data/Analysis_output2.RData: R/03_doAnalysis.R
	Rscript R/03_doAnalysis.R
 
Data/bogus_output1.csv: R/01_inputC.R
	Rscript R/01_inputC.R
 
Data/bogus_output2.csv: R/01_inputC.R
	Rscript R/01_inputC.R
 
Data/bogus_output3.rds: R/01_inputC.R
	Rscript R/01_inputC.R
 
Data/partA.RData: R/01_inputA.R
	Rscript R/01_inputA.R
 
Data/partB.RData: R/01_inputB.R
	Rscript R/01_inputB.R
 
Data/partC.rds: R/01_inputC.R
	Rscript R/01_inputC.R
 
Data/Step2_csv2_output.csv: R/02_combine_ABC.R
	Rscript R/02_combine_ABC.R
 
R/01_inputA.R: Data/csv_input.csv Data/stata_input.dta Data/excel_input.xlsx
	--touch R/01_inputA.R
 
R/01_inputB.R: RData_input1.RData
	--touch R/01_inputB.R
 
R/01_inputC.R: character(0)
	--touch R/01_inputC.R
 
R/02_combine_ABC.R: Data/partA.RData Data/partB.RData Data/partC.rds
	--touch R/02_combine_ABC.R
 
R/03_doAnalysis.R: Data/Step2_csv2_output.csv
	--touch R/03_doAnalysis.R
 
R/04_Report.Rmd: Data/Analysis_output1.RData Data/Analysis_output2.RData
	Rscript -e 'rmarkdown::render("R/04_Report.Rmd")'
 
