filelist <-  c("analysis/file1.R", "analysis/file2.R", "analysis/markdown.Rmd",
 "dep.RData", "Makefile", "maker.Rproj", "man/hello.Rd",
 "mtcars.csv", "mtcars.RData", "output.txt", "R/dep_graph.R",
 "R/hello.R", "R/make_maker.R")

dependencies <- data_frame(
 file    = c("analysis/file2.R", "analysis/markdown.Rmd", "mtcars.csv",
 						"mtcars.RData", "analysis/file2.R"),
 pre_req = c("mtcars.csv", "mtcars.RData", "analysis/file1.R",
 					"analysis/file2.R", "R/hello.R"))

graph_dependencies(filelist, dependencies)

easy_make(dependencies, path = "tests/Makefile")
