#' easy_make
#' @description Produce a makefile based on
#'
#' @param dependencies
#' A dataframe containing the edge list of dependencies
#' between files. The dataframe should have two fields "file" and "pre_req"
#' showing the file and its immediate pre-requisite respectively. If a file
#' has multiple #' pre-requisites, list each dependency as a separate line
#' in the dataframe.
#' @param render_markdown
#' If TRUE, the Makefile will render any Markdown files, if FALSE markdown
#' files will be ignored.
#' @param path
#' Destination path of the Makefile.
#'
#' @return
#' A Makefile
#' @export
#' @importFrom dplyr group_by summarise filter left_join %>%
#'
#' @examples
#' dependencies <- data_frame(
#' file    = c("analysis/file2.R", "analysis/markdown.Rmd", "mtcars.csv",
#' 						"mtcars.RData", "analysis/file2.R"),
#' pre_req = c("mtcars.csv", "mtcars.RData", "analysis/file1.R",
#' 					"analysis/file2.R", "R/hello.R"))
#' easy_make(dependencies)
#'
easy_make <- function(dependencies,
											render_markdown = TRUE,
											path = "Makefile"){
	dependencies$file_type    <- tools::file_ext(dependencies$file)
	dependencies$pre_req_type <- tools::file_ext(dependencies$pre_req)

	all_dependenciesendencies <- dependencies %>%
		group_by(file) %>%
		summarise(pre_req = paste(pre_req, collapse = " "),
							file_type = file_type[1],
							pre_req_type = ifelse(
								(n() == 1) | all(pre_req_type == "R"),
								pre_req_type[1], "multiple"))

	r_dependenciesendencies <- dependencies %>%
		filter(pre_req_type == "R") %>%
		group_by(file) %>%
		summarise(R_pre_req = paste(
			paste("\tRScript", pre_req),
			collapse = "\r"))

	dependencies <- left_join(all_dependenciesendencies, r_dependenciesendencies, by = "file")
	make <- rep("", length.out = length(dependencies$file))

	for (i in seq_along(dependencies$file)) {
		# If target is R, run the target only,
		# If all dependenciesendencies are R, run each dependenciesendency in order
		# If all files are R, run each dependenciesendency in order, then run the target
		if (dependencies$file_type[i] == "R" & dependencies$pre_req_type[i] != "R") {
			make[i] <- paste0(dependencies$file[i], ": ", dependencies$pre_req[i],
												"\r",
												"\tRScript ", dependencies$file[i],
												"\r ")
		} else if (dependencies$file_type[i] != "R" & dependencies$pre_req_type[i] == "R") {
			make[i] <- paste0(dependencies$file[i], ": ", dependencies$pre_req[i],
												"\r",
												"\tRScript ", dependencies$pre_req[i],
												"\r ")
		} else if (dependencies$file_type[i] == "R" & dependencies$pre_req_type[i] == "R") {
			make[i] <- paste0(dependencies$file[i], ": ", dependencies$pre_req[i],
												"\r",
												dependencies$R_pre_req[i],
												"\r ",
												"\tRScript ", dependencies$file[i],
												"\r",
												"\r")
		} else if (dependencies$file_type[i] == "Rmd" & render_markdown) {
			make[i] <- paste0(dependencies$file[i], ": ", dependencies$pre_req[i],
												"\r",
												"\tRscript -e 'rmarkdown::render(",
												dependencies$file[i], ")'",
												"\r ")
		} else if (dependencies$pre_req_type[i] == "Rmd" & render_markdown) {
			make[i] <- paste0(dependencies$file[i], ": ", dependencies$pre_req[i],
												"\r",
												"\tRscript -e 'rmarkdown::render(",
												dependencies$pre_req[i], ")'",
												"\r ")
		}	else {
			next
		}
	}

	make <- c(paste0( ".all: ", dependencies[ length(dependencies$file), "file"]),
						".DELETE_ON_ERROR: ",
						make)
	fileConn <- file(path)
	writeLines(make,  fileConn)
	close(fileConn)
}

