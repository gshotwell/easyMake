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
#' @importFrom dplyr group_by summarise filter left_join %>% select data_frame select
#' @importFrom tools file_ext
#'
#' @examples
#' 	test_dir <- system.file("test_project", package = "easyMake")
#' easy_make(
#'	detect_dependencies(test_dir),
#'	path = "Makefile_test")
#'
#'	unlink(paste0("test_dir/", Makefile_test"))

easy_make <- function(dependencies = detect_dependencies(),
											render_markdown = TRUE,
											prevent_cycles = TRUE,
											path = "Makefile"){

	g <- igraph::graph.data.frame(dependencies)
	if (!igraph::is_dag(g) & prevent_cycles) {
		stop("Dependicies are cyclic. Check graph_dependecies to eliminate cycles in your project.")
	}

	dependencies$file_type    <- tools::file_ext(dependencies$file)
	dependencies$pre_req_type <- tools::file_ext(dependencies$pre_req)

	all_dependencies <- dependencies %>%
		group_by(file) %>%
		summarise(pre_req = paste(pre_req, collapse = " "),
							file_type = file_type[1],
							pre_req_type = ifelse(
							(n() == 1) | all(pre_req_type %in% c("R", "r")),
								pre_req_type[1], "multiple"))

	r_dependencies <- dependencies %>%
		filter(pre_req_type %in% c("R", "r")) %>%
		group_by(file) %>%
		summarise(R_pre_req = paste(
			paste("\tRscript", pre_req),
			collapse = "\n"))

	dependencies <- left_join(all_dependencies, r_dependencies, by = "file")
	make <- rep("", length.out = length(dependencies$file))

	for (i in seq_along(dependencies$file)) {
		# If target is R, run the target only,
		# If all dependenciesendencies are R, run each dependenciesendency in order
		# If all files are R, run each dependenciesendency in order, then run the target
		if (dependencies$file_type[i] %in% c("R", "r") &
				!(dependencies$pre_req_type[i] %in% c("R", "r"))) {
			make[i] <- paste0(dependencies$file[i], ": ", dependencies$pre_req[i],
												"\n",
												"\tRscript ", dependencies$file[i],
												"\n ")
		} else if ( !(dependencies$file_type[i] %in% c("R", "r")) &
								dependencies$pre_req_type[i] %in% c("R", "r")) {
			make[i] <- paste0(dependencies$file[i], ": ", dependencies$pre_req[i],
												"\n",
												"\tRscript ", dependencies$pre_req[i],
												"\n ")
		} else if (dependencies$file_type[i] %in% c("R", "r") & dependencies$pre_req_type[i] %in% c("R", "r")) {
			make[i] <- paste0(dependencies$file[i], ": ", dependencies$pre_req[i],
												"\n",
												dependencies$R_pre_req[i],
												"\n ",
												"\tRscript ", dependencies$file[i],
												"\n",
												"\n")
		} else if (dependencies$file_type[i] %in% c("Rmd", "rmd") & render_markdown) {
			make[i] <- paste0(dependencies$file[i], ": ", dependencies$pre_req[i],
												"\n",
												"\tRscript -e 'rmarkdown::render(\"",
												dependencies$file[i], "\")'",
												"\n ")
		} else if (dependencies$pre_req_type[i] %in% c("Rmd", "rmd") & render_markdown) {
			make[i] <- paste0(dependencies$file[i], ": ", dependencies$pre_req[i],
												"\n",
												"\tRscript -e 'rmarkdown::render(\"",
												dependencies$pre_req[i], "\")'",
												"\n ")
		}	else {
			next
		}
	}

	make <- c(paste0( "all: ", dependencies[ length(dependencies$file), "file"]),
						".DELETE_ON_ERROR: ",
						make)
	fileConn <- file(path)
	writeLines(make,  fileConn)
	close(fileConn)
}

