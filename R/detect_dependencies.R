#' Detect file
#' @description Reads a file, and detects any filename arguments passed
#' to any of the listed functions.
#'

#' @param file
#' The file to be searched.
#' @param function_list
#' The list of functions to search within.
#' @return
#' A dataframe showing the functions, their filename arguments, adn the searched
#' file.
#'

#'
#' @export
#' @importFrom dplyr group_by summarise filter left_join %>% select data_frame select bind_rows
#' @importFrom stringr str_replace_all str_extract

detect_file <- function(file, function_list){

	list <- vector("list", length(function_list))
	for (i in seq_along(function_list)) {

		if (tools::file_ext(file) %in% c("Rmd", "rmd")) {
			temp <- parse(knitr::purl(file, output = tempfile(), documentation = 0)) %>%
				as.character()
		} else {
			temp <- file %>%
				readLines(warn = FALSE)
		}

		# Dummy value for zero-line files
		if (length(temp) == 0) temp <- NA

		temp <- temp %>%
			stringr::str_extract(paste0(function_list[i],"(.*)"))%>%
			stringr::str_extract("(\".*?\\.*?\")") %>%
			stringr::str_replace_all(pattern = '\\"', "")
		df <- data.frame(temp, stringsAsFactors = FALSE)
		names(df) <- "object"
		df$r_file <- str_replace_all(file, "//", "/")
		df$funct <- function_list[i]
		list[[i]] <- df
	}
	dplyr::bind_rows(list)
}

#' Detect Dependencies
#'
#' @description Detect dependencies within files. This is accomplished by
#' reading the filenames passed to the various input and output R funcitons,
#' then matching those filenames between the different R files. The assumption
#' is that all the files imported to an R script are pre-requisites for that
#' script, and the R script is the pre-requiste for all exported files.
#'
#' @param path
#' A string listing the filepath to search, defaults to the working directory.
#' @param import_functions
#' A character vector listing the import functions. This defaults to a
#' pre-populated list, but you can pass your own list if you only want to
#' detect certain dependencies.
#' @param export_functions
#' A character vector listing the export functions. This defaults to a
#' pre-populated list, but you can gpass your own list if you only want to
#' detect certain dependencies.
#' @param source_detect
#' Logical. Do you want to detect dependencies between R files when one
#' R script sources another one?
#'@param detect_cycle
#'Do you want easyMake to automatically warn you when a script depends on its own output?
#'If FALSE this function will run faster, but may lead to invalid Makefiles.
#'
#'
#' @examples
#' detect_dependencies(
#' 	system.file("test_project", package = "easyMake")
#' 	)
#'
#' @return
#' A dataframe showing the edge list of dependencies between files.
#' @importFrom dplyr %>%
#' @export
#'
detect_dependencies <- function(path = getwd(),
																import_functions = input,
																export_functions = output,
																source_detect = TRUE,
																detect_cycle  = TRUE){
	files <- list.files(path = path, recursive = TRUE, full.names = TRUE)
	R_files <- files[tools::file_ext(files) %in% c("R", "r", "Rmd", "rmd", "RMD")]

	export_list <- lapply(R_files, detect_file,
												function_list = export_functions)

	if (length(export_list) == 0){
		exports <-  dplyr::data_frame(file = NA,
													 pre_req = NA)
	}else {
		exports <- dplyr::bind_rows(export_list) %>%
			filter(!is.na(object)) %>%
			select(file = object, pre_req = r_file)
	}

	import_list <- lapply(R_files, detect_file,
												function_list = import_functions)
	if (length(import_list) == 0){
		imports <-  dplyr::data_frame(file = NA,
													 pre_req = NA)
	}else {
		imports <- dplyr::bind_rows(import_list) %>%
			filter(!is.na(object)) %>%
			select(file = r_file, pre_req = object)
	}

	dependencies <- dplyr::bind_rows(imports, exports)

	if (source_detect) {
		source_list <- lapply(R_files, detect_file,
													function_list = "source")

		if (length(source_list) == 0) {
			sourced <-  dplyr::data_frame(file = NA,
														 pre_req = NA)
		} else {
			sourced <- dplyr::bind_rows(source_list) %>%
				dplyr::filter(!is.na(object)) %>%
				dplyr::select(file = r_file, pre_req = object)
			dependencies <- dplyr::bind_rows(dependencies, sourced)
		}
	}
	dependencies$file <- gsub( paste0(path, "/"), "", x = dependencies$file)
	dependencies$pre_req <- gsub( paste0(path, "/"), "", x = dependencies$pre_req)
	dependencies <- dplyr::distinct(dependencies)


	if(detect_cycle) {
		df  <- dependencies
		df2 <- df
		names(df2)[2] <- "working"
		for(i in seq_len(nrow(df))) {
			df2 <- dplyr::left_join(df2, df, by = c("working" = "file"))
			names(df2)[ncol(df2)] <- "working"
			names(df2)[ncol(df2) -1 ] <- paste0("pre_req", i)
		}

		if(!all(is.na(df2$working))){
			cycle_files <- df2 %>%
				dplyr::filter(!is.na(working)) %>%
				.$file %>%
				unique() %>%
				paste(collapse = ", ")
			warning(paste("Circular dependencies detected in the following files:",
										cycle_files))
		}
	}
	dependencies
}

