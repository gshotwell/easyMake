
detect_file <- function(text, function_list){
	list <- vector("list", length(function_list))
	for (i in seq_along(function_list)) {
		temp <- text %>%
			parse() %>%
			as.character() %>%
			str_extract(paste0(function_list[i],"(.*)"))%>%
			str_extract("(\".*?\\.*?\")") %>%
			str_replace_all(pattern = '\\"', "")
		df <- data.frame(temp, stringsAsFactors = FALSE)
		names(df) <- "object"
		df$r_file <- str_replace_all(text, "//", "/")
		df$funct <- function_list[i]
		list[[i]] <- df
	}
	dplyr::bind_rows(list)
}



detect_dependencies <- function(path, import_functions, export_functions){
	files <- list.files(path = path, full.names = TRUE)
	R_files <- files[tools::file_ext(files) == "R"]

	import_functions <- c("save", "import")
	export_functions <- c("load", "export")

	export_list <- lapply(R_files, detect_file,
												function_list = export_functions)
	exports <- dplyr::bind_rows(export_list) %>%
		filter(!is.na(object)) %>%
		select(file = object, pre_req = r_file)

	import_list <- lapply(R_files, detect_file,
												function_list = import_functions)
	imports <- dplyr::bind_rows(import_list) %>%
		filter(!is.na(object)) %>%
		select(file = r_file, pre_req = object)

	dependencies <- bind_rows(imports, exports)
	dependencies
}

