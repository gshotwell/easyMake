#' Detect file
#' @description Reads a file, and detects any filename arguments passed
#' to any of the listed functions.
#'
#' @param this.file The file to be searched.
#' @param function_list The list of functions to search within.
#' @return A dataframe showing the functions, their filename
#'     arguments, adn the searched file.
#' 
#' @export
#' @importFrom dplyr group_by summarise filter left_join %>% select data_frame select bind_rows
#' @importFrom stringr str_replace_all str_extract
#' 
detect_file <- function(this.file, function_list) {

    function_list <- unique(function_list)

    ## list to hold the results (the exports/imports found in the file)
    ## object.list <- vector("list", length(function_list))

    ## read the file
    if (tools::file_ext(this.file) %in% c("Rmd", "rmd")) {
        text <- parse(knitr::purl(this.file, output = tempfile(),
                                  documentation = 0)) %>% 
            as.character() %>% paste0(collapse = "\n")
    } else {
        text <- this.file %>% readLines(warn = FALSE) %>%
            str_replace_all("^#+.*", "") %>% # delete lines starting with "#"
        paste0(collapse = "\n")
    }

    ## Dummy value for zero-line files
    if (nchar(text) > 0) {
        ## parse the file: look for the possible exports/imports

        ## for-loop version:
        ## for (i in seq_along(function_list)) {
        ##     temp <- text %>%
        ##         stringr::str_extract_all(paste0(function_list[i], "(.*)"),
        ##                                  simplify = TRUE) %>%
        ##         stringr::str_extract_all("(\".*?\\.*?\")") %>%
        ##         stringr::str_replace_all(pattern = "\\\"", "")
        ##     if(length(temp) > 0) {
        ##         df <- data.frame(temp, stringsAsFactors = FALSE)
        ##         names(df) <- "object"
        ##         df$r_file <- stringr::str_replace_all(this.file, "//", "/")
        ##         ## df$funct <- function_list[i]
        ##         object.list[[i]] <- df
        ##         print(i)
        ##         print(df)
        ##     }
        ## }

        ## code above can be easily vectorized:
        temp <- text %>%
            str_extract_all(paste0(function_list, "\\(.*\\)")) %>%
            unlist %>%
            ## str_extract_all("(\".*?\\.*?\")") %>%
            str_extract("(\".*?\\.*?\")") %>%
            str_replace_all(pattern = "\\\"", "")

        if(length(temp) > 0) {
            df <- data.frame(object = temp,
                             r_file = stringr::str_replace_all(this.file, "//", "/"),
                             stringsAsFactors = FALSE)

            ## return that data.frame
            df
        }
    }
}

#' Detect Dependencies
#'
#' @description Detect dependencies within files. This is accomplished by
#' reading the filenames passed to the various input and output R functions,
#' then matching those filenames between the different R files. The assumption
#' is that all the files imported to an R script are pre-requisites for that
#' script, and the R script is the pre-requiste for all exported files.
#'
#' @param path A string listing the filepath to search, defaults to
#'     the working directory.
#' @param import_functions A character vector listing the import
#'     functions. This defaults to a pre-populated list, but you can
#'     pass your own list if you only want to detect certain
#'     dependencies.
#' @param export_functions A character vector listing the export
#'     functions. This defaults to a pre-populated list, but you can
#'     pass your own list if you only want to detect certain
#'     dependencies.
#' @param source_detect Logical. Do you want to detect dependencies
#'     between R files when one R script sources another one?
#' @param detect_cycle Do you want easyMake to automatically warn you
#'     when a script depends on its own output?  If FALSE this
#'     function will run faster, but may lead to invalid Makefiles.
#'
#'
#' @examples
#' detect_dependencies(
#' \tsystem.file('test_project', package = 'easyMake')
#' \t)
#'
#' @return
#' A dataframe showing the edge list of dependencies between files.
#' @importFrom dplyr %>%
#' @export
#'
detect_dependencies <- function(path = getwd(),
                                import_functions = c("fread",
                                                     "import",
                                                     "load",
                                                     "read_delim",
                                                     "read_file",
                                                     "read_fw",
                                                     "read_lines",
                                                     "read_log",
                                                     "read_table",
                                                     "read.csv",
                                                     "read.dta",
                                                     "read.systat",
                                                     "read.table",
                                                     "read.xlsx",
                                                     "readRDS",
                                                     "sasxport.get",
                                                     "spss.get")
                               ,
                                export_functions = c("export",
                                                     "save",
                                                     "saveRDS",
                                                     "write_csv",
                                                     "write.csv",
                                                     "write.csv2",
                                                     "write.dcf",
                                                     "write.dta",
                                                     "write.foreign",
                                                     "write.ftable",
                                                     "write.table",
                                                     "write.table",
                                                     "write.tsv",
                                                     "write.xlsx")
                               ,
                                file_pattern = NULL,
                                source_detect = TRUE,
                                detect_cycle = TRUE) {

    ## get all files that are to be parsed
    files <- list.files(path = path, pattern = file_pattern,
                        recursive = TRUE, full.names = TRUE)
    R_files <- files[tools::file_ext(files) %in% c("R", "r", "Rmd", 
        "rmd")]

    ## 1.
    ## find exports from the files/scripts
    export_list <- lapply(R_files, detect_file,
                          function_list = export_functions)
    
    if (length(export_list) == 0) {
        exports <- dplyr::data_frame(file = NA, pre_req = NA)
    } else {
        exports <- dplyr::bind_rows(export_list) %>%
            dplyr::select(file = object, pre_req = r_file)
            ## filter(!is.na(object)) %>% 
    }

    ## 2.
    ## find imports from the files/scripts
    import_list <- lapply(R_files, detect_file,
                          function_list = import_functions)
    
    if (length(import_list) == 0) {
        imports <- dplyr::data_frame(file = NA, pre_req = NA)
    } else {
        imports <- dplyr::bind_rows(import_list) %>%
            dplyr::select(file = r_file, pre_req = object)
            ## filter(!is.na(object)) %>% 
    }
    
    dependencies <- dplyr::bind_rows(imports, exports)
    
    if (source_detect) {
        source_list <- lapply(R_files, detect_file,
                              function_list = "source")
        
        if(all(sapply(source_list, is.null))) {
            sourced <- dplyr::data_frame(file = NA, pre_req = NA)
        } else {
            sourced <- dplyr::bind_rows(source_list) %>%
                dplyr::select(file = r_file, pre_req = object)
                ## dplyr::filter(!is.na(object)) %>% 
            dependencies <- dplyr::bind_rows(dependencies, sourced)
        }
    }
    dependencies$file <- gsub(paste0(path, "/"), "",
                              x = dependencies$file)
    dependencies$pre_req <- gsub(paste0(path, "/"), "",
                                 x = dependencies$pre_req)
    dependencies <- dplyr::distinct(dependencies)
    
    if (detect_cycle) {
        df <- dependencies
        df2 <- df
        names(df2)[2] <- "working"
        for (i in seq_len(nrow(df))) {
            df2 <- dplyr::left_join(df2, df, by = c(working = "file"))
            names(df2)[ncol(df2)] <- "working"
            names(df2)[ncol(df2) - 1] <- paste0("pre_req", i)
        }
        
        if (!all(is.na(df2$working))) {
            cycle_files <- df2 %>% dplyr::filter(!is.na(working)) %>% 
                .$file %>% unique() %>% paste(collapse = ", ")
            warning(paste("Circular dependencies detected in the following files:", 
                cycle_files))
        }
    }
    
    dependencies
}
