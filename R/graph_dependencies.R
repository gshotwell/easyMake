#' graph_dependencies
#'
#' @param files A character vector of filenames within the project,
#' for instance one produced by list.files(recursive = TRUE)
#' @param dependencies A dataframe containing the edge list of dependencies
#' between files. This is the same file which is passed to easy_make(). The
#' dataframe should have two fields 'file' and 'pre_req' showing the file
#' and its immediate pre-requisite respectively. If a file has multiple
#' pre-requisites, list each dependency as a separate line in the dataframe.
#'
#' @return A DiagrammeR plot showing the dependency graph.
#' @export
#'
#' @examples
#' test_dir <- system.file('test_project', package = 'easyMake')
#' graph_dependencies(
#'\tdetect_dependencies(test_dir),
#'\tfiles = list.files(test_dir))

graph_dependencies <- function(dependencies,
                               files = list.files(recursive = TRUE)) {
    
    file_df <- data.frame(files, stringsAsFactors = FALSE)
    file_df$type <- tools::file_ext(file_df$files)
    file_df$shape <- ifelse(file_df$type %in% c("R", "Rmd"), "circle", 
        "square")
    
    nodes <- DiagrammeR::create_nodes(nodes = file_df$files,
                                      shape = file_df$shape)
    
    edges <- DiagrammeR::create_edges(from = dependencies$pre_req, 
                                      to = dependencies$file,
                                      relationship = "leading to")
    
    graph <- DiagrammeR::create_graph(nodes_df = nodes,
                                      edges_df = edges, 
                                      graph_attrs = "layout = circo")
    
    DiagrammeR::render_graph(graph, width = 2000, height = 2000)
}


