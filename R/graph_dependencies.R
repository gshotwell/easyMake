#' graph_dependencies
#'
#' @param files A character vector of filenames within the project,
#' for instance one produced by list.files(recursive = TRUE)
#' @param dependencies A dataframe containing the edge list of dependencies
#' between files. This is the same file which is passed to easy_make(). The
#' dataframe should have two fields "file" and "pre_req" showing the file
#' and its immediate pre-requisite respectively. If a file has multiple
#' pre-requisites, list each dependency as a separate line in the dataframe.
#'
#' @return A DiagrammeR plot showing the dependency graph.
#' @export
#'
#' @examples
#' test_dir <- system.file("test_project", package = "easyMake")
#' graph_dependencies(
#'	detect_dependencies(test_dir),
#'	files = list.files(test_dir))
graph_dependencies <- function(dependencies = detect_dependencies(),
															 files = list.files(recursive = TRUE)){

	dependencies <- dependencies[, c("pre_req", "file")]

	g <- igraph::graph_from_data_frame(dependencies)

	ggraph::ggraph(g, 'dendrogram')+
		ggraph::geom_edge_link() +
		ggraph::geom_node_point() +
		ggraph::geom_node_text(ggplot2::aes(label = igraph::V(g)$name),
									 repel = TRUE) +
		ggplot2::theme_void()
}
