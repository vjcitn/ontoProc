#' @include ontoDiff.R
#' @title Display Version Differences
#' @import Rgraphviz
#' @importFrom grid grid.text gpar
#' @description
#' Highlights in green the terms that are present in the new ontology but not the old one 
#' @param newonto the newest version of the ontology
#' @param oldonto the old version of the ontology
#' @param terms2use terms of interest
#' @param cex numeric(1) defaults to .8, supplied to Rgraphviz::graph.par
#' @param ... passed to onto_plot of ontologyPlot
#' @return onto_plot2 style plot with version differences highlighted
#' @examples
#' cl = getOnto("diseaseOnto")
#' cl2 = getOnto(ontoname = "diseaseOnto", year_added = "2021")
#' cl3k = c("DOID:0040064","DOID:0040076","DOID:0081127","DOID:0081126","DOID:0081131","DOID:0060034")
#' ontoDiff(cl,cl2,cl3k)
#' @export
#Credit to ontoPlot for the use of some of it's functions
ontoDiff <- function (newonto,oldonto,terms2use, cex = 0.8, ...)
  
{
  
  #Nodes in black exist in both versions of the ontology, while green nodes exist only in the new ontology.
  
  termsn = ontologyPlot:::remove_uninformative_terms(newonto, terms2use)
  termso = ontologyPlot:::remove_uninformative_terms(oldonto, terms2use)
  
  addedterms <- setdiff(termsn, termso)
  pl = ontologyPlot::onto_plot(newonto, terms2use, ...)
  gnel = make_graphNEL_from_ontology_plot(pl)
  gnel = improveNodes(gnel, newonto)
  graph.par(list(nodes = list(shape = "plaintext", cex = cex)))
  node_colors <- rep("black", length(nodes(gnel)))
  names(node_colors) <- nodes(gnel)
  for (term in addedterms) {
    matching_nodes <- names(node_colors)[grepl(term, names(node_colors))]
    node_colors[matching_nodes] <- "green"
  }
  nodeRenderInfo(gnel)$textCol <- node_colors
  gnel <- layoutGraph(gnel)
  renderGraph(gnel)
  
  
  grid.text("New Terms", x = 0.07, y = 0.985, gp = gpar(fontsize = 8, col = "green"))
  grid.text("Common Terms", x = 0.09, y = 0.96, gp = gpar(fontsize = 8, col = "black"))
  invisible(gnel)
}