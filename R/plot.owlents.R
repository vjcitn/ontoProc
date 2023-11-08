#' visualize ontology selection via onto_plot2, based on owlents
#' @rawNamespace S3method(plot,owlents)
#' @param x owlents instance
#' @param y character() vector of entries in x$clnames
#' @param \dots passed to onto_plot2
#' @examples
#' cl3k = c("CL:0000492", "CL:0001054", "CL:0000236", 
#'   "CL:0000625", "CL:0000576", 
#'   "CL:0000623", "CL:0000451", "CL:0000556")
#' cl3k = gsub(":", "_", cl3k)
#' clont_path = owl2cache(url="http://purl.obolibrary.org/obo/cl.owl")
#' clont = setup_entities(clont_path)
#' plot(clont,cl3k)
#' @export
plot.owlents = function(x, y, ...) {
  if (missing(y)) stop("must provide classes for plotting as 'y'")
  oky = intersect(y, x$clnames)
  if (length(oky)<2) stop("not enough selected names present in x$clnames")
  litc = x[oky]
  alitc = ancestors(litc)
  anames = ancestors_names(alitc)
  alluse = unique(unlist(anames))
  red = x[alluse]
  myo = ontologyIndex::ontology_index(parents=parents(red),
         name=labels(red))
  onto_plot2(myo, oky, ...)
}
