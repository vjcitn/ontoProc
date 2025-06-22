#' interrogate the cache for owl files and serialized ontologyIndex instances
#' @param ca defaults to BiocFileCache::BiocFileCache()
#' @param token character(1) defaults to "owl|OIRDS"
#' @return NULL if nothing relevant is found in cache, otherwise a data.frame
#' with all cached information
#' @export
available_ontos = function(ca = BiocFileCache::BiocFileCache(),
   token = "owl|OIRDS") {
    dat = BiocFileCache::bfcquery(ca, token)
    if (nrow(dat)==0) return(NULL)
    as.data.frame(dat)
    }

#' browse available ontologies with datatable
#' @param ca defaults to BiocFileCache::BiocFileCache()
#' @param token character(1) defaults to "owl|OIRDS"
#' @export
browse_ontos = function(ca = BiocFileCache::BiocFileCache(),
  token = "owl|OIRDS") {
  dat = available_ontos(ca=ca, token=token)
  if (is.null(dat)) stop("no ontology data to browse")
  DT::datatable(dat)
}
  
