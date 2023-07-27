#'This function returns a list of available ontologies in the ontoProc package.
#'@importFrom AnnotationHub AnnotationHub
#'@importFrom AnnotationHub query
#'@importFrom S4Vectors mcols
#'@import ontologyIndex 
#' @examples
#' getAvailOntos()
#' @export
getAvailOntos = function() {
  ah = AnnotationHub::AnnotationHub()
  avail = AnnotationHub::query(ah, "ontoProc")
  df =  as.data.frame(S4Vectors::mcols(avail))[,c(1,6)]
  df
}

