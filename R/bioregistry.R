#' produce bioregistry_ols table
#' @return data.frame
#' @note This uses the `resources` method of the bioregistry module from pip
#' to isolate resources with a non-null `ols` component.
#' @examples
#' tab = bioregistry_ols_resources()
#' head(tab[,1:3])
#' @export
bioregistry_ols_resources = function() {
 proc = basilisk::basiliskStart(bsklenv, testload="owlready2") # avoid package-specific import
 on.exit(basilisk::basiliskStop(proc))
 basilisk::basiliskRun(proc, function(owlfn, thecall, cache_object) {
 br = reticulate::import("bioregistry")
#
# call the api
#
 xx = try(br$resources())
 if (inherits(xx, "try-error")) stop("bioregistry 'resources' method not answering; internet issue?")
#
# extract ols-associated elements
#
 allo = lapply(xx, function(x) x$ols)
 lens = vapply(allo,length,numeric(1))
 dr = which(lens==0)
 if (length(dr)>0) allo = allo[-dr]
 which(sapply(allo,length)==8)
#
# set up a template data frame with missing values for a family of fields
#
 start_df = function( fields = c("contact", "description", "download_owl", "homepage", "name",
     "prefix", "version", "version.iri", "download", "download_rdf") ) {
  n = NA_character_
  simp = rep(n, length(fields))
  base = data.frame(lapply(simp,force))
  names(base) = fields
  base
 }
 setup = function(x) {
  ans = start_df()
  ans[names(x)] = as.character(x)
  ans
 }
#
# create one data frame per resource, confident that there is a 
# common set of possibly NA fields
#
 nn = lapply(allo, setup)
#
# rbind them
#
 do.call(rbind, nn)
})
}
