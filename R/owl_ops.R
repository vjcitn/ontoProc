
#' return a generator with ontology classes 
#' @param owlfile reference to OWL file, can be URL, will be processed
#' by owlready2.get_ontology
#' @return generator with output of classes() on the loaded ontology
get_classes = function(owlfile) {
 o2 = reticulate::import("owlready2") # 'cached' by reticulate?
 o2$get_ontology(owlfile)$load()$classes()
}
 
#library(reticulate)

#o2 = reticulate::import("owlready2")

#' construct owlents instance from an owl file
#' @importFrom reticulate import
#' @param owlfn character(1) path to valid owl ontology
#' @return instance of owlents, which is a list with clnames (
#' a vector of term names in form `[namespace]_[tag]`), allents
#' (a list with python references to owlready2 entities, that
#' can be operated on using owlready2.EntityClass methods),
#' owlfn (filename), iri (IRI), call (record of call producing
#' the entity.)
#' @export 
setup_entities = function(owlfn) {
  thecall = match.call()
  o2 = reticulate::import("owlready2")
  ont = o2$get_ontology(owlfn)$load()
  cl = ont$classes()
  clnames = iterate(cl, function(x) x$name) # exhausts cl
  ents = ont$classes()  # new iterator
  allents = iterate(ents)
  ans = list(clnames=clnames, allents=allents, owlfn=owlfn, 
    iri=ont$base_iri, call=thecall)
  class(ans) = c("owlents", "list")
  ans
}

#' short printer
#' @export
print.owlents = function(x, ...) {
 cat(sprintf("owlents instance with %d classes.\n", length(x$clnames)))
}

#> args(get("[.data.frame"))
#function (x, i, j, drop = if (missing(i)) TRUE else length(cols) == 
#    1)

#' subset method
#' @param x owlents instance
#' @param i character or numeric vector
#' @param j not used
#' @param drop not used
#' @export
"[.owlents" <- function(x, i, j, drop = FALSE) {
  thecall = match.call()
  if (is.numeric(i)) inds = i
  else inds = match(i, x$clnames)
  if (any(is.na(inds))) {
    message("some index values not matched, ignoring")
    inds = inds[-which(is.na(inds))]
    }
  ans = x
  ans$clnames = x$clnames[inds]
  ans$allents = x$allents[inds]
  ans
}

#' subset method
#' @param oe owlents instance
ancestors = function(oe) {
  lapply(oe$allents, o2$EntityClass$ancestors)
}

#' subset method
#' @param oe owlents instance
subclasses = function(oe) {
  lapply(oe$allents, o2$EntityClass$subclasses)
}

#> ee["EFO_0008710"]$allents[[1]]
#efo.EFO_0008710
#> o2$EntityClass$ancestors(.Last.value)
#{efo.EFO_0008710, obo.BFO_0000015, obo.OBI_0000070, efo.EFO_0000001, efo.EFO_0001457, efo.EFO_0002772, efo.EFO_0004542, owl.Thing, efo.EFO_0002694}
#> ee["EFO_0008710"]$allents[[1]]
#efo.EFO_0008710
#> o2$EntityClass$subclasses(ee["EFO_0008710"]$allents[[1]])
#<generator object EntityClass.subclasses at 0x7f91ad731cb0>
#> iterate(.Last.value)
#> .Last.value
#
#
#system.time(tt <- setup_entities("ordo_orphanet.owl"))
#system.time(ee <- setup_entities("efo.owl"))
#
#
#
#system.time(ocl <- get_classes("ordo_orphanet.owl"))
# 
#system.time(ocl <- get_classes("ordo_orphanet.owl"))
#
#library(BiocParallel)
#register(MulticoreParam(3))
#
#get_clinfo <- function(x, BPPARAM=BiocParallel::bpparam()) {
#  o2 = reticulate::import("owlready2") # 'cached' by reticulate?
#  anc = bplapply(iterate(o2$EntityClass$ancestors(x)), function(x) x$name,
#      BPPARAM=BPPARAM)
#  subs = o2$EntityClass$subclasses(x)
#  list(parents=x$is_a, label=x$label, name=x$name, ancestors=anc,
#     children=subs)
#}
#
#system.time(ordcl <- iterate(ocl, get_clinfo))
#
##system.time(ecl <- get_classes("efo.owl"))
##
##system.time(efocl <- iterate(ecl, get_clinfo))
#
