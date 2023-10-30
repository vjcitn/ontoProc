
#' return a generator with ontology classes 
#' @param owlfile reference to OWL file, can be URL, will be processed
#' by owlready2.get_ontology
#' @return generator with output of classes() on the loaded ontology
get_classes = function(owlfile) {
 o2 = reticulate::import("owlready2") # 'cached' by reticulate?
 o2$get_ontology(owlfile)$load()$classes()
}
 
#' construct owlents instance from an owl file
#' @importFrom reticulate import iterate
#' @param owlfn character(1) path to valid owl ontology
#' @return instance of owlents, which is a list with clnames (
#' a vector of term names in form `[namespace]_[tag]`), allents
#' (a list with python references to owlready2 entities, that
#' can be operated on using owlready2.EntityClass methods),
#' owlfn (filename), iri (IRI), call (record of call producing
#' the entity.)
#' @examples
#' pa = get_ordo_owl_path()
#' orde = setup_entities(pa)
#' orde
#' ancestors(orde[1000:1001])
#' labels(orde[1000:1001])
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

#' retrieve ancestor 'sets'
#' @param oe owlents instance
#' @export
ancestors = function(oe) {
  o2 = reticulate::import("owlready2")
  lapply(oe$allents, o2$EntityClass$ancestors)
}

#' retrieve is_a 
#' @param oe owlents instance
#' @return list of vectors of tags of parents
#' @export
parents = function(oe) {
    pts = lapply(oe$allents, function(x) x$is_a)
# must remove restrictions, only want actual entities
    isent = function(x) inherits(x, "owlready2.entity.EntityClass")
    ok = lapply(pts, sapply, isent)
    for (i in seq_len(length(ok))) pts[[i]] = pts[[i]][which(ok[[i]])]
    ans = lapply(pts, sapply, function(x) x$name)
    names(ans) = oe$clnames
    ans
}

#  pts = lapply(oe$allents, function(x) x$is_a)
## must remove restrictions, only want actual entities
#  ok = sapply(pts, inherits, "owlready2.entity.EntityClass")
#  pts = pts[which(ok)]
#  lapply(pts, function(x) names(labels(x)))
#}
#
#' retrieve subclass entities
#' @param oe owlents instance
#' @export
subclasses = function(oe) {
  o2 = reticulate::import("owlready2")
  lapply(oe$allents, o2$EntityClass$subclasses)
}

#' retrieve labels with names
#' @param object owlents instance
#' @param \dots not used
#' @note When multiple labels are present, only first is silently returned.
#' To get ontology tags, use `names(labels(...))`.
#' @export
labels.owlents = function(object, ...) {
  o2 = reticulate::import("owlready2")
  ll = sapply(object$allents, function(x) x$label[1])
  names(ll) = sapply(object$allents, function(x) x$name[1])
  unlist(ll)
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
