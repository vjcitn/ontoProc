
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
#' o2 = try(reticulate::import("owlready2"), silent=TRUE)
#' if (!inherits(o2, "try-error")) {
#'  orde = setup_entities(pa)
#'  orde
#'  ancestors(orde[1000:1001])
#'  labels(orde[1000:1001])
#' }
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
#' @param x owlents instance
#' @param \dots not used
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
#' @return a list of sets
#' @examples
#' pa = get_ordo_owl_path()
#' o2 = try(reticulate::import("owlready2"), silent=TRUE)
#' if (!inherits(o2, "try-error")) {
#'  orde = setup_entities(pa)
#'  orde
#'  ancestors(orde[1:5])
#'  labels(orde[1:5])
#' }
#' @export
ancestors = function(oe) {
  o2 = try(reticulate::import("owlready2"))
  if (inherits(o2, "try-error")) stop("ensure that reticulate can find owlready2")
  ans = lapply(oe$allents, o2$EntityClass$ancestors)
  names(ans) = oe$clnames
  ans
}

#' obtain list of names of a set of ancestors
#' @param anclist output of `ancestors`
#' @note non-entities are removed and names are extracted
#' @return list of vectors of character()
#' @examples
#' pa = get_ordo_owl_path()
#' o2 = try(reticulate::import("owlready2"), silent=TRUE)
#' if (!inherits(o2, "try-error")) {
#'  orde = setup_entities(pa)
#'  al = ancestors(orde[1001:1002])
#'  ancestors_names(al)
#' }
#' @export
ancestors_names = function(anclist) {
  lapply(anclist, .ancestor_element_names)
}

#' obtain list of names of a set of subclasses/children
#' @param sclist output of `subclasses`
#' @note non-entities are removed and names are extracted
#' @return list of vectors of character()
#' @examples
#' pa = get_ordo_owl_path()
#' o2 = try(reticulate::import("owlready2"), silent=TRUE)
#' if (!inherits(o2, "try-error")) {
#'  orde = setup_entities(pa)
#'  al = subclasses(orde[100:120])
#'  children_names(al)
#' }
#' @export
children_names = function(sclist) {
  lapply(sclist, .subclass_element_names)
}

.ancestor_element_names = function(anclist_el) {
  al = reticulate::iterate(anclist_el) # python set becomes R list
  alok = sapply(al, function(x) inherits(x, "owlready2.entity.EntityClass"))
  if (any(!alok)) al = al[which(alok)]
  sapply(al, function(x) x$name)
}

.subclass_element_names = function(sclist_el) {
  scl = reticulate::iterate(sclist_el) # python set becomes R list
  sclok = sapply(scl, function(x) inherits(x, "owlready2.entity.EntityClass"))
  if (any(!sclok)) scl = scl[which(sclok)]
  sapply(scl, function(x) x$name)
}
  

#' retrieve is_a 
#' @param oe owlents instance
#' @return list of vectors of tags of parents
#' @examples
#' pa = get_ordo_owl_path()
#' o2 = try(reticulate::import("owlready2"), silent=TRUE)
#' if (!inherits(o2, "try-error")) {
#'  orde = setup_entities(pa)
#'  orde
#'  parents(orde[1000:1001])
#'  labels(orde[1000:1001])
#' }
#' @export
parents = function(oe) {
    pts = lapply(oe$allents, function(x) x$is_a)
# must remove restrictions, only want actual entities
    isent = function(x) inherits(x, "owlready2.entity.EntityClass")
    pts = lapply(pts, function(x)
		 reticulate::import_builtins()$list(x))
    ok = lapply(pts, sapply, isent)
    for (i in seq_len(length(ok))) pts[[i]] = pts[[i]][which(ok[[i]])]
    ans = lapply(pts, sapply, function(x) x$name)
    names(ans) = oe$clnames
    ans
}


#' retrieve subclass entities
#' @param oe owlents instance
#' @examples
#' pa = get_ordo_owl_path()
#' o2 = try(reticulate::import("owlready2"), silent=TRUE)
#' if (!inherits(o2, "try-error")) {
#'  orde = setup_entities(pa)
#'  orde
#'  sc <- subclasses(orde[1:5])
#'  labels(orde[3])
#'  o3 = reticulate::iterate(sc[[3]])
#'  print(length(o3))
#'  o3[[2]]
#'  labels(orde["Orphanet_100011"])
#' }
#' @export
subclasses = function(oe) {
  o2 = reticulate::import("owlready2")
  ans = lapply(oe$allents, o2$EntityClass$subclasses)
  names(ans) = oe$clnames
  ans
}

#' retrieve labels with names
#' @param object owlents instance
#' @param \dots not used
#' @note When multiple labels are present, only first is silently returned.  Note that reticulate 1.35.0 made a change that
#' appears to imply that `[0]` can be used to retrieve the desired
#' components.
#' To get ontology tags, use `names(labels(...))`.  Note: This function was revised Jul 12 2024
#' to allow terms that lack labels (like CHEBI references in cl.owl) to be processed, returning NA.
#' The previous functionality which failed is available, not exported, as labelsOLD.owlents.
#' @examples
#' clont_path = owl2cache(url="http://purl.obolibrary.org/obo/cl.owl")
#' o2 = try(reticulate::import("owlready2"), silent=TRUE)
#' if (!inherits(o2, "try-error")) {
#'  clont = setup_entities(clont_path)
#'  labels(clont[1:5])
#'  labels(clont[51:55])
#' }
#' @export
labels.owlents = function (object, ...) 
{
    ll = sapply(object$allents, function(x) {
      z = try(x$label[0], silent=TRUE)
      if (inherits(z, "try-error")) return(NA)
      z
      })
    names(ll) = sapply(object$allents, function(x) x$name)
    unlist(ll)
}

labelsOLD.owlents = function(object, ...) {
  #o2 = reticulate::import("owlready2")
  ll = sapply(object$allents, function(x) x$label[0])
  names(ll) = sapply(object$allents, function(x) x$name)
  unlist(ll)
}
  

#' use owlready2 ontology search facility on term labels
#' @param ontopath character(1) path to owl file
#' @param regexp character(1) simple regular expression
#' @param case_sensitive logical(1) should case be respected in search?
#' @return A named list: term labels are elements, tags are names of elements.
#' Will return NULL if nothing is found.
#' @examples
#' pa = get_ordo_owl_path()
#' ol = search_labels(pa, "*Immunog*")
#' orde = setup_entities2(pa)
#' onto_plot2(orde, names(ol))
#' @export
search_labels = function (ontopath, regexp, case_sensitive=TRUE) 
{
    stopifnot(inherits(ontopath, "character"))
    thecall = match.call()
    proc = basilisk::basiliskStart(bsklenv, testload="owlready2") # avoid package-specific import
    on.exit(basilisk::basiliskStop(proc))
    basilisk::basiliskRun(proc, function(owlfn, thecall) {
     o2 = reticulate::import("owlready2")
     ont = o2$get_ontology(owlfn)$load()
     ans = ont$search(`_case_sensitive`=case_sensitive, label = regexp)
     lans = reticulate::iterate(ans, force)
     if (length(lans)<=0) return(NULL)
     allv = lapply(lans, function(x) x$label[0])
     alln = lapply(lans, function(x) x$name)
     names(allv) = unlist(alln)
     allv
     }, ontopath, thecall)
}

