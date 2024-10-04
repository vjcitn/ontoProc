# necessary for python module control
bsklenv <- basilisk::BasiliskEnvironment(envname="bsklenv",
    pkgname="ontoProc",
    packages=c("h5py==3.6.0"), pip="owlready2==0.46")

#' preparing for a small number of entry points to owlready2 mediated by
#' basilisk, this setup function will ingest OWL, enumerate classes and their
#' names, and produce the 'parents' list, which can then be used with ontology_index
#' to produce a functional ontology representation
#' @param owlfn character(1) path to OWL file
#' @examples
#' pa = get_ordo_owl_path()
#' orde = setup_entities2(pa)
#' orde
#' @export
setup_entities2 = function (owlfn) 
{
    thecall = match.call()
    proc = basilisk::basiliskStart(bsklenv, testload="owlready2") # avoid package-specific import
    on.exit(basilisk::basiliskStop(proc))
    basilisk::basiliskRun(proc, function(owlfn, thecall) {
     o2 = reticulate::import("owlready2")
     ont = o2$get_ontology(owlfn)$load()
     cl = ont$classes()
     clnames = iterate(cl, function(x) x$name)
     ents = ont$classes()
     allents = iterate(ents)
     ans = list(clnames = clnames, allents = allents, owlfn = owlfn, 
         iri = ont$base_iri, call = thecall)
     thelabs = vapply(allents, function(x) {
        z = try(x$label[0], silent = TRUE)
        if (inherits(z, "try-error")) 
            return(NA_character_)
        z
        }, character(1))
     theonto = ontologyIndex::ontology_index(parents=ontoProc::parents(ans),
        name=thelabs)
#    names(ll) = sapply(object$allents, function(x) x$name)
#    unlist(ll)
#}
#
#     ans = list(clnames = clnames, allents = allents, owlfn = owlfn, 
#         iri = ont$base_iri, call = thecall)
#     class(ans) = c("owlents", "list")
#     pp = parents(ans)
#     list(oe = ans, parents = pp)
     theonto
     }, owlfn=owlfn, thecall=thecall)
}
