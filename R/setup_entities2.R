# necessary for python module control
bsklenv <- basilisk::BasiliskEnvironment(envname="bsklenv",
    pkgname="ontoProc",
    packages=c("h5py==3.6.0"), 
    pip=c("owlready2==0.46", "bioregistry==0.11.18"))

#' preparing for a small number of entry points to owlready2 mediated by
#' basilisk, this setup function will ingest OWL, enumerate classes and their
#' names, and produce the 'parents' list, which can then be used with ontology_index
#' to produce a functional ontology representation
#' @param owlfn character(1) path to OWL file
#' @param cache_object logical(1) if TRUE, cache the `ontology_index` instance in BiocFileCache::BiocFileCache()
#' @examples
#' pa = get_ordo_owl_path()
#' orde = setup_entities2(pa)
#' orde
#' @export
setup_entities2 = function (owlfn, cache_object=TRUE) 
{
    thecall = match.call()
    ca = BiocFileCache::BiocFileCache()
    lk = BiocFileCache::bfcquery(ca, paste0(basename(owlfn), "_OIRDS"))
    if (nrow(lk)>0) {
      if (nrow(lk)>1) message("found multiple instances in cache, using latest; examine BiocFileCache if necessary")
      rec = lk[nrow(lk),,drop=FALSE]
      return(readRDS(rec$rpath[1]))
      }
    proc = basilisk::basiliskStart(bsklenv, testload="owlready2") # avoid package-specific import
    on.exit(basilisk::basiliskStop(proc))
    basilisk::basiliskRun(proc, function(owlfn, thecall, cache_object) {
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
     if (cache_object) {
#
# we always cache an owl file, but need to distinguish the serialized
# ontology_index, so use OIRDS in rname
#
       tf = tempfile(catag <- paste0(basename(owlfn),"_OIRDS"))
       saveRDS(theonto, file=tf)
       ca = BiocFileCache::BiocFileCache()
       BiocFileCache::bfcadd(ca, catag, tf)
       }
     theonto
     }, owlfn=owlfn, thecall=thecall, cache_object=cache_object)
}
