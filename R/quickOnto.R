#' Probe the cache for entries found by bfcquery using the given query.
#' @param query character(1), will be used by BiocFileCache::bfcquery
#' @param cache instance of class(BiocFileCache::BiocFileCache())
#' @param qans.only logical, defaulting to FALSE, if TRUE, just return
#' the bfcquery result.
#' @note  Take only the last entry found if there are multiple hits.  Use
#' `setup_entities2` to transform owl to ontologyIndex and cache the
#' result if this is absent for the owl identified by query.
#' @examples
#' aeo = quickOnto("aeo.owl")
#' str(aeo)
#' @export
quickOnto = function(query, cache=BiocFileCache::BiocFileCache(), qans.only=FALSE) {
  ans = BiocFileCache::bfcquery( cache, query )
  if (qans.only) return(ans)
  if (nrow(ans)>0) {
   nr = nrow(ans)
   if (nr>1) message("multiple hits, returning last")
   ans = ans[nr,]
   }
  if (nrow(ans)==0) {
    message("no hits for this query, returning NULL")
    return(NULL)
    }
  targ = ans$rpath
  if (length(grep("OIRDS", targ))>0) {
    requireNamespace("ontologyIndex")
    return(readRDS(targ))
    }
  ontoProc::setup_entities2( targ )
}
