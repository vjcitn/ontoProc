#' check that a URL can get a 200 for a HEAD request
#' @importFrom httr status_code HEAD
#' @param url character(1)
#' @return logical(1)
url_ok = function(url) {
  httr::status_code(httr::HEAD(url)) == 200
}

#' cache an owl file accessible via URL
#' @param cache BiocFileCache instance or equivalent
#' @param url character(1)
#' @note This function will check for presence of url in cache using bfcquery; 
#' if a hit is found, returns the rpath associated with the last
#' matching record.  etags can be available for use with bfcneedsupdate.
#' @examples
#' ca = BiocFileCache::BiocFileCache()
#' hppa = owl2cache(ca, 
#'    url="http://purl.obolibrary.org/obo/hp/releases/2023-10-09/hp-base.owl")
#' setup_entities(hppa)
#' @export
owl2cache = function(cache = BiocFileCache::BiocFileCache(), url) {
   inf = BiocFileCache::bfcquery(cache, url)
   if (nrow(inf)>0) {
     res = inf[nrow(inf),]
     message(sprintf("resource %s already in cache from %s\n", res$rid, url))
     return(res$rpath)
     } 
   if (!url_ok(url)) stop(sprintf("HEAD for %s does not return status code 200\n", url))
   BiocFileCache::bfcadd(cache, rname = basename(url), fpath=url, rtype="web")
}
