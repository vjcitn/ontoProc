# October 2022 information in AnnotationHub for Bioc 3.16
# opd =query(ah, "ontoProcData")
# meta = mcols(opd)
# meta |> as.data.frame() |> filter(grepl("2022", rdatadateadded)) |> select(title, description)
#                          title              description
#AH107268        caro_2022.02.18 common anatomy reference
#AH107269   cellLineOnto_2.1.178               cell lines

# April 2023 information in AnnotationHub for Bioc 3.17
# opd =query(ah, "ontoProcData")
# meta = mcols(opd)
#meta |> as.data.frame() |> filter(grepl("2023", rdatadateadded)) |> select(title, description)
#                          title              description
#AH111554    cellOnto_2023.02.15                    cells
#AH111555     cellosaurusOnto_44               cell lines
#AH111556         chebi_full_218 large chemicals ontology
#AH111557         chebi_lite_218  short chemical ontology
#AH111558 diseaseOnto_2023.01.30                 diseases
#AH111559         efoOnto_3.51.0      experimental factor
#AH111560      goOnto_2023.01.01            Gene ontology
#AH111561    hcaoOnto_2022.12.16         human cell atlas
#AH111562       mondo_2022.12.01                  disease
#AH111563    patoOnto_2023.02.17      phenotype and trait
#AH111564              PROnto_68                  protein
#AH111565      uberon_2023.02.14                  anatomy


#' give a vector of valid 'names' of ontoProc ontologies
#' @examples
#' head(valid_ontonames())
#' @export
valid_ontonames = function() {
c("caro", "cellLineOnto", "cellOnto", "cellosaurusOnto", "chebi_full", 
"chebi_lite", "diseaseOnto", "efoOnto", "goOnto", "hcaoOnto", "mondo", 
"patoOnto", "PROnto", "uberon")
}

#' get the ontology based on a short tag and year
#' @param ontoname character(1) must be an element in `valid_ontonames()`
#' @param year_added character(1) refers to `rdatadateadded` in AnnotationHub metadata
#' @note This queries AnnotationHub for "ontoProcData" and then filters to find
#' the AnnotationHub accession number and retrieves the ontologyIndex serialization
#' of the associated OBO representation of the ontology.
#' @examples
#' co = getOnto()
#' tail(co$name[1000:1500])
#' @export
getOnto = function( ontoname="cellOnto", year_added = "2022" ) {
 stopifnot(ontoname %in% valid_ontonames())
 ah = AnnotationHub::AnnotationHub()
 opd = AnnotationHub::query(ah, "ontoProcData")
 meta = mcols(opd)
 tmp = meta |> as.data.frame() |> dplyr::filter(grepl(year_added, rdatadateadded)) |> dplyr::select(title, description)
 if (year_added == "2023") {
   ontoname = paste0(ontoname, "_")
   stopifnot(length(grep(ontoname, tmp$title))==1)
 }
 else if (year_added == "2022") {
    ontoname = paste0(ontoname, "_")
    stopifnot(length(grep(ontoname, tmp$title))==1)
    }
 else if (year_added == "2021") {
    stopifnot(length(grep(paste0("^", ontoname, "$"), tmp$title))==1)
    }
 tmp = tmp |> filter(grepl(ontoname, title))
 tag = rownames(tmp)
 stopifnot(length(tag)==1)
 ah[[tag]]
}


#This is the October 2021 result of querying AnnotationHub in Bioc 3.14 for ontoProcData
#AH97934             caro           NA    <NA>         NA   <NA>
#AH97935     cellLineOnto           NA    <NA>         NA   <NA>
#AH97936         cellOnto           NA    <NA>         NA   <NA>
#AH97937  cellosaurusOnto           NA    <NA>         NA   <NA>
#AH97938       chebi_full           NA    <NA>         NA   <NA>
#AH97939       chebi_lite           NA    <NA>         NA   <NA>
#AH97940      diseaseOnto           NA    <NA>         NA   <NA>
#AH97941          efoOnto           NA    <NA>         NA   <NA>
#AH97942           goOnto           NA    <NA>         NA   <NA>
#AH97943          hcaOnto           NA    <NA>         NA   <NA>
#AH97944         oncotree           NA    <NA>         NA   <NA>
#AH97945         patoOnto           NA    <NA>         NA   <NA>
#AH97946           Pronto           NA    <NA>         NA   <NA>
#AH97947           uberon           NA    <NA>         NA   <NA>
#AH97948 mondo_2021_04_07           NA    <NA>         NA   <NA>
#
# we will need metadata added to deal with versioning in future
#

ont_tags = c(caro = "AH97934", cellLineOnto = "AH97935",
             cellOnto="AH111554",
             cellosaurusOnto="AH111555",
             chebi_full="AH111556",
             chebi_lite="AH111557",
             diseaseOnto="AH111558",
             efoOnto="AH111559",
             goOnto="AH111560",
             hcaOnto="AH111561",
             mondo_2022_12_01="AH111562",
             patoOnto="AH111563",
             PROnto="AH111564",
             uberon="AH111565", 
             oncotree = "AH97944")

get_tag = function(stub) {
 stopifnot(length(stub)==1)
 ans = try(ont_tags[stub])
 if (!inherits(ans, "try-error")) return(as.character(ans))
}

get_onto = function(stub) {
 tag = get_tag(stub)
 hu = AnnotationHub()
 hu[[tag]]
}


# utility for caching recent obo for cell ontology
add_cache_cl_simple = function(cache = BiocFileCache::BiocFileCache(),
     target = "https://raw.githubusercontent.com/obophenotype/cell-ontology/master/cl-simple.obo") {
 BiocFileCache::bfcadd(cache, target)
}

#' load ontologies that may include non-ascii strings and therefore cannot be in data folder
#' @import BiocFileCache
#' @importFrom AnnotationHub AnnotationHub
#' @param useNew logical(1) only for getCellOnto if TRUE return ontology_index instance of cell ontology 2.1 of May 21 2020, defaults to TRUE
#' @param use0718 logical(1) only for getCellOnto if TRUE cell ontology of July 2018
#' @param newest logical(1) if TRUE will use BiocFileCache to retrieve/use latest cl-simple.obo; overrides
#' @param cache instance of BiocFileCache
#' @note You may want to try `bfcupdate` on the BiocFileCache element.
#' useNew
#' @examples
#' co = getCellOnto(useNew=TRUE)
#' co
#' clo = getCellLineOnto()
#' length(clo$id)
#' che = getChebiLite()
#' length(che$id)
#' efo = getEFOOnto()
#' length(efo$id)
#' @return instance of ontology_index (S3) from ontologyIndex
#' @note Provenance information is kept in the form
#' of excerpts of top records in `dir(system.file("obo", package="ontoProc"), full=TRUE)`.  This
#' function is deprecated in Bioc 3.16.  Use getOnto("cellOnto") to get the latest version
#' provided by AnnotationHub.
#' @export
getCellOnto = function(useNew=TRUE, newest=FALSE, cache=BiocFileCache::BiocFileCache(),
    use0718=FALSE)  {
    .Deprecated("getOnto", msg="getCellOnto is deprecated: getOnto('cellOnto') should be used for versioned access")
    if (newest) {
     qu = BiocFileCache::bfcquery(cache, "cl-simple")
     if (nrow(qu) == 0) {
       chk = try(add_cache_cl_simple(cache=cache))
       if (inherits(chk, "try-error")) stop("could not add cl-simple.obo to cache")
       qu = BiocFileCache::bfcquery(cache, "cl-simple")
       }
     if (nrow(qu)>1) warning("more than one row mentions cl-simple, using first")
     return(ontologyIndex::get_OBO(qu$fpath[1], extract_tags="everything"))
    }
    sfstr = "ontoRda/cellOnto.rda"
    if (use0718) sfstr = "ontoRda/co_0718.rda"
    get(load(system.file(
      sfstr, package="ontoProc")))
    }


dmsg = function(x) .Deprecated(x, msg = sprintf("%s is deprecated: getOnto('%s') should be used for versioned access.", x, x)) 


#' @rdname getCellOnto
#' @aliases getCellLineOnto
#' @export
getCellLineOnto = function() {
 dmsg("cellLineOnto")
 get_onto("cellLineOnto")
}

#' @rdname getCellOnto
#' @aliases getEFOOnto
#' @export
getEFOOnto = function() {
 dmsg("efoOnto")
 get_onto("efoOnto")
}

#' @rdname getCellOnto
#' @aliases getChebiLite
#' @export
getChebiLite = function() {
 get_onto("chebi_lite")
}

#' @rdname getCellOnto
#' @aliases getCellosaurusOnto
#' @export
getCellosaurusOnto = function() {
 get_onto("cellosaurusOnto")
}

#' @rdname getCellOnto
#' @aliases getUBERON_NE
#' @export
getUBERON_NE = function() {
 get_onto("uberon")
}

#' @rdname getCellOnto
#' @aliases getChebiOnto
#' @note getChebiOnto loads ontoRda/chebi_full.rda
#' @export
getChebiOnto = function() {
 get_onto("chebi_full")
}


#' @rdname getCellOnto
#' @aliases getOncotreeOnto
#' @note getOncotreeOnto loads ontoRda/oncotree.rda
#' @return instance of ontology_index (S3) from ontologyIndex
#' @export
getOncotreeOnto = function() {
 get_onto("oncotree")
}

#' @rdname getCellOnto
#' @aliases getDiseaseOnto
#' @export
getDiseaseOnto = function() {
 get_onto("diseaseOnto")
}


#' @rdname getCellOnto
#' @aliases getGeneOnto
#' @note getDiseaseOnto loads ontoRda/diseaseOnto.rda
#' @export
getGeneOnto = function() {
 get_onto("goOnto")
}

#' @rdname getCellOnto
#' @aliases getHCAOnto
#' @note getHCAOnto loads ontoRda/hcaOnto.rda produced from hcao.owl at https://github.com/HumanCellAtlas/ontology/releases/tag/1.0.6 2/11/2019,
#' python pronto was used to convert OWL to OBO.
#' @export
getHCAOnto = function() {
 get_onto("hcaOnto")
}

#' @rdname getCellOnto
#' @aliases getPROnto
#' @note getPROnto loads ontoRda/PRonto.rda, produced from http://purl.obolibrary.org/obo/pr.obo 'reasoned' ontology from OBO foundry, 02-08-2019.
#' In contrast to other ontologies, this is imported via get_OBO with
#' `extract_tags='minimal'`.
#' @export
getPROnto = function() {
 get_onto("PROonto")
}


#' @rdname getCellOnto
#' @aliases getPATOnto
#' @note getPATOnto loads ontoRda/patoOnto.rda, produced from https://raw.githubusercontent.com/pato-ontology/pato/master/pato.obo from OBO foundry, 02-08-2019.
#' @export
getPATOnto = function() {
 get_onto("patoOnto")
}

#' @rdname getCellOnto
#' @aliases getMondoOnto
#' @export
getMondoOnto = function() {
 get_onto("mondo_2022_12_01")
}

#' @rdname getCellOnto
#' @aliases getSIOOnto
#' @export
getSIOOnto = function() {
 get(load(system.file(
      "ontoRda/sio_rel_2021_04_26.rda", package="ontoProc")))
}
