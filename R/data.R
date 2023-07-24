#' allGOterms: data.frame with ids and terms
#' @importFrom utils data read.delim
#' @docType data
#' @format data.frame instance
#' @source This is a snapshot of all the terms available from 
#' GO.db (3.4.2), August 2017, using keys(GO.db, keytype="TERM").
#' @usage data(allGOterms)
#' @examples
#' data(allGOterms)
#' head(allGOterms)
"allGOterms"
#' stopWords: vector of stop words from xpo6.com
#' @docType data
#' @format character vector
#' @source \url{http://xpo6.com/list-of-english-stop-words/}
#' @note "Stop words" are english words that are assumed to contribute limited 
#' semantic value in the analysis of free text. 
#' @usage data(stopWords)
#' @examples
#' data(stopWords)
#' head(stopWords)
"stopWords"
#' minicorpus: a vector of annotation strings found in 'study title' of SRA metadata.
#' @docType data
#' @format character vector
#' @source NCBI SRA
#' @note arbitrarily chosen from titles of RNA-seq studies for taxon 9606
#' @usage data(minicorpus)
#' @examples
#' data(minicorpus)
#' head(minicorpus)
"minicorpus"
#' humrna: a data.frame of SRA metadata related to RNA-seq in humans
#' @docType data
#' @format data.frame
#' @source NCBI SRA
#' @note arbitrarily chosen from RNA-seq studies for taxon 9606
#' @usage data(humrna)
#' @examples
#' data(humrna)
#' names(humrna)
#' head(humrna[,1:5])
"humrna"
#' PROSYM: HGNC symbol synonyms for PR (protein ontology) entries identified in Cell Ontology
#' @docType data
#' @format data.frame instance
#' @source OBO Foundry
#' @usage data(PROSYM)
#' @note This is a snapshot of the synonyms
#' component of an extract_tags='everything' import of PR.
#' The 'EXACT.*PRO-short.*:DNx' pattern is used to retrieve
#' HGNC symbols.
#' See ?getPROnto for more provenance information.
#' @examples
#' data(PROSYM)
#' head(PROSYM)
"PROSYM"

#' packDesc2019: overview of ontoProc resources
#' @format data.frame instance
#' @note Brief survey of functions available to load serialized
#' ontology_index instances imported from OBO.
#' @usage data(packDesc2019)
#' @examples
#' data(packDesc2019)
#' head(packDesc2019)
"packDesc2019"

#' packDesc2022: overview of ontoProc resources
#' @format data.frame instance
#' @note Brief survey of functions available to load serialized
#' ontology_index instances imported from OBO.  Focus is on versions added in 2022.
#' @usage data(packDesc2022)
#' @examples
#' data(packDesc2022)
#' head(packDesc2022)
"packDesc2022"

#' packDesc2021: overview of ontoProc resources
#' @format data.frame instance
#' @note Brief survey of functions available to load serialized
#' ontology_index instances imported from OBO.  Focus is on versions added in 2021.
#' @usage data(packDesc2021)
#' @examples
#' data(packDesc2021)
#' head(packDesc2021)
"packDesc2021"

#' packDesc2023: overview of ontoProc resources
#' @format data.frame instance
#' @note Brief survey of functions available to load serialized
#' ontology_index instances imported from OBO.  Focus is on versions added in 2023.
#' Several manual interventions were needed -- cellosaurus was too large to use the
#' script in inst/scripts/desc.R, and a number of ontologies do not have 2023 versions.
#' @usage data(packDesc2023)
#' @examples
#' data(packDesc2023)
#' head(packDesc2023)
"packDesc2023"
