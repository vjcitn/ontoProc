# [1] "cellTypeToGenes" "cellTypeToGO"    "children_TAG"    "getCellLineOnto"
# [5] "getCellOnto"     "getChebiLite"    "getEFOOnto"      "label_TAG"      
# [9] "secLevGen"       "show"            "siblings_TAG"   

#> valid_ontonames()
# [1] "caro"            "cellLineOnto"    "cellOnto"        "cellosaurusOnto"
# [5] "chebi_full"      "chebi_lite"      "diseaseOnto"     "efoOnto"        
# [9] "goOnto"          "hcaoOnto"        "mondo"           "patoOnto"       
#[13] "PROnto"          "uberon"          "Pronto"         



library(ontoProc)

context("ontology processing")

test_that("siblings compute", {
  efoOnto = getOnto("efoOnto")
  sibs = siblings_TAG( ontology = efoOnto )
  expect_true(is(sibs,"TermSet"))
})

test_that("cellTypeToGenes yields genes", {
  data(allGOterms)
  library(org.Hs.eg.db)
  cc = cellTypeToGenes("GABAergic neuron", allGOterms, org.Hs.eg.db)
  expect_true(nrow(cc)>=2)  # can change as annotations do
})

test_that("children_TAG works", {
  co = getOnto("cellOnto")
  chn = children_TAG("CL:0000540", co)
  expect_true(nrow(chn@cleanFrame)==43) 
})

#test_that("onto generators work", {
#  onts = c("getCellLineOnto", "getCellOnto", "getChebiLite", "getEFOOnto")
#  oo = vapply(onts, function(x) class(get(x)()), character(1))
#  expect_true(all(oo=="ontology_index"))
#}) # they were removed, getOnto should be used
  
test_that("label_TAG works", {
  co = getOnto("cellOnto")
  chn = label_TAG("CL:0000540", co)
  expect_true(as.character(chn) == "neuron")
})

test_that("secLevGen works", {
  co = getOnto("cellOnto")
  chn = secLevGen("neuron", co)
  expect_true(nrow(chn@cleanFrame)==43)
})
  

test_that("siblings_TAG works", {
  co = getOnto("cellOnto")
  chn = siblings_TAG("CL:0000540", co)
  expect_true(nrow(chn@cleanFrame)==52)
})

test_that("concatenation works", {
  efoOnto = getOnto("efoOnto")
  defsibs = siblings_TAG("EFO:1001209", efoOnto)
  n = length(defsibs@ontoTags)
  conc = c(defsibs, defsibs)
  expect_true(length(conc@ontoTags)==2*n)
})

