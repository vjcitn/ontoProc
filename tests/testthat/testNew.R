

library(ontoProc)

context("working with owl")

test_that("owl2cache works", {
  ca = BiocFileCache::BiocFileCache()
  hppa = owl2cache(ca, 
    url="http://purl.obolibrary.org/obo/hp/releases/2023-10-09/hp-base.owl")
  expect_true(is(hppa, "character"))
  expect_true(length(hppa) == 1L)
  })

test_that("setup_entities works", {
  ca = BiocFileCache::BiocFileCache()
  hppa = owl2cache(ca, 
    url="http://purl.obolibrary.org/obo/hp/releases/2023-10-09/hp-base.owl")
  hpe = setup_entities(hppa)
  expect_true(inherits(hpe, "owlents"))
  expect_true(all(head(hpe$clnames) ==
    c("HP_0000001", "CHEBI_12777", "CHEBI_132952", "CHEBI_138675", 
         "CHEBI_15318", "CHEBI_15361")))
  expect_true(length(hpe$allents) == 20179)
  expect_true(hpe$iri == "http://purl.obolibrary.org/obo/hp/hp-base.owl#")
  })


