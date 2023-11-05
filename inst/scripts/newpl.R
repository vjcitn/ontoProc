
library(ontoProc)
library(ontologyIndex)
cl3k = c("CL:0000492", "CL:0001054", "CL:0000236", 
  "CL:0000625", "CL:0000576", 
  "CL:0000623", "CL:0000451", "CL:0000556")
n3k = gsub(":", "_", cl3k)
cl2 = setup_entities("cl.owl")
litc = cl2[n3k]
alitc = ancestors(litc)
anames = ancestors_names(alitc)
alluse = unique(unlist(anames))
red = cl2[alluse]
myo = ontology_index(parents=parents(red), name=labels(red))
onto_plot2(myo, n3k)
