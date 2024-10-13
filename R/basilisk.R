# necessary for python module control
bsklenv <- basilisk::BasiliskEnvironment(envname="bsklenv",
    pkgname="ontoProc",
    packages=c("h5py==3.6.0"), 
    pip=c("owlready2==0.46", "bioregistry==0.11.18"))
