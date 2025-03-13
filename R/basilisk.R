# necessary for python module control
bsklenv <- basilisk::BasiliskEnvironment(envname="bsklenv",
    pkgname="ontoProc",
    packages=c("h5py==3.13.0"), 
    pip=c("owlready2==0.47", "bioregistry==0.12.4"))
