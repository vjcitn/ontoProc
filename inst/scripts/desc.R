
nroots = function(x) {
 obs <- if (is.null(x$obsolete)) 
        rep(FALSE, length(x$id))
    else x$obsolete
    roots <- x$id[!obs & sapply(x$parents, length) == 0]
length(roots)
}

#funcs = grep("^get", ls("package:ontoProc"), value=TRUE)

library(S4Vectors)
library(dplyr)
library(ontoProc)
select <- dplyr::select

vn = valid_ontonames()
if (!exists("ontos")) {
  if (!file.exists("ontos.rda")) ontos = lapply(vn, getOnto) else load("ontos.rda")
}
names(ontos) = vn
ontos = lapply(seq_len(length(ontos)), function(i) {attr(ontos[[i]], "oname") <- names(ontos)[i]; ontos[[i]]})

desc = function(x) {
  ona = function(x) if(length(x)==0 || nchar(x)==0) NA else x
  dsub = function(x) sub("data-version: ", "", x)
  fsub = function(x) sub("format-version: ", "", x)
  #ont = do.call(x, list()); 
   ont = x
  nm = attr(ont, "oname")
   dv = grep("^data-version", attr(ont, "version"), value=TRUE)
   fv = grep("^format-version", attr(ont, "version"), value=TRUE)
   data.frame(name=nm, nclass=length(ont$name), nprop=length(names(ont)), nroots=nroots(ont), datav=ona(dsub(dv)), fmtv=ona(fsub(fv))) #, stringsAsFactors=FALSE)
}
#packdesc = do.call(rbind, lapply(funcs, desc))
packdesc = do.call(rbind, lapply(ontos, desc))
write.csv(packdesc, file="packdesc2.csv")
