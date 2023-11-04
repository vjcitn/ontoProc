#' decompress ordo owl file
#' @param target character(1) path to where decompressed owl will live
#' @export
get_ordo_owl_path = function(target=tempdir()) {
   ini = system.file("owl", "ordo.owl.gz", package="ontoProc")
   file.copy(ini, target)
   goal = file.path(target, "ordo.owl")
   if (!file.exists(goal))
      R.utils::gunzip(file.path(target, "ordo.owl.gz"))
   goal
}
   
