#' use doc at https://rstudio.github.io/reticulate/articles/python_dependencies.html

#' user support for python module
#' @param \dots as needed
#' @param envname character(1) arbitrary
#' @export
install_owlready2 <- function(..., envname = "r-owlready2") {
  reticulate::py_install("owlready2", envname = envname, ...)
}


.onLoad <- function(...) {
  reticulate::use_virtualenv("r-owlready2", required = FALSE)
}
