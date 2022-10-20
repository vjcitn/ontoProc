#' app to review molecular properties of cell types via cell ontology
#' @param cl an import of a Cell Ontology (or extended Cell Ontology) in ontology_index form
#' @param pr an import of a Protein Ontology in ontology_index form
#' @param go an import of a Gene Ontology in ontology_index form
#' @note Prototype of harvesting of cell ontology by searching
#' has_part, has_plasma_membrane_part, intersection_of and allied
#' ontology relationships.  Uses shiny.  Can perform better if getPROnto() and getGeneOnto() values
#' are in .GlobalEnv as pr and go respectively.
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter transmute left_join select
#' @importFrom DT renderDataTable dataTableOutput
#' @return a data.frame with features for selected cell types
#' @examples
#' if (interactive()) {
#'    co = getOnto("cellOnto", year_added="2021")  # has plasma membrane relations
#'    go = getOnto("goOnto", "2021")
#'    pr = getOnto("Pronto", "2021") # peculiar tag used in legacy, would be PROnto with 2022
#'    ctmarks(co, go, pr)
#' }
#' @export
ctmarks = function(cl, pr, go) {
 rp = recognizedPredicates()
 lens = lapply(rp, function(x) which(sapply(cl[[x]],length)>0))
 kp = unique(unlist(lapply(lens,names)))
 clClassNames = sort(cl$name[kp])
 clClassDF = data.frame(tag=names(clClassNames), 
    text=as.character(clClassNames), stringsAsFactors=FALSE)
 clCL = clClassDF %>% dplyr::filter(grepl("^CL", tag))
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(width=3,
    helpText("CL classes, limited to those for which presence or absence
of plasma membrane parts (or high or low plasma membrane amounts, expression, etc.) are indicated.  See ontoProc::recognizedPredicates() for full list"),
    selectInput("CLclasses", "CL classes", 
      choices = clCL$text, selected=clCL$text[1],
      multiple=FALSE),
    helpText("Visit the 'tags' tab to examine assertions about the selected cell type.  A data.frame is accumulated for all selections, and sent to the current session when the app is stopped."),
    actionButton("btnSend", "Stop app")
    ),
   mainPanel(
    tabsetPanel(
     tabPanel("plot", plotOutput("deriv", height="900px")),
     tabPanel("tags", 
        helpText("The table generated here consists of information obtained about the query cell type by traversing the 'intersection_of' Cell Ontology elements associated with it.  When multiple distinct entries are present in the 'tag' and 'name' fields, the properties of the query cell type are asserted to be the intersection of the properties of the named additional cell types.  'SYMBOL' values are obtained for PR: entries that have a
EXACT PRO-short-label [PRO:DNx] annotation, and are missing for other entries."),
        DT::dataTableOutput("picks")),
     tabPanel("about", helpText("The intention of this app is to
navigate the Cell Ontology, and its formally linked companions Protein
Ontology and Gene Ontology, for developing views of molecular components
that distinguish types of cells.  Given a name of a cell
type to which a CL: tag corresponds, the program finds the
tag and the 'intersection_of', 'has/lacks_plasma_membrane_part',
'has_high/low_plasma_membrane_amount' elements of
the associated ontology annotation.  When these elements involve
PR: or GO: references, these are retrieved and reported.  This
process iterates over all the CL: references in the
intersection_of element for the input cell type."), 
      helpText("Current ontology versions in use in this app call:"),
      helpText("Cell Ontology:"),
      verbatimTextOutput("co_vers"),
      helpText("Gene Ontology:"),
      verbatimTextOutput("go_vers"),
      helpText("Protein Ontology:"),
      verbatimTextOutput("pr_vers"),
      helpText(" "),
      helpText("Ontologies available for use in this package, 2022 versions:"),
      DT::dataTableOutput("desc2022"),
      helpText("Ontologies available for use in this package, 2021 versions:"),
      DT::dataTableOutput("desc2021")
      )
     )
   )
  )
 )
 server = function(input, output) {
  output$deriv = renderPlot({
   tag = names(cl$name[ which(cl$name == input$CLclasses) ] )
   anc = unique(c(cl$ancestors[[tag]], isachain(cl, tag))) # cl$ancestors[[tag]]
   anc = grep("^CL", anc, value=TRUE)
   drp1 = cl$ancestors["CL:0000548"][[1]] # excl ancestors of
   drp2 = cl$ancestors["CL:0000003"][[1]] # native and animal
   drp = union(drp1, drp2)
   #onto_plot(cl, intersect(setdiff(anc,drp),clCL$tag))
   onto_plot2(cl, setdiff(anc,drp))
   })
  output$picks = DT::renderDataTable({
    cumu <- NULL
    curtag = (clCL %>% dplyr::filter(text == input$CLclasses))[["tag"]]
    ans = CLfeats(cl, curtag, pr=pr, go=go)
    cumu <<- rbind(cumu, ans)
    ans
  })
  output$desc2022 = DT::renderDataTable({
    data(packDesc2022)
    packDesc2022
    })
  output$desc2021 = DT::renderDataTable({
    data(packDesc2021)
    packDesc2021
    })
  output$co_vers = renderPrint({
     attributes(cl)$version[1:2]
     })
  output$go_vers = renderPrint({
     attributes(go)$version[1:2]
     })
  output$pr_vers = renderPrint({
     attributes(pr)$version[1:2]
     })
  observe({
            if(input$btnSend > 0)
               isolate({
                 stopApp(returnValue=0)
                      })  
           })  
 }
 runApp(list(ui=ui, server=server))
 cumu
}
