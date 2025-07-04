#' use an LLM to match informal terms to terms in an ontology
#' @import ellmer
#' @importFrom dplyr left_join
#' @param informal_terms  character() vector of terms not necessarily found in ontology
#' @param ontology_terms character() vector of ontology terms
#' @param ontology_tags character() vector of tags for ontology terms, must be of same length as ontology_terms
#' @param ellmer_chatfun function available in ellmer to connect to chatbot
#' @param llm_model character(1) used with chat_openai in ellmer, defaults to "gpt-4.1-2025-04-14",
#' or other models for other providers available through ellmer.
#' @note Expects to have OPENAI_API_KEY set if an openai chatfun is used, or GOOGLE_API_KEY
#' if, e.g., a gemini chatfun is used.
#' @return A data.frame with columns informal_term, formal_term, similarity_score, and tag.
#' Invisible attributes chat_tokens, chat_cost, and chat_provider are also present.
#' @examples
#' if (interactive()) {
#'  ctypes = c("tPlasma cells", "tMoMacDC", "tT cells",   # from Zilionis
#'   "tB cells", "tNK cells", "tNeutrophils", "Fibroblasts", "Type II cells", 
#'   "tpDC", "Endothelial cells", "tMast cells", "Smooth muscle cells", 
#'   "ND", "Club cells", "bNeutrophils", "bT cells", "bMonocytes", 
#'   "bNK cells", "bRBC", "bpDC", "bB cells", "bPlasma cells", "bPlatelets", 
#'   "tRBC", "Type I cells", "Ciliated cells", "bBasophils")
#'  cc = owl2cache(url="http://purl.obolibrary.org/obo/cl.owl")
#'  cloi = setup_entities2(cc)
#'  oname = cloi$name
#'  actual = grep("CL_", names(oname))
#'  oterms = as.character(oname[actual])
#'  otags = names(oname[actual])
#'  octy = formalize(ctypes, oterms, otags)
#'  head(octy)
#'  attr(octy, "chat_tokens")
#'  onto_plot2(cloi, unique(na.omit(octy$tag)))
#' }
#' @export 
formalize = function(informal_terms, ontology_terms, ontology_tags,
   ellmer_chatfun = ellmer::chat_openai, llm_model = "gpt-4.1-2025-04-14") {
#
# most of the code was produced using perplexity, asking it to use ellmer
#
  stopifnot(length(ontology_terms)==length(ontology_tags))
  # Create a prompt for the LLM
  prompt <- paste0(
    "Given the following semicolon-delimited list of informal medical terms:\n",
    paste(informal_terms, collapse = "; "), "\n",
    "and the following semicolon-delimited list of formal ontology terms:\n",
    paste(ontology_terms, collapse = "; "), "\n",
    "For each informal term, return the closest matching formal ontology term, do not
       create any new terms.  For example, RBC matches erythrocyte.",
    "Return the results as a table with columns: informal_term, formal_term, similarity_score (0-1, optional).",
    "Do not create any terms that are not in the formal ontology term list.",
    "Return only the original input values of the informal terms, do not change them in any way."
  )
  
  # Define the expected structured output type
  type_match <- type_array(
    "Array of matches between informal and formal terms.",
    type_object(
      informal_term = type_string("The informal term."),
      formal_term = type_string("The closest formal ontology term."),
      similarity_score = type_number("Optional: similarity score between 0 and 1.")
    )
  )
  
  # Create a chat object (using OpenAI as an example)
  chat <- ellmer_chatfun(model = llm_model)
  
  # Get structured matches
  matches <- chat$chat_structured(prompt, type = type_match)
  
  # Convert to data frame for display
  ans = as.data.frame(matches)
  tagdf = data.frame(formal_term=ontology_terms, tag=ontology_tags)
  ans = dplyr::left_join(ans, tagdf, by="formal_term")
  attr(ans, "chat_tokens") = invisible(chat$get_tokens())
  attr(ans, "chat_cost") = invisible(chat$get_cost())
  attr(ans, "chat_model") = invisible(chat$get_model())
  ans
}
