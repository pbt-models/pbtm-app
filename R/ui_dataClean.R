# ---- dataCleanSelect ----#

#' @param ns namespace function from calling server

dataCleanUI <- function(ns) {
  radioButtons(
    inputId = ns("dataCleanSelect"),
    label = "Select data cleaning:",
    choices = list(
      "Original" = "original",
      "Cleaned (remove duplicates)" = "clean"
    )
  )
}
