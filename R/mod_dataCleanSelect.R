# ---- dataCleanSelect ----#

#' @param ns namespace function from calling server

dataCleanSelect <- function(ns) {
  radioButtons(
    inputId = ns("dataCleanSelect"),
    label = "Select data cleaning:",
    choices = list(
      "Original" = "original",
      "Cleaned (remove duplicates)" = "clean"
    )
  )
}
