
#' @description a shared ui component
#' @param ns namespace function from calling server

dataTransfUI <- function(ns) {
  radioButtons(
    inputId = ns("dataTransfSelect"),
    label = "Select dosage transformation:",
    choices = list(
      "None" = "none",
      "Logarithmic" = "log"
    )
  )
}
