
#' @description ui element showing model error if present
#' @param results object from modelResults either list or string error
#' @returns a rendered UI element or nothing

modelErrorUI <- function(results) {
  renderUI({
    if (is.list(results())) return()
    div(
      class = "model-error",
      sprintf("Model failed under current settings: %s. Last valid model coefficients shown above.", results())
    )
  })
}
