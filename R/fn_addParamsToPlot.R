
#' @description plot helper
#' @param `gg` the ggplot object
#' @param `params` list of params to add, including math notation
#' @returns updated ggplot object

addParamsToPlot <- function(gg, params) {
  y <- 0.95
  
  gg <- gg + annotate(
    "text",
    x = -Inf,
    y = y,
    hjust = -0.05,
    label = "Model parameters:",
    fontface = "bold"
  )
  
  for (par in params) {
    y <- y - 0.04
    gg <- gg + annotate(
      "text",
      x = -Inf,
      y = y,
      hjust = 0,
      label = par,
      parse = TRUE
    )
  }
  
  gg
}
