
#' @description plot helper
#' @param `gg` the ggplot object
#' @param `params` list of params to add, including math notation
#' @returns updated ggplot object

addParamsToPrimingPlot <- function(gg, params) {
  y <- 0.038
  
  gg <- gg + annotate(
    "text",
    x = -Inf,
    y = y,
    hjust = -0.1,
    label = "Model parameters:",
    fontface = "bold"
  )
  
  for (par in params) {
    y <- y - 0.002
    
    gg <- gg + annotate(
      "text",
      x = -Inf,
      y = y,
      hjust = -0.15,
      label = par,
      parse = TRUE
    )
  }
  
  gg
}
