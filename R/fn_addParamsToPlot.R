
#' @description plot helper
#' @param `gg` the ggplot object
#' @param `params` list of params to add, including math notation
#' @param `y` initial max y value to anchor the text
#' @returns updated ggplot object

addParamsToPlot <- function(gg, params, y) {
  lineheight = y / 25
  y <- y - lineheight
  
  gg <- gg + annotate(
    "text",
    x = -Inf,
    y = y,
    hjust = -0.1,
    label = "Model parameters:",
    fontface = "bold"
  )
  
  for (par in params) {
    y <- y - lineheight

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
