# ---- addParamsToPlot ---- #

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
