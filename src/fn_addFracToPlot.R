
#' @description plot helper
#' @param maxFrac horizontal cutoff value
#' @returns list of ggproto objects to add to ggplot

addFracToPlot <- function(maxFrac) {
  list(
    annotate(
      "rect",
      xmin = 0,
      xmax = Inf,
      ymin = maxFrac,
      ymax = 1,
      fill = "grey",
      alpha = 0.1
    ),
    geom_hline(
      yintercept = maxFrac,
      color = "darkgrey",
      linetype = "dashed"
    )
  )
}
