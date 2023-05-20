# ---- cumFracSliders ---- #

#' @param ns namespace function from calling server

germSlidersUI <- function(ns) {
  namedWell(
    title = "Germination contraints",
    sliderInput(
      inputId = ns("maxCumFrac"),
      label = "Maximum germination (%) observed",
      min = 10,
      max = 100,
      value = 100,
      step = 1
    ),
    sliderInput(
      inputId = ns("cumFracRange"),
      label = "Included interval (%):",
      min = 0,
      max = 100,
      value = c(0, 100)
    )
  )
}
