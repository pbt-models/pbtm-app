
#' @description a shared ui component
#' @param ns namespace function from calling server

germSpeedSliderUI <- function(ns) {
  namedWell(
    title = "Germination speed options",
    sliderInput(
      inputId = ns("germSpeedBasis"),
      label = "Germination (%) for speed calculation:",
      min = 10,
      max = 100,
      value = 50,
      step = 5
    )
  )
}


