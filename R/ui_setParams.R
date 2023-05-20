
#' @description a shared ui component
#' @param ns namespace function from calling server
#' @param params vector of param names

setParamsUI <- function(ns, params) {
  namedWell(
    title = "Specify model coefficients (optional)",
    div(
      class = "flex-row",
      lapply(params, function(p) {
        numericInput(
          inputId = ns(paste0(p, "_set")),
          label = p,
          value = NA,
          step = .1,
          width = "30%"
        )
      })
    ),
    em("Specify individual model coefficients, or leave blank to allow the model to find a best-fit value.")
  )
}
