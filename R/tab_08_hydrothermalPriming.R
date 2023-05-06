# ---- Hydrothermal Priming ---- #

# UI ----

hydrothermalPrimingUI <- function() {
  ns <- NS("hydrothermalPriming")
  
  tagList(
    h3(class = "tab-title", "Hydrothermal priming analysis"),
    div(class = "tab-info", "This hydrothermal priming analysis component is still under contruction."),
    uiOutput(ns("content"))
  )
}


# Server ----

#' @references colValidation
#' 
#' @param `data` a `reactive()` data frame containing the loaded clean data
#' @param `ready` a `reactive()` boolean indicating if the model is ready

hydrothermalPrimingServer <- function(data, ready) {
  moduleServer(
    id = "hydrothermalPriming",
    function(input, output, session) {
      ns <- session$ns
      
      
      # Reactives ----
      
      
      # Observers ----
      
      
      # Outputs ----
      
      
      
    } # end
  )
}
