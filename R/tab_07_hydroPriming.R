# ---- Hydro Priming ---- #

# UI ----

hydroPrimingUI <- function() {
  ns <- NS("hydroPriming")
  
  tagList(
    h3(class = "tab-title", "Hydro priming analysis"),
    div(class = "tab-info", "This hydro priming analysis component is still under contruction."),
    uiOutput(ns("content"))
  )
}


# Server ----

#' @references colValidation
#' 
#' @param `data` a `reactive()` data frame containing the loaded clean data
#' @param `ready` a `reactive()` boolean indicating if the model is ready

hydroPrimingServer <- function(data, ready) {
  moduleServer(
    id = "hydroPriming",
    function(input, output, session) {
      ns <- session$ns
      
      
      # Reactives ----
      
      
      # Observers ----
      
      
      # Outputs ----
      
      
      
    } # end
  )
}
