# ---- Hydropriming ---- #

# UI ----

HydroprimingUI <- function() {
  ns <- NS("hydropriming")
  
  tagList(
    h3(class = "tab-title", "Hydropriming analysis"),
    div(class = "tab-info", "This hydropriming analysis component is still under contruction."),
    uiOutput(ns("content"))
  )
}


# Server ----

#' @references colValidation
#' 
#' @param `data` a `reactive()` data frame containing the loaded clean data
#' @param `ready` a `reactive()` boolean indicating if the model is ready

HydroprimingServer <- function(data, ready) {
  moduleServer(
    id = "hydropriming",
    function(input, output, session) {
      ns <- session$ns
      
      
      # Reactives ----
      
      
      # Observers ----
      
      
      # Outputs ----
      
      
      
    } # end
  )
}
