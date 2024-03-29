# ---- Inhibitor ---- #

# UI ----

InhibitorUI <- function() {
  ns <- NS("inhibitor")
  
  tagList(
    h3(class = "tab-title", "Inhibitor analysis"),
    div(class = "tab-info", "This inhibitor analysis component is still under contruction."),
    uiOutput(ns("content"))
  )
}


# Server ----

#' @references colValidation
#' 
#' @param `data` a `reactive()` data frame containing the loaded clean data
#' @param `ready` a `reactive()` boolean indicating if the model is ready

InhibitorServer <- function(data, ready) {
  moduleServer(
    id = "inhibitor",
    function(input, output, session) {
      ns <- session$ns
      
      
      # Reactives ----
      
      
      # Observers ----
      
      
      # Outputs ----
      
      
      
    } # end
  )
}
