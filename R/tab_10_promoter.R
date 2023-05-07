# ---- Promoter ---- #

# UI ----

PromoterUI <- function() {
  ns <- NS("promoter")
  
  tagList(
    h3(class = "tab-title", "Promoter analysis"),
    div(class = "tab-info", "This promoter analysis component is still under contruction."),
    uiOutput(ns("content"))
  )
}


# Server ----

#' @references colValidation
#' 
#' @param `data` a `reactive()` data frame containing the loaded clean data
#' @param `ready` a `reactive()` boolean indicating if the model is ready

PromoterServer <- function(data, ready) {
  moduleServer(
    id = "promoter",
    function(input, output, session) {
      ns <- session$ns
      
      
      # Reactives ----
      
      
      # Observers ----
      
      
      # Outputs ----
      
      
      
    } # end
  )
}
