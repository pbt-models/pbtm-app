# ---- Aging ---- #

# UI ----

agingUI <- function() {
  ns <- NS("aging")
  
  tagList(
    h3(class = "tab-title", "Aging analysis"),
    div(class = "tab-info", ),
    uiOutput(ns("content"))
  )
}


# Server ----

#' @references colValidation
#' 
#' @param `data` a `reactive()` data frame containing the loaded clean data
#' @param `ready` a `reactive()` boolean indicating if the model is ready

agingServer <- function(data, ready) {
  moduleServer(
    id = "aging",
    function(input, output, session) {
      ns <- session$ns
      
      
      # Reactives ----
      
      ## dataReady ----
      dataReady <- reactive({
        truthy(data()) & ready()
      })
      
      
      # Observers ----
      
      
      # Outputs ----
      
      
      
    } # end
  )
}