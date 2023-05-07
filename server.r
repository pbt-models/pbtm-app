# ---- Server ---- #

server <- function(input, output, session) {
  
  # Values ----
  
  rv <- reactiveValues(
    data = tibble(),
    colStatus = NULL,
    modelReady = list()
  )
  
  
  # Outputs ----
  
  ## Reactive menu entry for loading tab ----
  output$LoadMenu <- renderMenu({
    ready <- nrow(rv$data) > 0
    menuItem(
      "Upload data",
      tabName = "LoadDataTab",
      badgeLabel = ifelse(ready, "OK", "!"),
      badgeColor = ifelse(ready, "green", "yellow")
    )
  })
  
  ## Reactive menu entries for models ----
  lapply(modelNames, function(m) {
    output[[paste0(m, "Menu")]] <- renderMenu({
      ready <- truthy(rv$modelReady[[m]])
      menuItem(
        to_any_case(m, case = "sentence"),
        tabName = paste0(m, "Tab"),
        badgeLabel = ifelse(ready, "OK", "X"),
        badgeColor = ifelse(ready, "green", "red")
      )
    })
  })
  

  # Module Servers ----
  
  IntroServer()
  
  # capture return values from the load data server
  loadDataReturns <- LoadDataServer()
  
  # save return values
  observe({
    rv$data <- loadDataReturns()$data
    rv$colStatus <- loadDataReturns()$colStatus
    rv$modelReady <- loadDataReturns()$modelReady
  })
  
  # Call each of the model servers
  lapply(modelNames, function(m) {
    do.call(
      paste0(m, "Server"),
      list(
        data = reactive(rv$data),
        ready = reactive(truthy(rv$modelReady[[m]]))
      )
    )
  })
  
  
  # Gracefully exit ----
  
  session$onSessionEnded(\() stopApp())
  
}
