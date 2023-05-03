# ---- Server ---- #

server <- function(input, output, session) {
  
  # Values ----
  
  rv <- reactiveValues(
    data = tibble(),
    colStatus = NULL,
    modelReady = list()
  )
  
  
  # Reactives ----
  
  ## dataLoaded // boolean: data has rows ----
  dataLoaded <- reactive({
    nrow(rv$data) > 0
  })
  
  ## trtChoices // list of factor cols ----
  trtChoices <- reactive({
    req(rv$colStatus)
    cols <- sapply(1:nCols, function(i) {
      if (rv$colStatus[i] == T && colValidation$Role[i] == "Factor") colValidation$Column[i]
    })
    compact(cols)
  })
  
  
  # Outputs ----
  
  ## Reactive menu entry for loading tab ----
  output$LoadMenu <- renderMenu({
    ready <- dataLoaded()
    menuItem(
      "Upload data",
      tabName = "LoadTab",
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
  
  ## Intro ----
  introServer()
  
  ## Load Data ----
  loadDataReturn <- loadDataServer()
  
  observe({
    rv$data <- loadDataReturn()$data
    rv$colStatus <- loadDataReturn()$colStatus
    rv$modelReady <- loadDataReturn()$modelReady
  })
  
  ## Germination ----
  germinationServer(
    data = reactive(rv$data),
    ready = reactive(truthy(rv$modelReady$Germination)),
    trtChoices = reactive(trtChoices())
  )
  
  ## Thermal Time ----
  thermalTimeServer(
    data = reactive(rv$data),
    ready = reactive(truthy(rv$modelReady$ThermalTime))
  )

  ## Hydro Time  ----
  hydroTimeServer(
    data = reactive(rv$data),
    ready = reactive(truthy(rv$modelReady$HydroTime))
  )
  
  ## Hydrothermal Time ----
  hydrothermalTimeServer(
    data = reactive(rv$data),
    ready = reactive(truthy(rv$modelReady$HydrothermalTime))
  )
  
  ## Hydro Priming ----
  hydroPrimingServer(
    data = reactive(rv$data),
    ready = reactive(truthy(rv$modelReady$HydroPriming))
  )
  
  ## Hydrothermal Priming ----
  hydrothermalPrimingServer(
    data = reactive(rv$data),
    ready = reactive(truthy(rv$modelReady$HydrothermalPriming))
  )
  
  ## Aging ----
  agingServer(
    data = reactive(rv$data),
    ready = reactive(truthy(rv$modelReady$Aging))
  )
  
  ## Promoter model ----
  promoterServer(
    data = reactive(rv$data),
    ready = reactive(truthy(rv$modelReady$Promoter))
  )
  
  ## Inhibitor model ----
  inhibitorServer(
    data = reactive(rv$data),
    ready = reactive(truthy(rv$modelReady$Inhibitor))
  )
  
  
  # Gracefully exit ----
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}
