# ---- Server ---- #

server <- function(input, output, session) {
  # Values ----

  rv <- reactiveValues(
    data = tibble(),
    colStatus = NULL,
    modelReady = list()
  )

  # Outputs ----

  ## Badge for loading tab ----
  output$LoadBadge <- renderUI({
    ready <- nrow(rv$data) > 0
    if (ready) {
      span(class = "badge bg-success", "OK")
    } else {
      span(class = "badge bg-warning", "!")
    }
  })

  ## Badges for model tabs ----
  lapply(modelNames, function(m) {
    output[[paste0(m, "Badge")]] <- renderUI({
      ready <- truthy(rv$modelReady[[m]])
      if (ready) {
        span(class = "badge bg-success", "OK")
      } else {
        span(class = "badge bg-danger", "X")
      }
    })
  })

  ## Sidebar navigation ----
  lapply(
    c("loadData", modelNames),
    function(m) {
      observeEvent(input[[paste0("nav_", m, "Tab")]], {
        nav_select("mainNav", paste0("nav_", m, "Tab"))
      })
    }
  )

  # Module Servers ----

  # IntroServer()

  # capture return values from the load data server
  loadDataReturns <- loadDataServer()

  # save return values
  observe({
    rv$data <- loadDataReturns()$data
    rv$colStatus <- loadDataReturns()$colStatus
    rv$modelReady <- loadDataReturns()$modelReady
  })

  # Call each of the model servers. Models with a spec run through the factory
  # (modelServer); Germination keeps its bespoke GerminationServer.
  lapply(modelNames, function(m) {
    dataReactive <- reactive(rv$data)
    readyReactive <- reactive(truthy(rv$modelReady[[m]]))
    if (m %in% names(modelSpecs)) {
      modelServer(
        spec = modelSpecs[[m]],
        data = dataReactive,
        ready = readyReactive
      )
    } else {
      do.call(
        paste0(m, "Server"),
        list(data = dataReactive, ready = readyReactive)
      )
    }
  })

  ## Modal handler ----
  observe({
    m <- req(input$show_modal)
    show_modal(md = m$md)
  })
}
