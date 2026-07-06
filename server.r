# ---- Server ---- #

server <- function(input, output, session) {
  # Values ----

  rv <- reactiveValues(
    raw_data = tibble(),
    data = tibble(),
    col_status = NULL,
    model_ready = list()
  )

  # Outputs ----

  ## Badge for loading tab ----
  output$badge_loadData <- renderUI({
    ready <- nrow(rv$data) > 0
    if (ready) {
      span(class = "badge bg-success", icon("check"))
    } else {
      span(class = "badge bg-warning", icon("exclamation"))
    }
  })

  ## Badges for model tabs ----
  lapply(modelNames, function(m) {
    output[[paste0("badge_", m)]] <- renderUI({
      ready <- truthy(rv$model_ready[[m]])
      if (ready) {
        span(class = "badge bg-success", icon("check"))
      } else {
        span(class = "badge bg-danger", icon("times"))
      }
    })
  })

  # Module Servers ----

  loadDataServer(rv)

  # Call each of the model servers. Models with a spec run through the factory
  # (modelServer); Germination keeps its bespoke GerminationServer.
  lapply(modelNames, function(m) {
    dataReactive <- reactive(rv$data)
    readyReactive <- reactive(truthy(rv$model_ready[[m]]))
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
