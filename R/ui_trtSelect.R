
#' @description a shared ui component
#' @param ns namespace function from calling server
#' @param data reactive dataset from which to pull TrtID values

trtSelectUI <- function(ns, trtCols, data) {
  
  trtColChoices <- list()
  for (col in trtCols) {
    trtColChoices[[col]] = sort(unique(data()[[col]]))
  }
  
  trtSelectServer(trtColChoices)
  
  renderUI({
    bsCollapse(
      id = ns("trtFilterCollapse"),
      bsCollapsePanel(
        title = "Additional treatment filters",
        value = "panel",
        div(
          style = "padding: 10px;",
          checkboxGroupInput(
            inputId = ns("trtFilterCols"),
            label = "Filter by:",
            choices = trtCols,
            inline = TRUE
          ),
          uiOutput(ns("trtFilters"))
        )
      )
    )
  })
}


#' @description creates the rendered ui for treatment filters
#' @param trtChoices a named list containing the filter options

trtSelectServer <- function(trtChoices) {
  moduleServer(
    id = NULL,
    function(input, output, session) {
      ns <- session$ns
      
      # render the selection menus for only the trt cols picked
      output$trtFilters <- renderUI({
        style <- NULL
        if (is.null(input$trtFilterCols)) {
          ui <- style <- NULL
        } else {
          ui <- lapply(input$trtFilterCols, function(col) {
            uiOutput(ns(paste0("trtSelect-", col)))
          })
          style <- "warning"
        }
        updateCollapse(
          session = session,
          id = "trtFilterCollapse",
          style = list("panel" = style)
        )
        ui
      })
      
      # create each checkbox selection group
      lapply(names(trtChoices), function(col) {
        id <- paste0("trtSelect-", col)
        print(id)
        output[[id]] <- renderUI({
          checkboxGroupInput(
            inputId = ns(id),
            label = col,
            choices = trtChoices[[col]],
            selected = trtChoices[[col]],
            inline = TRUE
          )
        })
      })
    }
  )
}
