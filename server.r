# ---- Server ---- #

library(tidyverse)
library(shiny)
library(shinyjs)
library(DT)
# library(plotly)


server <- function(input, output, session) {
  
  # Data definitions ----
  
  values <- reactiveValues(data = tibble())
  
  
  
  # Reactions ----
  
  # action buttons
  observeEvent(input$loadSampleGermData, {
    values$data <- sampleGermData
    updateCollapse(session, "data", open = "view")
  })
  observeEvent(input$loadSamplePrimingData, {
    values$data <- samplePrimingData
    updateCollapse(session, "data", open = "view")
  })
  observeEvent(input$userData, {
    values$data <- read_csv(input$userData$datapath, col_types = cols())
    updateCollapse(session, "data", open = "view")
  })
  observeEvent(input$clearData, {
    values$data <- tibble()
    reset("userData")
  })
  observeEvent(input$viewColumnMatching, {
    updateCollapse(session, "data", open = "cols")
  })
  
  
  
  # Outputs ----
  
  # download buttons
  output$downloadTemplate <- downloadHandler(
    filename = "pbtm-data-template.csv",
    content = function(file) {write_csv(sampleTemplate, file)}
  )
  output$downloadSampleGermData <- downloadHandler(
    filename = "pbtm-sample-germination-data.csv",
    content = function(file) {write_csv(sampleGermData, file)}
  )
  output$downloadSamplePrimingData <- downloadHandler(
    filename = "pbtm-sample-priming-data.csv",
    content = function(file) {write_csv(samplePrimingData, file)}
  )
  
  
  output$columnDescriptions <- renderTable({columnDescriptions})
  
  output$currentDataTable <- renderDataTable({values$data})
  
  output$currentDataDisplay <- renderUI({
    if (nrow(values$data) == 0) {
      return("No data loaded.")
    } else {
      dataTableOutput("currentDataTable")
    }
  })
  
  columnNames <- reactive({
    req(values$data)
    names(values$data)
  })
  
  columnChoices <- reactive({
    setNames(as.list(c(NA, columnNames())), c("Not specified", columnNames()))
  })
  
  # observeEvent(values$data, {
  #   updateCollapse(session, "data", open = "columns")
  # })
  
  
  
  # Column name matching ----
  
  # generate selection boxes
  lapply(1:nrow(columnDefaults), function(i) {
    output[[paste0("colSelect", i)]] <- renderUI({
      selectInput(
        inputId = paste0("colSelect", i),
        label = columnDefaults$description[i],
        choices = columnChoices(),
        selected = columnDefaults$defaultColumn[i]
      )
    })
  })
  
  # validation message handler
  lapply(1:nrow(columnDefaults), function(i) {
    outCol <- paste0("colValidate", i)
    inCol <- paste0("colSelect", i)
    # colName <- input[[inCol]]
    # matchedCol <- values$data[[colName]]
    
    # output[[outCol]] <- renderUI({
    #   p(
    #     paste0("value =", input[[inCol]]), br(),
    #     if (!is.na(input[[inCol]])) {
    #       return(paste0("column type =", class(values$data[[input[[inCol]]]])))
    #     }
    #   )
    # })
    
    output[[outCol]] <- renderUI({
      
      req(input[[inCol]])
      
      if (input[[inCol]] == "NA") {
        ui <- list(
          p("No column specified.")
        )
      } else {
        ui <- list(
          p(
            paste0("value = ", input[[inCol]]), br(),
            paste0("column type = ", class(values$data[[input[[inCol]]]]))
          )
        )
      }
      ui
    })
  })
  
  
}

