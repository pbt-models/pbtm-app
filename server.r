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
  observeEvent(input$loadSampleGermData, {values$data <- sampleGermData})
  observeEvent(input$loadSamplePrimingData, {values$data <- samplePrimingData})
  observeEvent(input$userData, {values$data <- read_csv(input$userData$datapath, col_types = cols())})
  observeEvent(input$clearData, {
    values$data <- tibble()
    reset("userData")
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
  
  
  output$columnTypes <- renderTable({columnTypes})
  
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
  
  output$columnMatching <- renderUI({
    req(columnNames())
    
    choices <- setNames(as.list(c(NA, columnNames())), c("Not specified", columnNames()))
    
    if (length(columnNames()) == 0) {
      p("No column names detected, load a valid dataset.")
    } else {
      lapply(1:nrow(columnDefaults), function(i) {
        wellPanel(
          style = "display: inline-block; min-width: 20em;",
          selectInput(
            inputId = paste0("col.", columnDefaults$defaultColumn[i]),
            label = columnDefaults$description[i],
            choices = choices,
            selected = columnDefaults$defaultColumn[i]
          ),
          span("OK", style = "color: red;")
        )
      })
    }
  })
  
  output$columnValidation <- renderText("Hello world.")
  
  
  
}

