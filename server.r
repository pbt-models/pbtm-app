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
  
  
  output$currentDataTable <- renderTable({values$data})
  
  output$currentDataDisplay <- renderUI({
    if (nrow(values$data) == 0) {
      return("No data loaded.")
    } else {
      tableOutput("currentDataTable")
    }
  })
  
  
  
}

