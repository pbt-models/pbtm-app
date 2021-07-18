# ---- Server ---- #

library(tidyverse)
library(shiny)
library(DT)
# library(plotly)


server <- function(input, output, session) {
  
  currentData <- reactiveValues(data = tibble())
  
  # load example data when button pressed
  observeEvent(input$loadSampleGermData, {
    message("Loading sample germination data.")
    currentData$data <- sampleGermData
    output
  })
  
  observeEvent(input$loadSamplePrimingData, {
    message("Loading sample priming data.")
    currentData$data <- samplePrimingData
    # handleLoad()
  })
  
  observeEvent(input$user_data, {
    message("Loading user data.")
    currentData$data <- read_csv(input$userData$datapath, col_types = cols())
    # handleLoad()
  })
  
  observe({
    print(currentData$data)
  })
  
  output$currentDataTable <- renderDataTable(currentData$data)
  
  output$currentDataDisplay <- renderUI({
    if (nrow(currentData$data) == 0) {
      return("No data loaded.")
    }
    
    dataTableOutput("currentDataTable")
  })
  
  

  # 
  # handleLoad <- function() {
  #   if (nrow(currentData) > 0) {
  #     message("Rendering data table")
  #     output$currentDataDisplay <- renderUI({
  #       tagList(
  #         renderDataTable(currentData)
  #       )
  #     })
  #   } else {
  #     message("No data loaded")
  #     output$currentDataDisplay <- renderUI({
  #       tagList(
  #         renderText("No data currently selected.")
  #       )
  #     })
  #   }
  # }
  
  
}

