# ---- Server ---- #

library(tidyverse)
library(shiny)
library(shinyjs)
library(DT)
# library(plotly)


server <- function(input, output, session) {
  
  # Data definitions ----
  
  values <- reactiveValues(data = tibble())
  colStatus <- reactiveValues()
  modelStatus <- reactiveValues()
  
  
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
  
  
  
  # Column matching boxes ----
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
  
  # Column validation messages ----
  lapply(1:nrow(columnDefaults), function(i) {
    
    outCol <- paste0("colValidate", i)
    inCol <- paste0("colSelect", i)
    expectedType <- columnValidation$Type[i]
    minValue <- columnValidation$Min[i]
    maxValue <- columnValidation$Max[i]
    ui <- NULL
    msg <- NULL
    
    output[[outCol]] <- renderUI({
      
      req(input[[inCol]])
      
      # check if no column selected
      if (input[[inCol]] == "NA") {
        msg <- list(span("No column specified.", style = "color:orange"))
        colStatus[[paste0("col", i)]] <- F
        return(msg)
      }
      
      col <- values$data[[input[[inCol]]]]
      
      if (anyNA(col)) {
        msg <- list(br(), span("Warning: Missing value in data", style = "color:red"))
      }
      
      colType <- class(col)
      ui <- list(paste("Type =", colType))
      
      if (colType == "numeric") {
        add <- list(
          br(),
          paste0("Min = ", round(min(col), 2), ", Max = ", round(max(col), 2))
        )
        ui <- append(ui, add)
        
        # column type mismatch
        if (!is.na(expectedType) & colType != expectedType) {
          newmsg <- list(br(), span("Error: Incorrect column type, expected", expectedType, style = "color:red"))
          msg <- append(msg, newmsg)
        }
        
        # min check
        if (!is.na(minValue) & min(col) < minValue) {
          newmsg <- list(br(), span("Error: Min value less than", minValue, style = "color:red"))
          msg <- append(msg, newmsg)
        }
        
        # max check
        if (!is.na(maxValue) & max(col) > maxValue) {
          newmsg <- list(br(), span("Error: Max value greater than ", maxValue, style = "color:red"))
          msg <- append(msg, newmsg)
        }
        
      } else if (!is.na(expectedType) & colType != expectedType) {
        newmsg <- list(br(), span("Error: Incorrect column type, expected", expectedType, style = "color:red"))
        msg <- append(msg, newmsg)
      }
      
      if (is.null(msg)) {
        msg <- list(span(strong("OK"), style = "color:blue"))
        colStatus[[paste0("col", i)]] <- T
      } else {
        colStatus[[paste0("col", i)]] <- F
      }
      
      list(p(ui), p(msg))
      
    })
  })
  
  
  
  # Model readiness ----
  checkModelReadiness <- function(col) {
    compare <- sapply(1:nrow(columnDefaults), function(i) {
      test <- (col[i] == T & colStatus[[paste0("col", i)]] == T) | (col[i] == F)
      if (length(test) == 0) {F} else {test}
    })
    !(F %in% compare)
  }
  
  HPModelReady <- reactive({checkModelReadiness(columnValidation$HydroPriming)})
  HTPModelReady <- reactive({checkModelReadiness(columnValidation$HydroThermalPriming)})
  HTModelReady <- reactive({checkModelReadiness(columnValidation$HydroTime)})
  TTModelReady <- reactive({checkModelReadiness(columnValidation$ThermalTime)})
  HTTModelReady <- reactive({checkModelReadiness(columnValidation$HydroThermalTime)})
  AgingModelReady <- reactive({checkModelReadiness(columnValidation$Aging)})
  PromoterModelReady <- reactive({checkModelReadiness(columnValidation$Promoter)})
  InhibitorModelReady <- reactive({checkModelReadiness(columnValidation$Inhibitor)})
  
  
  
  # Model readiness display ----
  output$modelStatus <- renderUI({
    
    status <- function(x) {
      if (x) {
        span(strong("Ready"), style = "color:blue")
      } else {
        span(strong("Not ready"), "- required columns missing", style = "color:red")
      }
    }
    
    list(
      p(
        "Hydro Priming model:", status(HPModelReady()), br(),
        "Hydro Thermal Priming model:", status(HTPModelReady()), br(),
        "Hydro Time model:", status(HTModelReady()), br(),
        "Thermal Time model:", status(TTModelReady()), br(),
        "Hydro Thermal Time model:", status(HTTModelReady()), br(),
        "Aging model:", status(HTTModelReady()), br(),
        "Promoter model:", status(HTTModelReady()), br(),
        "Inhibitor model:", status(HTTModelReady())
      )
    )
    
  })
  
}

