# ---- Server ---- #

library(tidyverse)
library(shiny)
library(shinyjs)
library(DT)
# library(plotly)


server <- function(input, output, session) {
  
  # On load ----
  
  # this forces the bscollapse panels to load their content by starting open then closing
  updateCollapse(session, "data", close = c("view", "cols"))
  
  
  
  # Data definitions ----
  
  nCols <- nrow(columnValidation)
  
  rv <- reactiveValues(
    data = tibble(),
    colStatus = NULL
  )
  

  
  
  # Action Buttons ----
  
  observeEvent(input$loadSampleGermData, {
    rv$data <- sampleGermData
    updateCollapse(session, "data", open = "view", close = "load")
  })
  observeEvent(input$loadSamplePrimingData, {
    rv$data <- samplePrimingData
    updateCollapse(session, "data", open = "view", close = "load")
  })
  observeEvent(input$userData, {
    rv$data <- read_csv(input$userData$datapath, col_types = cols())
    updateCollapse(session, "data", open = "view", close = "load")
  })
  observeEvent(input$clearData, {
    rv$data <- tibble()
    reset("userData")
  })
  observeEvent(input$confirmDataView, {
    updateCollapse(session, "data", open = "cols", close = "view")
  })
  observeEvent(c(input$confirmColView, input$confirmColView2), {
    updateCollapse(session, "data", close = "cols")
  })
  
  
  
  # Download button handlers ----
  
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
  
  
  
  # Column description table ----
  
  output$columnDescriptions <- renderTable({columnDescriptions})
  
  
  
  # Current data DT ----
  
  output$currentDataTable <- renderDataTable({rv$data})
  
  output$currentDataDisplay <- renderUI({
    if (nrow(rv$data) == 0) {
      return("No data loaded.")
    } else {
      dataTableOutput("currentDataTable")
    }
  })
  

  # Column matching boxes ----
  
  columnNames <- reactive({
    req(rv$data)
    names(rv$data)
  })
  
  columnChoices <- reactive({
    setNames(as.list(c(NA, columnNames())), c("Not specified", columnNames()))
  })
  
  lapply(1:nCols, function(i) {
    output[[paste0("colSelect", i)]] <- renderUI({
      selectInput(
        inputId = columnValidation$InputId[i],
        label = columnValidation$Description[i],
        choices = columnChoices(),
        selected = columnValidation$Column[i]
      )
    })
  })
  
  
  
  # Column validation messages ----
  
  lapply(1:nCols, function(i) {
    
    outCol <- paste0("colValidate", i)
    inputId <- columnValidation$InputId[i]
    expectedType <- columnValidation$Type[i]
    minValue <- columnValidation$Min[i]
    maxValue <- columnValidation$Max[i]
    ui <- NULL
    msg <- NULL
    
    output[[outCol]] <- renderUI({
      
      req(input[[inputId]])
      
      # check if no column selected
      if (input[[inputId]] == "NA") {
        msg <- list(span("No column specified.", style = "color:orange"))
        rv$colStatus[[paste0("col", i)]] <- F
        return(msg)
      }
      
      col <- rv$data[[input[[inputId]]]]
      
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
        rv$colStatus[[paste0("col", i)]] <- T
        
      } else {
        rv$colStatus[[paste0("col", i)]] <- F
      }
      
      list(p(ui), p(msg))
      
    })
  })
  
  # lapply(1:nrow(columnDefaults), function(i) {
  #   observe({
  #     rv$colMatch[[i]] <- input[[paste0("colSelect", i)]]
  #     print(rv$colMatch[[i]])
  #   })
  # })
  
  
  # cleanData <- reactive({
  #   dfIn <- rv$data
  #   dfOut <- sampleTemplate
  #   lapply(1:nrow(columnDefaults), function(i) {
  #     inCol <- paste0("colSelect", i)
  #     req(input[[inCol]])
  #     if (input[[inCol]] != "NA") {
  #       dfOut[[columnDefaults$defaultColumn[i]]] <- dfIn[[]]
  #     } else {
  #       dfOut <- select(dfOut, -columnDefaults$defaultColumn[i])
  #     }
  #   })
  # })
  # 
  # observe({print(cleanData())})
  
  
  
  # Model readiness ----
  checkModelReadiness <- function(col) {
    compare <- sapply(1:nCols, function(i) {
      test <- (col[i] == T & rv$colStatus[[paste0("col", i)]] == T) | (col[i] == F)
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
  
  
  # Plot drafts ----
  output$PlotRawDt <- renderPlot({
    req(nrow(rv$data) > 0)
    req(TTModelReady())
    df <- tibble(
      TrtId = as.factor(rv$data[[input$TrtId]]),
      GermWP = as.factor(rv$data[[input$GermWP]]),
      GermTemp = as.factor(rv$data[[input$GermTemp]]),
      CumTime = rv$data[[input$CumTime]],
      CumFrac = rv$data[[input$CumFraction]]
    )
    
    df %>%
      ggplot(aes(x = CumTime, y = CumFrac, color = GermWP, shape = GermTemp)) +
      geom_line() +
      geom_point(size = 2) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        x = "Time",
        y = "Cumulative (%)",
        title = "Cumulative germination"
      ) +
      theme_classic()
      
      
    
    # # from pbtm
    # PlotRawDt <- function(df, t1, t2) {
    #   gp <- "geom_point(shape=19, size=2)"
    #     gp <- "geom_point(size=2)"
    #     eval(parse(text = paste("TreatData$", Treat2, " <- (factor(TreatData$",
    #       Treat2, "))", sep = "")))
    #     T2 <- Treat2
    #   df %>%
    #     ggplot(data = TreatData, aes_string(x = "CumTime",
    #     y = "CumFraction", color = T1, shape = T2)) + eval(parse(text = gp)) +
    #     geom_line() + xlab("Time") + ylab("Cumulative (%)") +
    #     scale_y_continuous(labels = scales::percent, expand = c(0,
    #       0), limits = c(0, 1.02)) + scale_x_continuous(expand = c(0,
    #         0)) + expand_limits(x = 0, y = 0)
    # }
    
  })
  
  output$PlotRateVsTreat <- renderPlot({
    req(nrow(rv$data) > 0)
    req(TTModelReady())
    df <- rv$data
    gr <- input$GRInput / 100
    t1 <- input[[paste0("colSelect", 7)]] # germ wp
    t2 <- input[[paste0("colSelect", 8)]] # germ temp
    try(speed <- CalcSpeed(df, gr, t1, t2))
    try(PlotRateVsTreat(speed, t2, paste0("GR", gr * 100)))
  })
  
}

