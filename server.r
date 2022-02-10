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
  
  factorCols <- filter(columnValidation, Role == "Factor")$Column
  
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
    rv$data <- read_csv(input$userData$datapath, col_types = cols()) %>%
      distinct() # remove any duplicate rows
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
  
  output$columnDescriptions <- renderTable({
    columnValidation %>%
      select(
        `Default column name` = Column,
        Description = LongDescription,
        `Data type` = TypeDescription,
        Usage
      )
  })
  
  
  
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
      } else {
        
        col <- rv$data[[input[[inputId]]]]
        
        if (anyNA(col)) {
          msg <- list(br(), span("Warning: Missing value in data", style = "color:red"))
        }
        
        colType <- class(col)
        ui <- list(paste("Type:", colType))
        
        if (colType == "numeric") {
          add <- list(br(), paste0("Range: ", round(min(col), 2), " => ", round(max(col), 2)))
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
        
        # check if any messages
        if (is.null(msg)) {
          msg <- list(span(strong("OK"), style = "color:blue"))
          rv$colStatus[[paste0("col", i)]] <- T
        } else {
          msg[1] <- NULL
          rv$colStatus[[paste0("col", i)]] <- F
        }
      }
      
      # display the validation
      fluidRow(
        style = "margin-top: 5px; padding-top: 1em;",
        column(6, msg),
        column(6, ui)
      )
    })
  })
  
  output$colSelectUI <- renderUI({
    ui <- list()
    style <- "border-bottom: 1px solid #cccccc; margin-bottom: 1em;"
    
    for (i in 1:nCols) {
      if (i == nCols) { style <- "" }
      row <- list(
        fluidRow(
          style = style,
          column(6, uiOutput(paste0("colSelect", i))),
          column(6, uiOutput(paste0("colValidate", i)))
        )
      )
      ui <- append(ui, row)
    }
    wellPanel(ui)
  })

  
  # Model readiness ----
  checkModelReadiness <- function(col) {
    compare <- sapply(1:nCols, function(i) {
      test <- (col[i] == T & rv$colStatus[[paste0("col", i)]] == T) | (col[i] == F)
      if (length(test) == 0) {F} else {test}
    })
    !(F %in% compare)
  }
  
  BasicDataReady <- reactive({checkModelReadiness(columnValidation$AllModels)})
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
        "Basic plots and models:", status(BasicDataReady()), br(),
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
  
  
  # Germination Plot and Speed ----
  
  output$germUI <- renderUI({
    validate(
      need(BasicDataReady(), "Please load a dataset and set required column types for germination analysis.")
    )
    list(
      h4("Cumulative germination plot:"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("germPlotTrt1"),
          uiOutput("germPlotTrt2")
        ),
        mainPanel(
          plotOutput("germPlot")
        )
      ),
      br(),
      h4("Germination time analysis: (work in progress)"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("germSpeedTrts"),
          uiOutput("germSpeedFracs"),
          uiOutput("germSpeedType")
        ),
        mainPanel(
          div(
            dataTableOutput("germSpeedTable"),
            style = "overflow-x: auto"
          )
        )
      )
    )
  })
  

  ## Germination plot ----
  
  germTrtChoices <- reactive({
    cols <- sapply(1:nCols, function(i) {
      if (rv$colStatus[[paste0("col", i)]] == T && columnValidation$Role[i] == "Factor") columnValidation$Column[i]
    })
    cols <- compact(cols)
    setNames(as.list(c(NA, cols)), c("Not specified", cols))
  })
  
  output$germPlotTrt1 <- renderUI({
    list(
      selectInput(
        "germPlotTrt1",
        "Treatment 1 (color)",
        choices = germTrtChoices()
      )
    )
  })
  
  output$germPlotTrt2 <- renderUI({
    req(input$germPlotTrt1)
    req(input$germPlotTrt1 != "NA")
    list(
      selectInput(
        "germPlotTrt2",
        "Treatment 2 (shape)",
        choices = germTrtChoices()
      )
    )
  })
  
  output$germPlot <- renderPlot({
    req(nrow(rv$data) > 0)
    req(BasicDataReady())
    
    trts <- 0
    
    df <- tibble(
      TrtID = as.factor(rv$data[[input$TrtID]]),
      CumTime = rv$data[[input$CumTime]],
      CumFrac = rv$data[[input$CumFraction]]
    )
    
    if (req(input$germPlotTrt1) != "NA") {
      df <- mutate(df, Trt1 = as.factor(rv$data[[input$germPlotTrt1]]))
      trts <- 1

      if (req(input$germPlotTrt2) != "NA") {
        df <- mutate(df, Trt2 = as.factor(rv$data[[input$germPlotTrt2]]))
        trts <- 2
      }
    }
    
    df <- df %>%
      group_by(TrtID) %>%
      arrange(TrtID, CumTime, CumFrac) %>%
      mutate(FracDiff = CumFrac - lag(CumFrac, default = 0))
    
    if (trts == 1) {
      
      df <- df %>%
        group_by(Trt1, CumTime) %>%
        summarise(FracDiff = sum(FracDiff), .groups = "drop_last") %>%
        mutate(CumFrac = cumsum(FracDiff) / sum(FracDiff))
      
      plt <- df %>%
        ggplot(aes(x = CumTime, y = CumFrac, color = Trt1)) +
        geom_line() +
        geom_point(shape = 19, size = 2) +
        labs(color = input$germPlotTrt1)
      
    } else if (trts == 2) {
      
      df <- df %>%
        group_by(Trt1, Trt2, CumTime) %>%
        summarise(FracDiff = sum(FracDiff), .groups = "drop_last") %>%
        mutate(CumFrac = cumsum(FracDiff) / sum(FracDiff))
      
      plt <- df %>%
        ggplot(aes(x = CumTime, y = CumFrac, color = Trt1, shape = Trt2)) +
        geom_line() +
        geom_point(size = 2) +
        labs(color = input$germPlotTrt1, shape = input$germPlotTrt2)
      
    } else {
      
      df <- df %>%
        group_by(CumTime) %>%
        summarise(FracDiff = sum(FracDiff), .groups = "drop_last") %>%
        mutate(CumFrac = cumsum(FracDiff) / sum(FracDiff))
      
      plt <- df %>%
        ggplot(aes(x = CumTime, y = CumFrac)) +
        geom_line() +
        geom_point(size = 2)
    } 
    
    plt <- plt +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = "Cumulative germination",
        x = "Time",
        y = "Cumulative (%)"
      ) +
      theme_classic()
    
    # if (req(input$germSpeedFracs)) {
    #   plt <- plt + geom_hline(yintercept = input$germSpeedFracs / 100)
    # }
    
    lines = seq(0, 1, by = input$germSpeedRes / 100)
    plt <- plt + geom_hline(yintercept = lines, color = "grey", size = 0.25, alpha = 0.5, linetype = "dashed")
    
    plt
  })
  
  
  ## Germination speed ----
  
  germSpeedTrtChoices <- reactive({
    cols <- sapply(1:nCols, function(i) {
      if (rv$colStatus[[paste0("col", i)]] == T && columnValidation$Role[i] == "Factor") columnValidation$Column[i]
    })
    cols <- compact(cols)
  })
  
  output$germSpeedTrts <- renderUI({
    checkboxGroupInput(
      inputId = "germSpeedTrtSelect",
      label = "Select all treatment factors:",
      choices = germSpeedTrtChoices(),
      selected = c("TrtID")
    )
  })
  
  output$germSpeedFracs <- renderUI({
    sliderInput(
      inputId = "germSpeedRes",
      label = "Germination fraction step",
      min = 10,
      max = 50,
      step = 5,
      value = 25
    )
  })
  
  output$germSpeedType <- renderUI({
    radioButtons(
      inputId = "germSpeedType",
      label = "Report values as:",
      choiceNames = c("Time (to % germinated)", "Rate (% / time)"),
      choiceValues = c("Time", "Rate")
    )
  })
  
  # TODO: I should have a second reactive dataset that includes only renamed columns from the primary dataset. That would simplify the references to that data.
  
  output$germSpeedTable <- renderDataTable({
    req(input$germSpeedRes)
    req(input$germSpeedType)
    
    # construct working dataset
    df <- tibble(TrtID = rv$data[[input$TrtID]])
    trts <- input$germSpeedTrtSelect
    for (trt in trts) { df[[trt]] <- rv$data[[input[[trt]]]] }
    df <- df %>% mutate(
      CumTime = rv$data[[input$CumTime]],
      CumFrac = rv$data[[input$CumFraction]]
    )
    
    # regenerate cumulative fractions depending on grouping trts
    df <- df %>%
      group_by(TrtID) %>%
      arrange(TrtID, CumTime, CumFrac) %>%
      mutate(FracDiff = CumFrac - lag(CumFrac, default = 0))
    
    # group by the selected treatments
    df <- group_by_at(df, trts)
    
    # merge values that occur at the same timepoint
    df <- df %>%
      group_by(CumTime, .add = T) %>%
      summarise(FracDiff = sum(FracDiff), .groups = "drop_last") %>%
      mutate(CumFrac = cumsum(FracDiff) / sum(FracDiff))
    
    # interpolate the curves to get the estimated value at given fraction
    df <- df %>%
      arrange(CumTime) %>%
      summarise(
        {
          df <- approx(CumFrac, CumTime, xout = seq(0, 1, by = input$germSpeedRes / 100), ties = "ordered", rule = 2)
          names(df) <- c("Frac", "Time")
          df <- as_tibble(df)
          drop_na(df)
        },
        .groups = "drop"
      )
    
    # TODO: I'm not convinced that this rate calculation makes any sense
    if (input$germSpeedType == "Rate") {
      # show as rate
      df <- df %>%
        mutate(
          Time = round(Frac / Time * 100, 2),
          Frac = paste0("GRx", Frac * 100)) %>%
        pivot_wider(
          names_from = "Frac",
          values_from = "Time"
        )
    } else {
      # show as cumulative fraciton
      df <- df %>%
        mutate(
          Frac = paste0("Tx", Frac * 100),
          Time = round(Time, 1)) %>%
        pivot_wider(
          names_from = "Frac",
          values_from = "Time"
        )
    }
    df
  },
    rownames = F,
    server = F,
    extensions = c("Buttons", "Select"),
    selection = "none",
    options = list(
      searching = F,
      paging = F,
      select = T,
      dom = "Bfrtip",
      buttons = list(
        list(
          extend = "copy",
          text = 'Copy'
        )
      )
    )
  )
  
}

