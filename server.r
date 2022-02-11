# ---- Server ---- #

library(tidyverse)
library(shiny)
library(shinyjs)
library(DT)
# library(plotly)


server <- function(input, output, session) {
  
  # Data definitions ----
  
  rv <- reactiveValues(
    data = tibble(),
    colStatus = NULL,
    modelReady = list()
  )
  
  
  # Action Buttons ----
  


  observeEvent(input$confirmDataView, {
    updateCollapse(session, "data", open = "cols", close = "view")
  })
  observeEvent(c(input$confirmColView, input$confirmColView2), {
    updateCollapse(session, "data", close = "cols")
  })
  
  
  
  # Format tab ----
  
  ## download buttons ----
  output$downloadTemplate <- downloadHandler(
    filename = "pbtm-data-template.csv",
    content = function(file) {write_csv(sampleTemplate, file)})
  
  output$downloadSampleGermData <- downloadHandler(
    filename = "pbtm-sample-germination-data.csv",
    content = function(file) {write_csv(sampleGermData, file)})
  
  output$downloadSamplePrimingData <- downloadHandler(
    filename = "pbtm-sample-priming-data.csv",
    content = function(file) {write_csv(samplePrimingData, file)})
  
  
  ## data format description table ----
  output$columnDescriptions <- renderTable({
    colValidation %>%
      select(
        `Default column name` = Column,
        Description = LongDescription,
        `Data type` = TypeDescription,
        Usage
      )
  })
  
  
  
  
  
  # Load tab ----
  
  ## menu badge handler ----
  output$loadMenu <- renderMenu({
    if (DataLoaded()) {
      label = "OK"; color = "green"
    } else {
      label = "!"; color = "yellow"
    }
    menuItem("Upload data", tabName = "load", badgeLabel = label, badgeColor = color)
  })
  
  ## action buttons ----
  observeEvent(input$loadSampleGermData, {
    rv$data <- sampleGermData
    # updateCollapse(session, "data", open = "view", close = "load")
  })
  
  observeEvent(input$loadSamplePrimingData, {
    rv$data <- samplePrimingData
    # updateCollapse(session, "data", open = "view", close = "load")
  })
  
  observeEvent(input$userData, {
    rv$data <- read_csv(input$userData$datapath, col_types = cols()) %>%
      distinct() # remove any duplicate rows
    # updateCollapse(session, "data", open = "view", close = "load")
  })
  
  observeEvent(input$clearData, {
    rv$data <- tibble()
    reset("userData")
  })
  
  
  ## current data display ----
  
  # the table
  output$currentDataTable <- renderDataTable({rv$data})
  
  # the ui, which shows the table and the matching boxes only when data loaded
  output$currentDataDisplay <- renderUI({
    validate(need(DataLoaded(), "Please load a dataset."))
    
    list(
      h3("Current dataset:"),
      div(style = "overflow: auto;", dataTableOutput("currentDataTable")),
      hr(),
      h3("Match column names to expected roles:"),
      p(em("If you used the same column names as the default data template, they will be automatically matched below. Otherwise, cast your column names into the appropriate data types. Warning messages will appear if your data doesn't match the expected type or range.")),
      div(style = "display: flex; flex-wrap: wrap;",
        lapply(1:nCols, function(i) {
          wellPanel(
            style = "flex: 1; vertical-align: top; min-width: 15em; margin: 5px;",
            uiOutput(paste0("colSelect", i)),
            uiOutput(paste0("colValidate", i))
          )
        })
      )
    )
  })
  
  
  ## column matching ----
  
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
        inputId = colValidation$InputId[i],
        label = colValidation$Description[i],
        choices = columnChoices(),
        selected = colValidation$Column[i]
      )
    })
  })
  
  
  ## column validation messages ----
  
  lapply(1:nCols, function(i) {
    
    outCol <- paste0("colValidate", i)
    inputId <- colValidation$InputId[i]
    expectedType <- colValidation$Type[i]
    minValue <- colValidation$Min[i]
    maxValue <- colValidation$Max[i]
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
        
        # set status TRUE if no messages
        if (is.null(msg)) {
          msg <- list(span(strong("OK"), style = "color:blue"))
          rv$colStatus[[paste0("col", i)]] <- T
        } else {
          msg[1] <- NULL # remove the first br()
          rv$colStatus[[paste0("col", i)]] <- F
        }
      }
      
      # display the validation
      p(ui, br(), msg)
    })
  })
  

  # Model readiness ----
  
  # returns false if colStatus is false for that column
  checkModelReadiness <- function(col) {
    compare <- sapply(1:nCols, function(i) {
      test <- (col[i] == T & rv$colStatus[[paste0("col", i)]] == T) | (col[i] == F)
      if (length(test) == 0) {F} else {test}
    })
    !(F %in% compare)
  }
  
  DataLoaded <- reactive({nrow(rv$data) > 0})
  BasicDataReady <- reactive({checkModelReadiness(colValidation$AllModels)})
  TTModelReady <- reactive({checkModelReadiness(colValidation$ThermalTime)})
  HTModelReady <- reactive({checkModelReadiness(colValidation$HydroTime)})
  HTTModelReady <- reactive({checkModelReadiness(colValidation$HydroThermalTime)})
  HPModelReady <- reactive({checkModelReadiness(colValidation$HydroPriming)})
  HTPModelReady <- reactive({checkModelReadiness(colValidation$HydroThermalPriming)})
  AgingModelReady <- reactive({checkModelReadiness(colValidation$Aging)})
  PromoterModelReady <- reactive({checkModelReadiness(colValidation$Promoter)})
  InhibitorModelReady <- reactive({checkModelReadiness(colValidation$Inhibitor)})
  
  # lapply(modelNames, function(m) {
  #   rv$modelReady[[m]] <- reactive({checkModelReadiness(colValidation[[m]])})
  # })
  
  observe({
    lapply(modelNames, function(m) {
      rv$modelReady[[m]] <- checkModelReadiness(colValidation[[m]])
    })
  })
  
  
  
  # Model readiness display ----
  # output$modelStatus <- renderUI({
  #   
  #   status <- function(x) {
  #     if (x) {
  #       span(strong("Ready"), style = "color:blue")
  #     } else {
  #       span(strong("Not ready"), "- required columns missing", style = "color:red")
  #     }
  #   }
  #   
  #   list(
  #     p(
  #       "Basic plots and models:", status(BasicDataReady()), br(),
  #       "Hydro Priming model:", status(HPModelReady()), br(),
  #       "Hydro Thermal Priming model:", status(HTPModelReady()), br(),
  #       "Hydro Time model:", status(HTModelReady()), br(),
  #       "Thermal Time model:", status(TTModelReady()), br(),
  #       "Hydro Thermal Time model:", status(HTTModelReady()), br(),
  #       "Aging model:", status(HTTModelReady()), br(),
  #       "Promoter model:", status(HTTModelReady()), br(),
  #       "Inhibitor model:", status(HTTModelReady())
  #     )
  #   )
  #   
  # })
  
  # Menus ----
  

  
  
  # Germination Plot and Speed ----
  
  output$germMenu <- renderMenu({
    if (BasicDataReady()) {
      label = "OK"; color = "green"
    } else {
      label = "X"; color = "red"
    }
    menuItem("Germination analysis", tabName = "germTab", badgeLabel = label, badgeColor = color)
  })
  
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
  

  ## germination plot ----
  
  germTrtChoices <- reactive({
    cols <- sapply(1:nCols, function(i) {
      if (rv$colStatus[[paste0("col", i)]] == T && colValidation$Role[i] == "Factor") colValidation$Column[i]
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
    validate(
      need(DataLoaded(), "No data loaded."),
      need(BasicDataReady(), "Necessary data columns not present.")
    )
    
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
  
  
  ## germination speed ----
  
  germSpeedTrtChoices <- reactive({
    cols <- sapply(1:nCols, function(i) {
      if (rv$colStatus[[paste0("col", i)]] == T && colValidation$Role[i] == "Factor") colValidation$Column[i]
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
  
  
  output$germSpeedTable <- renderDataTable({
    req(DataLoaded())
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
    
    if (input$germSpeedType == "Rate") {
      # show as rate
      df <- df %>%
        mutate(
          Time = round(1 / Time, 6),
          Frac = paste0("GR", Frac * 100)) %>%
        pivot_wider(
          names_from = "Frac",
          values_from = "Time"
        )
    } else {
      # show as cumulative fraction
      df <- df %>%
        mutate(
          Frac = paste0("T", Frac * 100),
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
          text = 'Copy table to clipboard'
        )
      )
    )
  )
  
  
  # Render menus ----
  
  lapply(modelNames, function(m) {
    output[[paste0(m, "Menu")]] <- renderMenu({
      if (rv$modelReady[[m]]) {label = "OK"; color = "green"} else {label = "X"; color = "red"}
      menuItem(m, tabName = paste0(m, "Tab"), badgeLabel = label, badgeColor = color)
    })
  })
  
  
  # ThermalTime ----
  output$ttMenu <- renderMenu({
    if (TTModelReady()) {label = "OK"; color = "green"} else {label = "X"; color = "red"}
    menuItem("ThermalTime", tabName = "ttTab", badgeLabel = label, badgeColor = color)
  })
  
  output$ttUI <- renderUI({
    p("Under construction.")
  })
  
  
  
  # HydroTime model ----
  output$htMenu <- renderMenu({
    if (HTModelReady()) {label = "OK"; color = "green"} else {label = "X"; color = "red"}
    menuItem("HydroTime", tabName = "htTab", badgeLabel = label, badgeColor = color)
  })
  
  output$htUI <- renderUI({
    p("Under construction.")
  })
  
  
  # HydroThermalTime ----
  output$httMenu <- renderMenu({
    if (HTTModelReady()) {label = "OK"; color = "green"} else {label = "X"; color = "red"}
    menuItem("HydroThermalTime", tabName = "httTab", badgeLabel = label, badgeColor = color)
  })
  
  output$httUI <- renderUI({
    p("Under construction.")
  })
  
  
  # HydroPriming model ----
  
  
  
  # HydroThermalPriming model ----
  
  
  
  # Aging model ----
  
  
  
  # Promoter model ----
  
  
  
  # Inhibitor model ----
  
  
  
}

