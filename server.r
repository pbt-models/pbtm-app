# ---- Server ---- #

library(tidyverse)
library(shiny)
library(shinyjs)
library(DT)
library(Cairo)
# library(plotly)


server <- function(input, output, session) {
  
  # Data definitions ----
  
  defaultGermSpeedFracs <- c(10, 16, 50, 84, 90)
  
  rv <- reactiveValues(
    data = tibble(),
    colStatus = NULL,
    modelReady = list(),
    germSpeedFracs = defaultGermSpeedFracs
  )
  
  
  
  # Intro tab ----
  
  #### Download buttons ####
  output$downloadTemplate <- downloadHandler(
    filename = "pbtm-data-template.csv",
    content = function(file) {write_csv(sampleTemplate, file)})
  
  output$downloadSampleGermData <- downloadHandler(
    filename = "pbtm-sample-germination-data.csv",
    content = function(file) {write_csv(sampleGermData, file)})
  
  output$downloadSamplePrimingData <- downloadHandler(
    filename = "pbtm-sample-priming-data.csv",
    content = function(file) {write_csv(samplePrimingData, file)})
  
  
  #### output$columnDescriptions ####
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
  
  #### loadMenu ####
  output$loadMenu <- renderMenu({
    if (DataLoaded()) {
      label = "OK"; color = "green"
    } else {
      label = "!"; color = "yellow"
    }
    menuItem("Upload data", tabName = "load", badgeLabel = label, badgeColor = color)
  })
  
  #### dataset load buttons ####
  observeEvent(input$loadSampleGermData, {rv$data <- sampleGermData})
  observeEvent(input$loadSamplePrimingData, {rv$data <- samplePrimingData})
  observeEvent(input$userData, {
    try({
      df <- read_csv(input$userData$datapath, col_types = cols()) %>% distinct()
      if (nrow(df) > 0) {rv$data <- df}
    })
  })
  observeEvent(input$clearData, {
    rv$data <- tibble()
    reset("userData")
  })
  
  #### currentDataTable ####
  output$currentDataTable <- renderDataTable({rv$data})
  
  #### currentDataDisplay ####
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
  
  
  #### columnNames ####
  columnNames <- reactive({
    req(rv$data)
    names(rv$data)
  })
  
  #### columnChoices ####
  columnChoices <- reactive({
    setNames(as.list(c(NA, columnNames())), c("Not specified", columnNames()))
  })
  
  #### colSelect ####
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
  
  
  #### colValidate ####
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

  observe({
    lapply(modelNames, function(m) {
      rv$modelReady[[m]] <- checkModelReadiness(colValidation[[m]])
    })
  })

  
  
  # Germination tab ----
  
  #### output$germMenu ####
  output$germMenu <- renderMenu({
    if (BasicDataReady()) {
      label = "OK"; color = "green"
    } else {
      label = "X"; color = "red"
    }
    menuItem("Germination analysis", tabName = "germTab", badgeLabel = label, badgeColor = color)
  })
  
  #### output$germUI ####
  output$germUI <- renderUI({
    validate(
      need(BasicDataReady(), "Please load a dataset and set required column types for germination analysis.")
    )
    list(
      fluidRow(
        box(
          title = "Cumulative germination plot",
          status = "primary",
          solidHeader = T,
          width = 12,
          sidebarLayout(
            sidebarPanel(
              uiOutput("germPlotTrt1"),
              uiOutput("germPlotTrt2")
            ),
            mainPanel(
              plotOutput("germPlot")
            )
          )
        ),
        box(
          title = "Germination time analysis",
          status = "primary",
          solidHeader = T,
          width = 12,
          sidebarLayout(
            sidebarPanel(
              uiOutput("germSpeedTrts"),
              uiOutput("germSpeedFracs"),
              uiOutput("germSpeedType")
            ),
            mainPanel(
              div(
                style = "overflow-x: auto",
                dataTableOutput("germSpeedTable")
              )
            )
          )
        )
      )
    )
  })
  
  #### germTrtChoices ####
  germTrtChoices <- reactive({
    cols <- sapply(1:nCols, function(i) {
      if (rv$colStatus[[paste0("col", i)]] == T && colValidation$Role[i] == "Factor") colValidation$Column[i]
    })
    cols <- compact(cols)
    setNames(as.list(c(NA, cols)), c("Not specified", cols))
  })
  
  #### output$germPlotTrt1 ####
  output$germPlotTrt1 <- renderUI({
    list(
      selectInput(
        "germPlotTrt1",
        "Treatment 1 (color)",
        choices = germTrtChoices()
      )
    )
  })
  
  #### output$germPlotTrt2 ####
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
  
  #### output$germPlot ####
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
    
    lines = rv$germSpeedFracs / 100
    plt + geom_hline(yintercept = lines, color = "grey", size = 0.25, alpha = 0.5, linetype = "dashed")
  })
  
  
  ## germination speed ----
  
  #### germSpeedTrtChoices ####
  germSpeedTrtChoices <- reactive({
    cols <- sapply(1:nCols, function(i) {
      if (rv$colStatus[[paste0("col", i)]] == T && colValidation$Role[i] == "Factor") colValidation$Column[i]
    })
    cols <- compact(cols)
  })
  
  #### output$germSpeedTrts ####
  output$germSpeedTrts <- renderUI({
    checkboxGroupInput(
      inputId = "germSpeedTrtSelect",
      label = "Select all treatment factors:",
      choices = germSpeedTrtChoices(),
      selected = c("TrtID")
    )
  })
  
  #### output$germSpeedFracs ####
  output$germSpeedFracs <- renderUI({
    list(
      textInput(
        inputId = "addGermSpeedFracs",
        label = "Set cumulative percent (separate with commas):",
        value = ""
      ),
      div(
        actionButton("setGermSpeedFracs", "Apply"),
        actionButton("resetGermSpeedFracs", "Reset"),
        style = "margin-top:15px"
      )
    )
  })
  
  # handle apply button
  observeEvent(input$setGermSpeedFracs, {
    try({
      fracs <- suppressWarnings(sort(parse_number(unlist(strsplit(input$addGermSpeedFracs, ",")))))
      fracs <- unique(as.integer(fracs))
      fracs <- fracs[fracs > 0]
      fracs <- fracs[fracs <= 100]
      if (length(fracs) > 0) {rv$germSpeedFracs <- fracs}
    })
    updateTextInput(inputId = "addGermSpeedFracs", value = "")
  })
  
  # handle reset button
  observeEvent(input$resetGermSpeedFracs, {
    updateTextInput(inputId = "addGermSpeedFracs", value = "")
    rv$germSpeedFracs = defaultGermSpeedFracs
  })
  
  #### output$germSpeedType ####
  output$germSpeedType <- renderUI({
    radioButtons(
      inputId = "germSpeedType",
      label = "Report values as:",
      choiceNames = c("Time (to % germinated)", "Rate (% / time)"),
      choiceValues = c("Time", "Rate")
    )
  })
  
  #### output$germSpeedTable ####
  output$germSpeedTable <- renderDataTable({
    req(DataLoaded())
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
          df <- approx(CumFrac, CumTime, xout = rv$germSpeedFracs / 100, ties = "ordered", rule = 2)
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
  
  lapply(modelNames, function(m) {
    output[[paste0(m, "UI")]] <- renderUI({
      p("Under construction.")
    })
  })
  
  
  # ThermalTime ----

  ## model ----
  TTSubOModelResults <- reactive({
    req(DataLoaded())
    req(rv$modelReady$ThermalTime)
    req(input$TTSubOMaxCumFrac)
    
    temp <- rv$data[[input$GermTemp]]
    time <- rv$data[[input$CumTime]]
    germ <- rv$data[[input$CumFraction]]
    
    max.cum.frac <- input$TTSubOMaxCumFrac
    
    # Calculate Thermaltime Suboptimal Model Parameters- nls plus algorithm port used to add constraints on the parameters
    model <- stats::nls(
      formula = germ ~ max.cum.frac * stats::pnorm(
        log(time, base = 10),
        mean = thetaT50 - log(temp - Tb, base = 10),
        sd = sigma,
        log = FALSE),
      start = list(
        Tb = 6,
        thetaT50 = 3,
        sigma = 0.09),
      lower = list(
        Tb = 0,
        thetaT50 = 0.5,
        sigma = 0.0001),
      upper = list(
        Tb = 15,
        thetaT50 = 50,
        sigma = 0.5),
      algorithm = "port")
    
    # get some estimation of goodness of fit
    Corr <- stats::cor(germ, stats::predict(model)) ^ 2
    
    # passing fitted Hydrotime Model Parameters
    Tb <- summary(model)$coefficients[[1]]
    ThetaT50 <- summary(model)$coefficients[[2]]
    Sigma <- summary(model)$coefficients[[3]]
    
    results <- list(
      Type = "ThermalTime Suboptimal",
      Model = model,
      Plot = NULL,
      MaxCumFrac = max.cum.frac,
      Tb = Tb,
      ThetaT50 = ThetaT50,
      Sigma = Sigma,
      Correlation = Corr
    )
    
    results
  })
  
  
  ## ui ----
  output$ThermalTimeUI <- renderUI({
    validate(need(rv$modelReady$ThermalTime, "Please load required data for ThermalTime analysis."))
    list(
      p(em("The thermal time model assumes a data set with germination temperature as a treatment condition. If you have additional treatments in your dataset, the model will average across those treatments and you may get unreliable or unexpected model results. Note: the model may fail to converge under certain max cumulative fraction values.")),
      br(),
      box(
        title = "Max cumulative fraction",
        sliderInput(
          inputId = "TTSubOMaxCumFrac",
          label = NULL,
          min = 0,
          max = 1,
          value = 1,
          step = 0.05)
      ),
      box(
        title = "Model results",
        tableOutput("TTResults")
      ),
      box(
        width = 12,
        plotOutput("TTPlot")
      )
      
    )
  })
  
  
  ## model results table ----
  output$TTResults <- renderTable({
    req(TTSubOModelResults())
    TTSubOModelResults() %>%
      enframe() %>%
      slice(5:8) %>% # just get the coefficients
      unnest(value) %>%
      rename(
        Parameter = name,
        Value = value
      )
  }, digits = 4)
  
  
  ## plot ----
  output$TTPlot <- renderPlot({
    req(DataLoaded())
    
    # generate the plot
    plt <- rv$data %>%
      ggplot(aes(
        x = .data[[input$CumTime]],
        y = .data[[input$CumFraction]],
        color = as.factor(.data[[input$GermTemp]]))) +
      annotate("rect", xmin = 0, xmax = Inf, ymin = input$TTSubOMaxCumFrac, ymax = 1, fill = "grey", alpha = 0.1) +
      geom_hline(yintercept = input$TTSubOMaxCumFrac, color = "darkgrey", linetype = "dashed") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, 1.02)) +
      scale_x_continuous(expand = c(0, 0)) +
      expand_limits(x = 0, y = 0) +
      labs(
        title = "Thermal Time Sub-optimal Model",
        x = "Time",
        y = "Cumulative fraction germinated (%)",
        color = "Temperature") +
      guides(color = guide_legend(reverse = T, order = 1)) +
      theme_classic()
    
    # try model so it will still plot on model error
    try({
      req(TTSubOModelResults())
      model <- TTSubOModelResults()
      
      maxCumFrac <- model$MaxCumFrac
      tb <- model$Tb
      thetaT50 <- model$ThetaT50
      sigma <- model$Sigma
      corr <- model$Correlation
      
      par1 <- paste("T[b] ==", round(tb, 1))
      par2 <- paste("ThetaT(50) ==", round(thetaT50, 3))
      par3 <- paste("sigma ==", round(sigma, 3))
      par4 <- paste("R^2 ==", round(corr, 2))
      
      # Plot all predicted treatments by the thermal time model
      df <- rv$data %>% distinct(.data[[input$GermTemp]], .keep_all = FALSE)
      modelLines <- mapply(function(temp) {
        stat_function(
          fun = function(x) {
            stats::pnorm(log(x, base = 10), thetaT50 - log(temp - tb, base = 10),  sigma, log = FALSE)
          },
          aes(color = as.factor(temp))
        )
      },
        df[[input$GermTemp]]
      )
      
      plt <- plt +
        modelLines +
        annotate("text", x = -Inf, y = 0.95, label = paste("Model Parameters"), color = "grey0", hjust = -0.1) +
        annotate("text", x = -Inf, y = 0.9, label = par1, color = "grey0", hjust = -0.2, parse = TRUE) +
        annotate("text", x = -Inf, y = 0.85, label = par2, color = "grey0", hjust = -0.1, parse = TRUE) +
        annotate("text", x = -Inf, y = 0.8, label = par3, color = "grey0", hjust = -0.2, parse = TRUE) +
        annotate("text", x = -Inf, y = 0.75, label = par4, color = "grey0", hjust = -0.2, parse = TRUE)
    })
    
    plt
  })

  
  
  
  # HydroTime model ----

  
  
  # HydroThermalTime ----

  
  
  # HydroPriming model ----
  
  
  
  # HydroThermalPriming model ----
  
  
  
  # Aging model ----
  
  
  
  # Promoter model ----
  
  
  
  # Inhibitor model ----
  
  
  
}

