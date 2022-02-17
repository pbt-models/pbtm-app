# ---- Server ---- #

suppressPackageStartupMessages({
  library(tidyverse)
  library(shiny)
  library(shinyjs)
  library(DT)
  library(Cairo)
  # library(plotly)
})

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
        rv$colStatus[i] <- F
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
          rv$colStatus[i] <- T
        } else {
          msg[1] <- NULL # remove the first br()
          rv$colStatus[i] <- F
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
      test <- (col[i] == T & rv$colStatus[i] == T) | (col[i] == F)
      if (length(test) == 0) {F} else {test}
    })
    !(F %in% compare)
  }
  
  # reactive readiness checks
  DataLoaded <- reactive({nrow(rv$data) > 0})
  BasicDataReady <- reactive({checkModelReadiness(colValidation$AllModels)})

  # set reactive values for each model's readiness
  observe({
    lapply(modelNames, function(m) {
      rv$modelReady[[m]] <- checkModelReadiness(colValidation[[m]])
    })
  })
  
  #### Model menu items ####
  lapply(modelNames, function(m) {
    output[[paste0(m, "Menu")]] <- renderMenu({
      if (rv$modelReady[[m]]) {label = "OK"; color = "green"} else {label = "X"; color = "red"}
      menuItem(m, tabName = paste0(m, "Tab"), badgeLabel = label, badgeColor = color)
    })
  })
  
  #### Model UI placeholders ####
  lapply(modelNames, function(m) {
    output[[paste0(m, "UI")]] <- renderUI({
      p("Under construction.")
    })
  })

  
  
  # Germination tab ----
  
  #### germMenu ####
  output$germMenu <- renderMenu({
    if (BasicDataReady()) {
      label = "OK"; color = "green"
    } else {
      label = "X"; color = "red"
    }
    menuItem("Germination analysis", tabName = "germTab", badgeLabel = label, badgeColor = color)
  })
  
  #### germUI ####
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
              uiOutput("germSpeedTrtsUI"),
              uiOutput("germSpeedFracsUI"),
              uiOutput("germSpeedTypeUI")
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
      if (rv$colStatus[i] == T && colValidation$Role[i] == "Factor") colValidation$Column[i]
    })
    cols <- compact(cols)
    setNames(as.list(c(NA, cols)), c("Not specified", cols))
  })
  
  #### germPlotTrt1 ####
  output$germPlotTrt1 <- renderUI({
    list(
      selectInput(
        "germPlotTrt1",
        "Treatment 1 (color)",
        choices = germTrtChoices()
      )
    )
  })
  
  #### germPlotTrt2 ####
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
  
  #### germPlot ####
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
  
  #### germSpeedTrtChoices ####
  germSpeedTrtChoices <- reactive({
    cols <- sapply(1:nCols, function(i) {
      if (rv$colStatus[i] == T && colValidation$Role[i] == "Factor") colValidation$Column[i]
    })
    cols <- compact(cols)
  })
  
  #### germSpeedTrts ####
  output$germSpeedTrtsUI <- renderUI({
    checkboxGroupInput(
      inputId = "germSpeedTrtSelect",
      label = "Select all treatment factors:",
      choices = germSpeedTrtChoices(),
      selected = c("TrtID")
    )
  })
  
  #### germSpeedFracsUI ####
  output$germSpeedFracsUI <- renderUI({
    list(
      textInput(
        inputId = "addGermSpeedFracs",
        label = "Set cumulative percent (separate with commas):",
        value = ""
      ),
      div(
        style = "margin-top: 1em; margin-bottom: 1em;",
        actionButton("setGermSpeedFracs", "Apply"),
        actionButton("resetGermSpeedFracs", "Reset")
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
  
  #### germSpeedTypeUI ####
  output$germSpeedTypeUI <- renderUI({
    radioButtons(
      inputId = "germSpeedType",
      label = "Report values as:",
      choiceNames = c("Time (to % germinated)", "Rate (1 / time)"),
      choiceValues = c("Time", "Rate")
    )
  })
  
  #### germSpeedTable ####
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
          approx(CumFrac, CumTime, xout = rv$germSpeedFracs / 100, ties = "ordered", rule = 2) %>%
            setNames(c("Frac", "Time")) %>%
            as_tibble() %>%
            drop_na()
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
  
  

  # ThermalTime ----
  
  #### Model params ####
  
  TT.start <- list(
    Tb = 6,
    ThetaT50 = 3,
    Sigma = 0.09)
  
  TT.min <- list(
    Tb = 0,
    ThetaT50 = 0.5,
    Sigma = 0)
  
  TT.max <- list(
    Tb = 15,
    ThetaT50 = 50,
    Sigma = 0.5)

  #### ThermalTimeUI ####
  output$ThermalTimeUI <- renderUI({
    validate(need(rv$modelReady$ThermalTime, "Please load required data for ThermalTime analysis."))
    list(
      p(em("The thermal time model assumes a data set with germination temperature as a treatment condition. If you have additional treatments in your dataset, the model will average across those treatments and you may get unreliable or unexpected model results. Input any of the following model coefficients, or leave blank to compute the best fit. If the model fails, try changing or unsetting parameters.")),
      br(),
      fluidRow(
        box(
          title = "Max cumulative fraction",
          status = "primary",
          sliderInput(
            inputId = "TT.maxCumFrac",
            label = NULL,
            min = 0,
            max = 1,
            value = 1,
            step = 0.01),
          lapply(names(TT.start), function(c) {
            min <- TT.min[[c]]
            max <- TT.max[[c]]
            step <- signif((max - min) / 100, 2)
            numericInput(
              inputId = paste0("TT.", c),
              label = paste0(c, " (range: ", min, " - ", max, ")"),
              value = NULL,
              min = min,
              max = max,
              step = step
            )
          })
        ),
        box(
          title = "Model results",
          status = "primary",
          tableOutput("TTResultsTable")
        )
      ),
      fluidRow(
        box(
          width = 12,
          status = "primary",
          plotOutput("TTPlot")
        )
      )
    )
  })
  
  #### TTSubOModelResults ####
  TTSubOModelResults <- reactive({
    req(DataLoaded())
    req(rv$modelReady$ThermalTime)
    
    # get data
    GermTemp <- rv$data[[input$GermTemp]]
    CumTime <- rv$data[[input$CumTime]]
    CumFrac <- rv$data[[input$CumFraction]]
    
    # get params
    maxCumFrac <- input$TT.maxCumFrac
    coefNames <- names(TT.start)
    userCoefs <- list()
    start <- TT.start
    lower <- TT.min
    upper <- TT.max
    
    for (c in coefNames) {
      userVal <- input[[paste0("TT.", c)]]
      if (!is.na(userVal)) {
        if (between(userVal, TT.min[[c]], TT.max[[c]])) {
          start[[c]] <- lower[[c]] <- upper[[c]] <- NULL
          userCoefs[[c]] <- userVal
          assign(c, userVal)
        } else {
          updateTextInput(inputId = paste0("TT.", c), value = "")
        }
      }
    }
    
    tryCatch({
      # run model
      model <- stats::nls(
        CumFrac ~ maxCumFrac * stats::pnorm(
          log10(CumTime),
          mean = ThetaT50 - log10(GermTemp - Tb),
          sd = Sigma,
          log = FALSE),
        start = start,
        lower = lower,
        upper = upper,
        algorithm = "port")
      
      # get correlation
      corr <- stats::cor(CumFrac, stats::predict(model)) ^ 2
      
      # at least one coef generated by the model
      if (length(userCoefs) < length(coefNames)) {
        coefs <- summary(model)$coefficients %>%
          as_tibble(rownames = "Parameter") %>%
          pull(Estimate, Parameter) %>%
          as.list()
        
        # merge model coefs and user coefs
        for (c in coefNames) {
          if (is.null(coefs[[c]])) {
            coefs[[c]] <- userCoefs[[c]]
          }
        }
      } else {
        # user specified all coefs
        coefs <- userCoefs
      }
      
      # return results
      list(
        Tb = coefs$Tb,
        ThetaT50 = coefs$ThetaT50,
        Sigma = coefs$Sigma,
        Correlation = corr
      )
    },
      error = function(e) {
        paste("Unable to compute model, try adjusting parameters. ", str_to_sentence(e[1]))
      }
    )
  })
  
  #### TTResultsTable ####
  output$TTResultsTable <- renderTable({
    req(TTSubOModelResults())
    results <- TTSubOModelResults()
    
    # print error message if model fails
    validate(need(is.list(results), results))
    
    # convert results list to data frame
    results %>%
      enframe() %>%
      unnest(value) %>%
      rename(
        Parameter = name,
        Value = value
      )
  }, digits = 4)
  
  
  #### TTPlot ####
  output$TTPlot <- renderPlot({
    req(DataLoaded())
    req(input$TT.maxCumFrac)
    
    maxCumFrac <- input$TT.maxCumFrac
    
    # generate the plot
    plt <- rv$data %>%
      ggplot(aes(
        x = .data[[input$CumTime]],
        y = .data[[input$CumFraction]],
        color = as.factor(.data[[input$GermTemp]]))) +
      annotate("rect", xmin = 0, xmax = Inf, ymin = maxCumFrac, ymax = 1, fill = "grey", alpha = 0.1) +
      geom_hline(yintercept = maxCumFrac, color = "darkgrey", linetype = "dashed") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, 1.02)) +
      scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks()) +
      expand_limits(x = 0, y = 0) +
      labs(
        title = "Thermal Time Sub-optimal Model",
        x = "Time",
        y = "Cumulative fraction germinated (%)",
        color = "Temperature") +
      guides(color = guide_legend(reverse = T, order = 1)) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(linetype = "dotted")
      )
    
    # use try so it will still plot on model error
    try({
      req(is.list(TTSubOModelResults()))
      model <- TTSubOModelResults()
      
      Tb <- model$Tb
      ThetaT50 <- model$ThetaT50
      Sigma <- model$Sigma
      Corr <- model$Correlation
      
      par1 <- paste("~~T[b]==", signif(Tb, 3))
      par2 <- paste("~~Theta[T](50)==", signif(ThetaT50, 3))
      par3 <- paste("~~sigma==", signif(Sigma, 3))
      par4 <- paste("~~R^2==", signif(Corr, 2))
      
      # Plot all predicted treatments by the thermal time model
      df <- rv$data %>% distinct(.data[[input$GermTemp]], .keep_all = F)
      modelLines <- mapply(function(temp) {
        stat_function(
          fun = function(x) {
            maxCumFrac * stats::pnorm(log10(x), ThetaT50 - log10(temp - Tb), Sigma, log = F)
          },
          aes(color = as.factor(temp))
        )
      },
        df[[input$GermTemp]]
      )
      
      plt <- plt +
        modelLines +
        annotate("text", x = -Inf, y = 0.95, label = " Model parameters:", color = "grey0", hjust = 0) +
        annotate("text", x = -Inf, y = 0.9, label = par1, color = "grey0", hjust = 0, parse = T) +
        annotate("text", x = -Inf, y = 0.85, label = par2, color = "grey0", hjust = 0, parse = T) +
        annotate("text", x = -Inf, y = 0.8, label = par3, color = "grey0", hjust = 0, parse = T) +
        annotate("text", x = -Inf, y = 0.75, label = par4, color = "grey0", hjust = 0, parse = T)
    })
    
    plt
  })

  
  
  # HydroTime model ----
  
  #### Model params ####
  
  HT.start <- list(
    HT = 60,
    Psib50 = -0.8,
    Sigma = 0.2)
  
  HT.min <- list(
    HT = 0,
    Psib50 = -5,
    Sigma = 0)
  
  HT.max <- list(
    HT = 1000,
    Psib50 = 0,
    Sigma = 2)

  #### HydroTimeUI ####
  output$HydroTimeUI <- renderUI({
    validate(need(rv$modelReady$HydroTime, "Please load required data for HydroTime analysis."))
    list(
      p(em("The hydro time model assumes a data set with germination temperature as a treatment condition. If you have additional treatments in your dataset, the model will average across those treatments and you may get unreliable or unexpected model results. Input any of the following model coefficients, or leave blank to compute the best fit. If the model fails, try changing or unsetting parameters.")),
      br(),
      fluidRow(
        box(
          title = "Max cumulative fraction",
          status = "primary",
          sliderInput(
            inputId = "HT.maxCumFrac",
            label = NULL,
            min = 0,
            max = 1,
            value = 1,
            step = 0.01
          ),
          lapply(names(HT.start), function(c) {
            min <- HT.min[[c]]
            max <- HT.max[[c]]
            step <- signif((max - min) / 100, 2)
            numericInput(
              inputId = paste0("HT.", c),
              label = paste0(c, " (range: ", min, " - ", max, ")"),
              value = NULL,
              min = min,
              max = max,
              step = step
            )
          })
        ),
        box(
          title = "Model results",
          status = "primary",
          tableOutput("HTResultsTable")
        )
      ),
      fluidRow(
        box(
          width = 12,
          plotOutput("HTPlot")
        )
      )
    )
  })
  
  #### HTModelResults ####
  HTModelResults <- reactive({
    req(DataLoaded())
    req(rv$modelReady$HydroTime)
    
    # get data
    GermWP <- rv$data[[input$GermWP]]
    CumTime <- rv$data[[input$CumTime]]
    CumFrac <- rv$data[[input$CumFraction]]
    
    # get params
    maxCumFrac <- input$HT.maxCumFrac
    coefNames <- names(HT.start)
    userCoefs <- list()
    start <- HT.start
    lower <- HT.min
    upper <- HT.max
    
    # handle user defined or fitted model params
    for (c in coefNames) {
      userVal <- input[[paste0("HT.", c)]]
      if (!is.na(userVal)) {
        if (between(userVal, HT.min[[c]], HT.max[[c]])) {
          start[[c]] <- lower[[c]] <- upper[[c]] <- NULL
          userCoefs[[c]] <- userVal
          assign(c, userVal)
        } else {
          updateTextInput(inputId = paste0("HT.", c), value = "")
        }
      }
    }
    
    # run model
    tryCatch({
      model <- stats::nls(
        CumFrac ~ maxCumFrac * stats::pnorm(
          GermWP - (HT / CumTime),
          mean = Psib50,
          sd = Sigma,
          log = F),
        start = start,
        lower = lower,
        upper = upper,
        algorithm = "port")
      
      # get correlation
      Corr <- stats::cor(CumFrac, stats::predict(model)) ^ 2
      
      # at least one coef generated by the model
      if (length(userCoefs) < length(coefNames)) {
        coefs <- summary(model)$coefficients %>%
          as_tibble(rownames = "Parameter") %>%
          pull(Estimate, Parameter) %>%
          as.list()
        
        # merge model coefs and user coefs
        for (c in coefNames) {
          if (is.null(coefs[[c]])) {
            coefs[[c]] <- userCoefs[[c]]
          }
        }
      } else {
        # user specified all coefs
        coefs <- userCoefs
      }
      
      # return results
      list(
        HT = coefs$HT,
        Psib50 = coefs$Psib50,
        Sigma = coefs$Sigma,
        Correlation = Corr)
    },
      error = function(e) {
        paste("Unable to compute model, try adjusting parameters. ", str_to_sentence(e[1]))
      }
    )
  })
  
  #### HTResultsTable ####
  output$HTResultsTable <- renderTable({
    req(HTModelResults())
    results <- HTModelResults()
    
    # print error message if model fails
    validate(need(is.list(results), results))
    
    # convert list to simple data frame
    results %>%
      enframe() %>%
      unnest(value) %>%
      rename(
        Parameter = name,
        Value = value
      )
  }, digits = 4)
  
  #### HTPlot ####
  output$HTPlot <- renderPlot({
    req(DataLoaded())
    req(input$HT.maxCumFrac)
    
    maxCumFrac <- input$HT.maxCumFrac
    
    # generate the plot
    plt <- rv$data %>%
      ggplot(aes(
        x = .data[[input$CumTime]],
        y = .data[[input$CumFraction]],
        color = as.factor(.data[[input$GermWP]]))) +
      annotate("rect", xmin = 0, xmax = Inf, ymin = maxCumFrac, ymax = 1, fill = "grey", alpha = 0.1) +
      geom_hline(yintercept = maxCumFrac, color = "darkgrey", linetype = "dashed") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, 1.02)) +
      scale_x_continuous(expand = c(0, 0)) +
      expand_limits(x = 0, y = 0) +
      labs(
        title = "Hydro Time Model",
        x = "Time",
        y = "Cumulative fraction germinated (%)",
        color = "Water potential") +
      guides(color = guide_legend(reverse = T, order = 1)) +
      theme_classic()
    
    # use try so it will still plot on model error
    try({
      req(is.list(HTModelResults()))
      model <- HTModelResults()
      
      Ht <- model$HT
      Psib50 <- model$Psib50
      Sigma <- model$Sigma
      Corr <- model$Correlation
      
      par1 <- paste("~~HT==", round(Ht, 2))
      par2 <- paste("~~Psi[b](50)==", round(Psib50, 3))
      par3 <- paste("~~sigma== ", round(Sigma, 3))
      par4 <- paste("~~R^2== ", round(Corr, 2))
      
      # Plot all predicted treatments by the thermal time model
      df <- rv$data %>% distinct(.data[[input$GermWP]], .keep_all = FALSE)
      modelLines <- mapply(function(wp) {
        stat_function(
          fun = function(x) {
            maxCumFrac * stats::pnorm(
              wp - (Ht / x),
              mean = Psib50,
              sd = Sigma,
              log = F
            )
          },
          aes(color = as.factor(wp))
        )
      },
        df[[input$GermWP]]
      )
      
      plt <- plt +
        modelLines +
        annotate("text", x = -Inf, y = 0.95, label = " Model parameters", color = "grey0", hjust = 0) +
        annotate("text", x = -Inf, y = 0.9, label = par1, color = "grey0", hjust = 0, parse = T) +
        annotate("text", x = -Inf, y = 0.85, label = par2, color = "grey0", hjust = 0, parse = T) +
        annotate("text", x = -Inf, y = 0.8, label = par3, color = "grey0", hjust = 0, parse = T) +
        annotate("text", x = -Inf, y = 0.75, label = par4, color = "grey0", hjust = 0, parse = T)
    })
    
    plt
  })
  
  
  
  # HydroThermalTime ----
  
  #### Model params ####
  HTT.start <- list(
    HT = 800,
    Tb = 1,
    Psib50 = -1,
    Sigma = 0.4
  )
  
  HTT.min <- list(
    HT = 0,
    Tb = 0,
    Psib50 = -5,
    Sigma = 0
  )
  
  HTT.max <- list(
    HT = 5000,
    Tb = 15,
    Psib50 = 0,
    Sigma = 10
  )

  #### HydroThermalTimeUI ####
  output$HydroThermalTimeUI <- renderUI({
    validate(need(rv$modelReady$HydroThermalTime, "Please load required data for HydroThermalTime analysis."))
    list(
      p(em("The hydro thermal time model assumes a data set with germination temperature and germination water potential as treatment conditions. If you have additional treatments in your dataset, the model will average across those treatments and you may get unreliable or unexpected model results. Input any of the following model coefficients, or leave blank to compute the best fit. If the model fails, try changing or unsetting parameters.")),
      br(),
      fluidRow(
        box(
          title = "Model parameters",
          status = "primary",
          sliderInput(
            inputId = "HTT.maxCumFrac",
            label = "Max cumulative fraction",
            min = 0,
            max = 1,
            value = 1,
            step = 0.01
          ),
          lapply(names(HTT.start), function(c) {
            min <- HTT.min[[c]]
            max <- HTT.max[[c]]
            step <- signif((max - min) / 100, 2)
            numericInput(
              inputId = paste0("HTT.", c),
              label = paste0(c, " (range: ", min, " - ", max, ")"),
              value = NULL,
              min = min,
              max = max,
              step = step
            )
          })
        ),
        box(
          title = "Model results",
          status = "primary",
          tableOutput("HTTResultsTable")
        )
      ),
      fluidRow(
        box(
          status = "primary",
          width = 12,
          plotOutput("HTTPlot")
        )
      )
    )
  })
  
  #### HTTModelResults ####
  HTTModelResults <- reactive({
    req(DataLoaded())
    req(rv$modelReady$HydroThermalTime)

    # get data
    GermWP <- rv$data[[input$GermWP]]
    GermTemp <- rv$data[[input$GermTemp]]
    CumTime <- rv$data[[input$CumTime]]
    CumFrac <- rv$data[[input$CumFraction]]
    
    # get params
    maxCumFrac <- input$HTT.maxCumFrac
    coefNames <- names(HTT.start)
    userCoefs <- list()
    start <- HTT.start
    lower <- HTT.min
    upper <- HTT.max
    
    for (c in coefNames) {
      userVal <- input[[paste0("HTT.", c)]]
      if (!is.na(userVal)) {
        if (between(userVal, HTT.min[[c]], HTT.max[[c]])) {
          start[[c]] <- lower[[c]] <- upper[[c]] <- NULL
          userCoefs[[c]] <- userVal
          assign(c, userVal)
        } else {
          updateTextInput(inputId = paste0("HTT.", c), value = "")
        }
      }
    }
    
    tryCatch({
      # run model
      model <- stats::nls(
        CumFrac ~ maxCumFrac * stats::pnorm(
          GermWP - (HT / ((GermTemp - Tb) * CumTime)),
          mean = Psib50,
          sd = Sigma,
          log = F),
        start = start,
        lower = lower,
        upper = upper,
        algorithm = "port")
      
      # get correlation
      Corr <- stats::cor(CumFrac, stats::predict(model)) ^ 2
      
      # at least one coef generated by the model
      if (length(userCoefs) < length(coefNames)) {
        coefs <- summary(model)$coefficients %>%
          as_tibble(rownames = "Parameter") %>%
          pull(Estimate, Parameter) %>%
          as.list()
        
        # merge model coefs and user coefs
        for (c in coefNames) {
          if (is.null(coefs[[c]])) {
            coefs[[c]] <- userCoefs[[c]]
          }
        }
      } else {
        # user specified all coefs
        coefs <- userCoefs
      }
      
      # return results
      list(
        HT = coefs$HT,
        Tb = coefs$Tb,
        Psib50 = coefs$Psib50,
        Sigma = coefs$Sigma,
        Correlation = Corr)
    },
      error = function(e) {
        paste("Unable to compute model, try adjusting parameters. ", str_to_sentence(e[1]))
      }
    )
  })
  
  #### HTTResultsTable ####
  output$HTTResultsTable <- renderTable({
    req(HTTModelResults())
    results <- HTTModelResults()
    
    # print error message if model fails
    validate(need(is.list(results), results))
    
    # convert results list to data frame
    results %>%
      enframe() %>%
      unnest(value) %>%
      rename(
        Parameter = name,
        Value = value
      )
  }, digits = 4)
  
  #### HTTPlot ####
  output$HTTPlot <- renderPlot({
    req(DataLoaded())
    req(input$HTT.maxCumFrac)
    
    maxCumFrac <- input$HTT.maxCumFrac
    
    # generate the plot
    plt <- rv$data %>%
      ggplot(aes(
        x = .data[[input$CumTime]],
        y = .data[[input$CumFraction]],
        color = as.factor(.data[[input$GermWP]]),
        linetype = as.factor(.data[[input$GermTemp]]))) +
      annotate("rect", xmin = 0, xmax = Inf, ymin = maxCumFrac, ymax = 1, fill = "grey", alpha = 0.1) +
      geom_hline(yintercept = maxCumFrac, color = "darkgrey", linetype = "dashed") +
      geom_point(aes(shape = as.factor(.data[[input$GermTemp]])), size = 2) +
      scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, 1.02)) +
      scale_x_continuous(expand = c(0, 0)) +
      expand_limits(x = 0, y = 0) +
      labs(
        title = "HydroThermalTime Model",
        x = "Time",
        y = "Cumulative fraction germinated (%)",
        color = "Water Potential",
        shape = "Temperature",
        linetype = "Temperature") +
      guides(color = guide_legend(reverse = T, order = 1)) +
      theme_classic()
    
    # use try so it will still plot on model error
    try({
      req(is.list(HTTModelResults()))
      model <- HTTModelResults()
      
      Ht <- model$HT
      Psib50 <- model$Psib50
      Tb <- model$Tb
      Sigma <- model$Sigma
      Corr <- model$Correlation
      
      # model params
      par1 <- paste("~~HT==", signif(Ht, 3))
      par2 <- paste("~~T[b]==", signif(Tb, 3))
      par3 <- paste("~~Psi[b](50)==", signif(Psib50,3))
      par4 <- paste("~~sigma == ", signif(Sigma, 3))
      par5 <- paste("~~R^2 == ", signif(Corr, 3))
      
      # function to plot all predicted treatments by the hydro thermal time model
      df <- rv$data %>%
        distinct(.data[[input$GermWP]], .data[[input$GermTemp]], .keep_all = F) %>%
        arrange(.data[[input$GermWP]], .data[[input$GermTemp]])
      
      modelLines <- mapply(function(wp, temp) {
        stat_function(
          fun = function(x) {
            maxCumFrac * stats::pnorm(
              wp - (Ht / ((temp - Tb) * x)),
              mean = Psib50,
              sd = Sigma,
              log = F
            )
          },
          aes(color = as.factor(wp), linetype = as.factor(temp))
        )
      },
        df[[input$GermWP]],
        df[[input$GermTemp]]
      )
      
      plt <- plt +
        modelLines +
        annotate("text", x = -Inf, y = 0.95, label = " Model Parameters:", color = "grey0", hjust = 0) +
        annotate("text", x = -Inf, y = 0.9, label = par1, color = "grey0", hjust = 0, parse = T) +
        annotate("text", x = -Inf, y = 0.85, label = par2, color = "grey0", hjust = 0, parse = T) +
        annotate("text", x = -Inf, y = 0.8, label = par3, color = "grey0", hjust = 0, parse = T) +
        annotate("text", x = -Inf, y = 0.75, label = par4, color = "grey0", hjust = 0, parse = T) +
        annotate("text", x = -Inf, y = 0.7, label = par5, color = "grey0", hjust = 0, parse = T)
    })
    
    plt
  })
  
  
  
  # HydroPriming model ----
  
  
  
  # HydroThermalPriming model ----
  
  
  
  # Aging model ----
  
  
  
  # Promoter model ----
  
  
  
  # Inhibitor model ----
  
  
  
}

