# ---- Server ---- #

server <- function(input, output, session) {
  
  # Data definitions ----
  
  rv <- reactiveValues(
    data = tibble(),
    colStatus = NULL,
    modelReady = list(),
    germSpeedFracs = defaultGermSpeedFracs
  )
  
  # data has rows
  dataLoaded <- reactive({
    nrow(rv$data) > 0
  })
  
  # basic data readiness
  basicDataReady <- reactive({
    checkModelReadiness(colValidation$AllModels, rv$colStatus)
  })
  
  # set reactive values for each model's readiness
  observe({
    lapply(modelNames, function(m) {
      rv$modelReady[[m]] <- checkModelReadiness(colValidation[[m]], rv$colStatus)
    })
  })
  
  
  
  # Dashboard reactive UI elements ----
  
  ## Names and icons for dashboard tabs ----
  
  output$loadMenu <- renderMenu({
    ready <- dataLoaded()
    menuItem(
      "Upload data",
      tabName = "load",
      badgeLabel = ifelse(ready, "OK", "!"),
      badgeColor = ifelse(ready, "green", "yellow")
    )
  })
  
  lapply(modelNames, function(m) {
    output[[paste0(m, "Menu")]] <- renderMenu({
      ready <- rv$modelReady[[m]]
      menuItem(
        to_any_case(m, case = "sentence"),
        tabName = paste0(m, "Tab"),
        badgeLabel = ifelse(ready, "OK", "X"),
        badgeColor = ifelse(ready, "green", "red")
      )
    })
  })
  
  
  ## Model UI placeholders ----
  
  lapply(modelNames, function(m) {
    output[[paste0(m, "UI")]] <- renderUI({
      p("Under construction.")
    })
  })
  
  
  
  
  # Modules ----
  
  ## Intro tab ----
  
  introTabServer()

  
  ## Load tab ----
  
  loadDataReturn <- loadDataServer()
  
  observe({
    rv$data <- loadDataReturn()$data
    rv$colStatus <- loadDataReturn()$colStatus
  })


  
  # Germination tab ----
  
  #### germUI ####
  output$GerminationUI <- renderUI({
    validate(
      need(basicDataReady(), "Please load a dataset and set required column types for germination analysis.")
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
      need(dataLoaded(), "No data loaded."),
      need(basicDataReady(), "Necessary data columns not present.")
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
      arrange(TrtID, CumTime, CumFrac) # %>%
      # mutate(FracDiff = CumFrac - lag(CumFrac, default = 0))
      # Datasets provided should be used as they are without combining nor normalizing them
    if (trts == 1) {
      df <- df %>%
        group_by(TrtID, Trt1, CumTime) #%>%
        # summarise(FracDiff = sum(FracDiff), .groups = "drop_last") %>%
        # mutate(CumFrac = cumsum(FracDiff) / sum(FracDiff))
      plt <- df %>%
        ggplot(aes(x = CumTime, y = CumFrac, fill = TrtID, color = Trt1)) +
        geom_line() +
        geom_point(shape = 19, size = 2) +
        labs(color = input$germPlotTrt1) +
        guides(fill = FALSE)
      
    } else if (trts == 2) {
      df <- df %>%
        group_by(TrtID, Trt1, Trt2, CumTime) #%>%
        # summarise(FracDiff = sum(FracDiff), .groups = "drop_last") %>%
        # mutate(CumFrac = cumsum(FracDiff) / sum(FracDiff))
      plt <- df %>%
        ggplot(aes(x = CumTime, y = CumFrac, fill = TrtID, color = Trt1, shape = Trt2)) +
        geom_line() +
        geom_point(size = 2) +
        labs(color = input$germPlotTrt1, shape = input$germPlotTrt2) +
        guides(fill = FALSE)
      
    } else {
      df <- df %>%
        group_by(TrtID,CumTime) #%>%
        # summarise(FracDiff = sum(FracDiff), .groups = "drop_last") %>%
        # mutate(CumFrac = cumsum(FracDiff) / sum(FracDiff))
      plt <- df %>%
        ggplot(aes(x = CumTime, y = CumFrac, fill = TrtID)) +
        geom_line() +
        geom_point(size = 2) +
        guides(fill = FALSE)
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
    req(dataLoaded())
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
      arrange(TrtID, CumTime, CumFrac) #%>%
      #mutate(FracDiff = CumFrac - lag(CumFrac, default = 0))
    
    # group by the selected treatments AND TrtID to calculate speed individually
    df <- group_by_at(df, vars(TrtID,trts))
    
    # merge values that occur at the same timepoint
    #df <- df %>%
      #group_by(CumTime, .add = T) #%>%
      #summarise(FracDiff = sum(FracDiff), .groups = "drop_last") %>%
      #mutate(CumFrac = cumsum(FracDiff) / sum(FracDiff))
    
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
    
    # group by the selected treatments for next step
    df <- group_by_at(df, vars(trts, Frac))
    
    # Calculate average speed and standard deviations (Future) for all speed fractions
    df <- df %>%
      summarise(
        Time = mean(Time), # Time_sd = sd(Time), ADD SD in the future
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
  
  

  # ThermalTime --------------------

  #### ThermalTimeUI ####
  output$ThermalTimeUI <- renderUI({
    validate(need(rv$modelReady$ThermalTime, "Please load required data for thermal time analysis."))
    list(
      p(em("The thermal time model assumes a data set with germination temperature as a treatment condition. If you have additional treatments in your dataset, the model will display those treatments and include them in the model calculation which can result on unreliable or unexpected results. Please inform and remove additonal treatments when pertinent in the lower left corner of this section. Note: the model may fail to converge under certain max cumulative fraction values.")),
      br(),
      box(
        title = "Model data input",
        uiOutput("tempFactorLevelSelect"),
        uiOutput("dataInputType")
      ),
      box(
        title = "Model results",
        tableOutput("TTResultsTable")
      ),
      box(
        width = 12,
        plotOutput("TTPlot")
      ),
      box(
        title = "Data input filter",
        uiOutput("thermalFactorsSelect"),
        uiOutput("WPFactorLevelSelect"),
        uiOutput("TrtIDFactorLevelSelect"),
        uiOutput("TrtDescFactorLevelSelect")
      ),
      box(
        title = "Maximum germination (%) observed",
        sliderInput(
          inputId = "TTSubOMaxCumFrac",
          label = NULL,
          min = 10,
          max = 100,
          value = 100,
          step = 1)
      ),
      box(
        title = "Included interval (%):",
        sliderInput(
          inputId = "germFracRange",
          label = NULL,
          min = 0,
          max = 100,
          value = c(0,100))
      )
    )
  })
  
  # create choices with all germ temperature levels 
  TempFactorLevelChoices <- reactive({
    req(rv$modelReady$ThermalTime)
    temps <- as.factor(rv$data[[input$GermTemp]])
    cols <- levels(temps)
  })
  
  
  # Create checkbox with all Temperature levels to be included in the model analysis
  output$tempFactorLevelSelect <- renderUI({
    checkboxGroupInput(
      inputId = "tempFactorLevelSelect",
      label = "Included temperature levels:",
      choices = TempFactorLevelChoices(),
      selected = TempFactorLevelChoices()
    )
  })
  
  # Create radio button for data input type: Original or Cleaned (no repetitive cumulative fractions, keep only initial presence of a value)
  output$dataInputType <- renderUI({
    radioButtons(
      inputId = "dataInputType",
      label = "Select data input type:",
      choices = c("Original" = "orig",
                  "Cleaned" = "clean")
    )
  })
  
  # create choices with validated column names and factors without GermTemp
  germTrtThermalChoices <- reactive({
    cols <- sapply(1:nCols, function(i) {
      if (rv$colStatus[i] == T && colValidation$Role[i] == "Factor" && colValidation$InputId[i] != "GermTemp") colValidation$Column[i] 
    })
    cols <- compact(cols) #remove all null entries 
    setNames(as.list(c(cols)), c(cols))
  })
  
  # create choices with all available columns on dataset besides GermTemp 
  output$thermalFactorsSelect <- renderUI({
    checkboxGroupInput(
      inputId = "thermalFactorsSelect",
      label = "Filter factor levels for additional treatments:",
      choices = germTrtThermalChoices()
      #, selected = c("TrtID")
    )
  })
  
  # create choices with all germ wp levels
  WPFactorLevelChoices <- reactive({
    req(rv$modelReady$ThermalTime)
    req("GermWP" %in% input$thermalFactorsSelect)
    df <- as.factor(rv$data[[input$GermWP]])
    cols <- levels(df)
  })
  
  # Create checkbox with all WP levels to be included in the model analysis
  output$WPFactorLevelSelect <- renderUI({
    req(rv$modelReady$ThermalTime)
    req("GermWP" %in% input$thermalFactorsSelect)
    checkboxGroupInput(
      inputId = "WPFactorLevelSelect",
      label = "Water Potential:",
      choices = WPFactorLevelChoices(),
      selected = 0
    )
  })
  
  # create choices with all TrtID levels
  TrtIDFactorLevelChoices <- reactive({
    req(rv$modelReady$ThermalTime)
    req("TrtID" %in% input$thermalFactorsSelect)
    df <- as.factor(rv$data[[input$TrtID]])
    cols <- levels(df)
  })
  
  # Create checkbox with all TrtID levels to be included in the model analysis
  output$TrtIDFactorLevelSelect <- renderUI({
    req(rv$modelReady$ThermalTime)
    req("TrtID" %in% input$thermalFactorsSelect)
    checkboxGroupInput(
      inputId = "TrtIDFactorLevelSelect",
      label = "Treatment ID:",
      choices = TrtIDFactorLevelChoices(),
      selected = TrtIDFactorLevelChoices()
    )
  })
  
  # create choices with all TrtDesc levels
  TrtDescFactorLevelChoices <- reactive({
    req(rv$modelReady$ThermalTime)
    req("TrtDesc" %in% input$thermalFactorsSelect)
    df <- as.factor(rv$data[[input$TrtDesc]])
    cols <- levels(df)
  })
  
  # Create checkbox with all TrtDesc levels to be included in the model analysis
  output$TrtDescFactorLevelSelect <- renderUI({
    req(rv$modelReady$ThermalTime)
    req("TrtDesc" %in% input$thermalFactorsSelect)
    checkboxGroupInput(
      inputId = "TrtDescFactorLevelSelect",
      label = "Treatment description:",
      choices = TrtDescFactorLevelChoices(),
      selected = TrtDescFactorLevelChoices()
    )
  })
  
  
  
  
  ### TTSubOModelWorkingDataset ###
  
  TTSubOModelWorkingDataset <- reactive({
    req(dataLoaded())
    req(rv$modelReady$ThermalTime)
    req(input$TTSubOMaxCumFrac)
    req(input$germFracRange)
    req(input$dataInputType)
    
    
    # construct working dataset
    df <- tibble(TrtID = rv$data[[input$TrtID]])
    trts <- append("GermTemp", input$thermalFactorsSelect)  # get all selected other factors + GermTemp
    for (trt in trts) { df[[trt]] <- as.factor(rv$data[[input[[trt]]]]) } #input all factors and respective data
    df <- df %>% mutate(
      CumTime = rv$data[[input$CumTime]],
      CumFrac = rv$data[[input$CumFraction]]
    )
    
    # group by the selected treatments
    df <- group_by_at(df, trts)
    
    # Update data input type based on radio button selection (Original or Cleaned)
    if (input$dataInputType == "clean"){
      #eval(parse(text=paste("dplyr::distinct(df,",trts,",CumFrac, .keep_all = TRUE)", sep="")))
      df <- dplyr::distinct(df,TrtID,CumFrac, .keep_all = TRUE)
    } 
  
    # Filter working dataset based on treatment selections 
    df <- df %>%
      group_by(CumTime, .add = T) #%>%
    #summarise(FracDiff = sum(FracDiff), .groups = "drop_last") %>%
    #mutate(CumFrac = cumsum(FracDiff) / sum(FracDiff))
    
    if (length(input$thermalFactorsSelect)>0){
      
      if ("GermWP" %in% input$thermalFactorsSelect) {
        df <- subset(df, subset = GermWP %in% input$WPFactorLevelSelect)
      }
      if ("TrtID" %in% input$thermalFactorsSelect) {
        df <- subset(df, subset = TrtID %in% input$TrtIDFactorLevelSelect)
      }
      if ("TrtDesc" %in% input$thermalFactorsSelect) {
        df <- subset(df, subset = TrtDesc %in% input$TrtDescFactorLevelSelect)
      }
      
    }
    
    df <- df %>% 
      filter(CumFrac >= (input$germFracRange[1]/100) & CumFrac <= (input$germFracRange[2]/100))
    
    df <- subset(df, subset = GermTemp %in% input$tempFactorLevelSelect)
  })
  
  
  
  #### TTSubOModelResults ####
  TTSubOModelResults <- reactive({
    req(dataLoaded())
    req(rv$modelReady$ThermalTime)
    req(TTSubOModelWorkingDataset())
    req(input$TTSubOMaxCumFrac)
    
    #Call working dataset
    df <- TTSubOModelWorkingDataset()
    
    tryCatch({
      # local vars
      temp <- as.numeric(as.character(df$GermTemp)) #rv$data[[input$GermTemp]]
      time <- as.numeric(as.character(df$CumTime)) #rv$data[[input$CumTime]]
      germ <- as.numeric(as.character(df$CumFrac)) #rv$data[[input$CumFraction]]
      max.cum.frac <- input$TTSubOMaxCumFrac/100
      
      # run model
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
          Tb = 20,
          thetaT50 = 50,
          sigma = 1.50),
        algorithm = "port")
      
      # grab coefs
      Corr <- stats::cor(germ, stats::predict(model)) ^ 2
      Tb <- summary(model)$coefficients[[1]]
      ThetaT50 <- summary(model)$coefficients[[2]]
      Sigma <- summary(model)$coefficients[[3]]
      
      # return results
      list(
        Tb = Tb,
        ThetaT50 = ThetaT50,
        Sigma = Sigma,
        Correlation = Corr
      )
    },
      error = function(cond) {
        paste("Unable to compute model, try adjusting parameters. ", str_to_sentence(cond[1]))
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
    req(dataLoaded())
    req(rv$modelReady$ThermalTime)
    req(TTSubOModelWorkingDataset())
    req(input$TTSubOMaxCumFrac)
    
    #Call and load working dataset
    df <- TTSubOModelWorkingDataset()
    
    #GermTemp <- as.factor(as.numeric(as.character(df$GermTemp)))
    #CumTime <- as.factor(as.numeric(as.character(df$CumTime)))
    #CumFraction <- as.factor(as.numeric(as.character(df$CumFraction)))
    
    # generate the plot
    plt <- df %>%
        ggplot(aes(
        x = CumTime,
        y = CumFrac,
        color = GermTemp)) +
      annotate("rect", xmin = 0, xmax = Inf, ymin = input$TTSubOMaxCumFrac/100, ymax = 1, fill = "grey", alpha = 0.1) +
      geom_hline(yintercept = input$TTSubOMaxCumFrac/100, color = "darkgrey", linetype = "dashed") +
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
    
    # use try so it will still plot on model error
    try({
      req(is.list(TTSubOModelResults()))
      model <- TTSubOModelResults()
      
      maxCumFrac <- model$MaxCumFrac
      tb <- model$Tb
      thetaT50 <- model$ThetaT50
      sigma <- model$Sigma
      corr <- model$Correlation
      
      par1 <- paste("~~T[b]==", round(tb, 1))
      par2 <- paste("~~ThetaT(50)==", round(thetaT50, 3))
      par3 <- paste("~~sigma==", round(sigma, 3))
      par4 <- paste("~~R^2==", round(corr, 2))
      
      # Plot all predicted treatments by the thermal time model
      #temps <- distinct(df$GermTemp, .keep_all = F)
      Temp <- as.factor(as.numeric(as.character(df$GermTemp)))
      temps <- as.numeric(levels(Temp))
      
      modelLines <- mapply(function(temp) {
        stat_function(
          fun = function(x) {
            stats::pnorm(log(x, base = 10), thetaT50 - log(temp - tb, base = 10),  sigma, log = F) * input$TTSubOMaxCumFrac/100
          },
          aes(color = as.factor(temp))
        )
      },
      temps
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

  
  
  # Hydrotime model ----------------------

  #### HydrotimeUI ####
  output$HydrotimeUI <- renderUI({
    validate(need(rv$modelReady$Hydrotime, "Please load required data for Hydrotime analysis."))
    list(
      p(em("The hydrotime model assumes a data set with germination temperature as a treatment condition. If you have additional treatments in your dataset, the model will average across those treatments and you may get unreliable or unexpected model results. Note: the model may fail to converge under certain max cumulative fraction values.")),
      br(),
      box(
        title = "Model data input",
        uiOutput("HTWPFactorLevelSelect"),
        uiOutput("HTdataInputType")
      ),
      box(
        title = "Model results",
        tableOutput("HTResultsTable")
      ),
      box(
        width = 12,
        plotOutput("HTPlot")
      ),
      box(
        title = "Data input filter",
        uiOutput("HTWPFactorsSelect"),
        uiOutput("HTtempFactorLevelSelect"),
        uiOutput("HTTrtIDFactorLevelSelect"),
        uiOutput("HTTrtDescFactorLevelSelect")
      ),
      box(
        title = "Maximum germination (%) observed",
        sliderInput(
          inputId = "HTMaxCumFrac",
          label = NULL,
          min = 10,
          max = 100,
          value = 100,
          step = 1)
      ),
      box(
        title = "Included interval (%):",
        sliderInput(
          inputId = "HTgermFracRange",
          label = NULL,
          min = 0,
          max = 100,
          value = c(0,100))
      )
    )
  })
  
  # create choices with all germ wp levels 
  HTwpFactorLevelChoices <- reactive({
    req(rv$modelReady$Hydrotime)
    wps <- as.factor(rv$data[[input$GermWP]])
    cols <- levels(wps)
  })
  
  
  # Create checkbox with all Temperature levels to be included in the model analysis
  output$HTWPFactorLevelSelect <- renderUI({
    checkboxGroupInput(
      inputId = "HTWPFactorLevelSelect",
      label = "Included water potential levels:",
      choices = HTwpFactorLevelChoices(),
      selected = HTwpFactorLevelChoices()
    )
  })
  
  # Create radio button for data input type: Original or Cleaned (no repetitive cumulative fractions, keep only initial presence of a value)
  output$HTdataInputType <- renderUI({
    radioButtons(
      inputId = "HTdataInputType",
      label = "Select data input type:",
      choices = c("Original" = "orig",
                  "Cleaned" = "clean")
    )
  })
  
  # create choices with validated column names and factors without GermTemp
  germTrtHydroChoices <- reactive({
    cols <- sapply(1:nCols, function(i) {
      if (rv$colStatus[i] == T && colValidation$Role[i] == "Factor" && colValidation$InputId[i] != "GermWP") colValidation$Column[i] 
    })
    cols <- compact(cols) #remove all null entries 
    setNames(as.list(c(cols)), c(cols))
  })
  
  # create choices with all available columns on dataset besides GermTemp 
  output$HTWPFactorsSelect <- renderUI({
    checkboxGroupInput(
      inputId = "HTWPFactorsSelect",
      label = "Filter factor levels for additional treatments:",
      choices = germTrtHydroChoices()
      #, selected = c("TrtID")
    )
  })
  
  # create choices with all germ temp levels
  HTtempFactorLevelChoices <- reactive({
    req(rv$modelReady$Hydrotime)
    req("GermTemp" %in% input$HTWPFactorsSelect)
    df <- as.factor(rv$data[[input$GermTemp]])
    cols <- levels(df)
  })
  
  # Create checkbox with all temp levels to be included in the model analysis
  output$HTtempFactorLevelSelect <- renderUI({
    req(rv$modelReady$Hydrotime)
    req("GermTemp" %in% input$HTWPFactorsSelect)
    checkboxGroupInput(
      inputId = "HTtempFactorLevelSelect",
      label = "Temperature:",
      choices = HTtempFactorLevelChoices(),
      selected = HTtempFactorLevelChoices()
    )
  })
  
  # create choices with all TrtID levels
  HTTrtIDFactorLevelChoices <- reactive({
    req(rv$modelReady$Hydrotime)
    req("TrtID" %in% input$HTWPFactorsSelect)
    df <- as.factor(rv$data[[input$TrtID]])
    cols <- levels(df)
  })
  
  # Create checkbox with all TrtID levels to be included in the model analysis
  output$HTTrtIDFactorLevelSelect <- renderUI({
    req(rv$modelReady$Hydrotime)
    req("TrtID" %in% input$HTWPFactorsSelect)
    checkboxGroupInput(
      inputId = "HTTrtIDFactorLevelSelect",
      label = "Treatment ID:",
      choices = HTTrtIDFactorLevelChoices(),
      selected = HTTrtIDFactorLevelChoices()
    )
  })
  
  # create choices with all TrtDesc levels
  HTTrtDescFactorLevelChoices <- reactive({
    req(rv$modelReady$Hydrotime)
    req("TrtDesc" %in% input$HTWPFactorsSelect)
    df <- as.factor(rv$data[[input$TrtDesc]])
    cols <- levels(df)
  })
  
  # Create checkbox with all TrtDesc levels to be included in the model analysis
  output$HTTrtDescFactorLevelSelect <- renderUI({
    req(rv$modelReady$Hydrotime)
    req("TrtDesc" %in% input$HTWPFactorsSelect)
    checkboxGroupInput(
      inputId = "HTTrtDescFactorLevelSelect",
      label = "Treatment description:",
      choices = HTTrtDescFactorLevelChoices(),
      selected = HTTrtDescFactorLevelChoices()
    )
  })
  
  
  
  ### HTModelWorkingDataset ###
  
  HTModelWorkingDataset <- reactive({
    req(dataLoaded())
    req(rv$modelReady$Hydrotime)
    req(input$HTMaxCumFrac)
    req(input$HTgermFracRange)
    req(input$HTdataInputType)
    
    
    # construct working dataset
    df <- tibble(TrtID = rv$data[[input$TrtID]])
    trts <- append("GermWP", input$HTWPFactorsSelect)  # get all selected other factors + GermWP
    for (trt in trts) { df[[trt]] <- as.factor(rv$data[[input[[trt]]]]) } #input all factors and respective data
    df <- df %>% mutate(
      CumTime = rv$data[[input$CumTime]],
      CumFrac = rv$data[[input$CumFraction]]
    )
    
    # group by the selected treatments
    df <- group_by_at(df, trts)
    
    # Update data input type based on radio button selection (Original or Cleaned)
    if (input$HTdataInputType == "clean"){
      #eval(parse(text=paste("dplyr::distinct(df,",trts,",CumFrac, .keep_all = TRUE)", sep="")))
      df <- dplyr::distinct(df,TrtID,CumFrac, .keep_all = TRUE)
    } 
    
    # Filter working dataset based on treatment selections 
    df <- df %>%
      group_by(CumTime, .add = T) #%>%
    #summarise(FracDiff = sum(FracDiff), .groups = "drop_last") %>%
    #mutate(CumFrac = cumsum(FracDiff) / sum(FracDiff))
    
    if (length(input$HTWPFactorsSelect)>0){
      
      if ("GermTemp" %in% input$HTWPFactorsSelect) {
        df <- subset(df, subset = GermTemp %in% input$HTtempFactorLevelSelect)
      }
      if ("TrtID" %in% input$HTWPFactorsSelect) {
        df <- subset(df, subset = TrtID %in% input$HTTrtIDFactorLevelSelect)
      }
      if ("TrtDesc" %in% input$HTWPFactorsSelect) {
        df <- subset(df, subset = TrtDesc %in% input$HTTrtDescFactorLevelSelect)
      }
      
    }
    
    df <- df %>% 
      filter(CumFrac >= (input$HTgermFracRange[1]/100) & CumFrac <= (input$HTgermFracRange[2]/100))
    
    df <- subset(df, subset = GermWP %in% input$HTWPFactorLevelSelect)
  })
  
  
  
  
  
  #### HTModelResults ####
  HTModelResults <- reactive({
    req(dataLoaded())
    req(rv$modelReady$Hydrotime)
    req(HTModelWorkingDataset())
    req(input$HTMaxCumFrac)
    
    #Call and load working dataset
    df <- HTModelWorkingDataset()
    
    tryCatch({
      # required data
      wp <- as.numeric(as.character(df$GermWP)) #rv$data[[input$GermWP]]
      time <- as.numeric(as.character(df$CumTime)) #rv$data[[input$CumTime]]
      germ <- as.numeric(as.character(df$CumFrac)) #rv$data[[input$CumFraction]]
      max.cum.frac <- input$HTMaxCumFrac/100
      
      # run model
      model <- stats::nls(
        formula = germ ~ max.cum.frac * stats::pnorm(
          wp - (HT / time),
          Psib50,
          Sigma,
          log = FALSE),
        start = list(
          HT = 60,
          Psib50 = -0.8,
          Sigma = 0.2),
        lower = list(
          HT = 1,
          Psib50 = -5,
          Sigma = 0.0001),
        upper = list(
          HT = 1000,
          Psib50 = -0.000000001,
          Sigma = 2),
        algorithm = "port")
      
      # grab coefs
      corr <- stats::cor(germ, stats::predict(model)) ^ 2
      HT <- summary(model)$coefficients[[1]]
      Psib50 <- summary(model)$coefficients[[2]]
      Sigma <- summary(model)$coefficients[[3]]
      
      # return results
      list(
        HT = HT,
        Psib50 = Psib50,
        Sigma = Sigma,
        Correlation = corr)
    },
      error = function(cond) {
        paste("Unable to compute model, try adjusting parameters. ", str_to_sentence(cond[1]))
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
    req(dataLoaded())
    req(rv$modelReady$Hydrotime)
    req(HTModelWorkingDataset())
    req(input$HTMaxCumFrac)
    
    #Call and load working dataset
    df <- HTModelWorkingDataset()
    
    # generate the plot
    plt <- df %>%
      ggplot(aes(
        x = CumTime,
        y = CumFrac,
        color = GermWP)) +
      annotate("rect", xmin = 0, xmax = Inf, ymin = input$HTMaxCumFrac/100, ymax = 1, fill = "grey", alpha = 0.1) +
      geom_hline(yintercept = input$HTMaxCumFrac/100, color = "darkgrey", linetype = "dashed") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, 1.02)) +
      scale_x_continuous(expand = c(0, 0)) +
      expand_limits(x = 0, y = 0) +
      labs(
        title = "Hydrotime Model",
        x = "Time",
        y = "Cumulative fraction germinated (%)",
        color = "Water potential") +
      guides(color = guide_legend(reverse = T, order = 1)) +
      theme_classic()
    
    # use try so it will still plot on model error
    try({
      req(is.list(HTModelResults()))
      model <- HTModelResults()
      
      maxCumFrac <- model$MaxCumFrac
      ht <- model$HT
      psib50 <- model$Psib50
      sigma <- model$Sigma
      corr <- model$Correlation
      
      par1 <- paste("~~HT==", round(ht, 2))
      par2 <- paste("~~Psi[b](50)==", round(psib50, 3))
      par3 <- paste("~~sigma== ", round(sigma, 3))
      par4 <- paste("~~R^2== ", round(corr, 2))
      
      # Plot all predicted treatments by the hydrotime model
      Wp <- as.factor(as.numeric(as.character(df$GermWP)))
      wps <- as.numeric(levels(Wp))
      
      #df <- rv$data %>% distinct(.data[[input$GermWP]], .keep_all = FALSE)
      modelLines <- mapply(function(wp) {
        stat_function(
          fun = function(x) {
            stats::pnorm(wp - (ht / x), psib50, sigma, log = FALSE) * input$HTMaxCumFrac/100
          },
          aes(color = as.factor(wp))
        )
      },
        wps
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
  
  
  
  # Hydrothermal time ----

  #### HydrothermalTimeUI ####
  output$HydrothermalTimeUI <- renderUI({
    validate(need(rv$modelReady$HydrothermalTime, "Please load required data for Hydrothermal time analysis."))
    list(
      p(em("The hydrothermal time model assumes a data set with germination temperature and germination water potential as treatment conditions. If you have additional treatments in your dataset, the model will average across those treatments and you may get unreliable or unexpected model results. Note: the model may fail to converge under certain max cumulative fraction values.")),
      br(),
        box(
          title = "Model data input",
          column(width = 12,HTML("<b>Included factor levels<b>")),
          hr(style = "margin-bottom: 0.3em"),
          column(width = 6,
          uiOutput("HTTWPFactorLevelSelect")),
          column(width = 6,
          uiOutput("HTTTempFactorLevelSelect")),
          column(width = 6,
          uiOutput("HTTdataInputType")),
          column(width = 6,
            numericInput(
              inputId = "HTTBaseTemp",
              label = HTML("Base temperature:"),
              value = NULL       
              ),
          ),
        ),
        box(
          title = "Model results",
          tableOutput("HTTResultsTable")
        ),
        box(
          width = 12,
          plotOutput("HTTPlot")
        ),
        box(
          title = "Data input filter",
          uiOutput("HTTFactorsSelect"),
          uiOutput("HTTTrtIDFactorLevelSelect"),
          uiOutput("HTTTrtDescFactorLevelSelect")
        ),
        box(
          title = "Maximum germination (%) observed",
          sliderInput(
            inputId = "HTTMaxCumFrac",
            label = NULL,
            min = 10,
            max = 100,
            value = 100,
            step = 1)
        ),
        box(
          title = "Included interval (%):",
          sliderInput(
            inputId = "HTTgermFracRange",
            label = NULL,
            min = 0,
            max = 100,
            value = c(0,100))
      )
    )
  })
  
  
  # create choices with all germ wp levels 
  HTTwpFactorLevelChoices <- reactive({
    req(rv$modelReady$HydrothermalTime)
    wps <- as.factor(rv$data[[input$GermWP]])
    cols <- levels(wps)
  })
  
  
  # Create checkbox with all wps levels to be included in the model analysis
  output$HTTWPFactorLevelSelect <- renderUI({
    checkboxGroupInput(
      inputId = "HTTWPFactorLevelSelect",
      label = "Water potential:",
      choices = HTTwpFactorLevelChoices(),
      selected = HTTwpFactorLevelChoices()
    )
  })
  
  # create choices with all germ temperature levels 
  HTTTempFactorLevelChoices <- reactive({
    req(rv$modelReady$HydrothermalTime)
    temps <- as.factor(rv$data[[input$GermTemp]])
    cols <- levels(temps)
  })
  
  
  # Create checkbox with all Temperature levels to be included in the model analysis
  output$HTTTempFactorLevelSelect <- renderUI({
    checkboxGroupInput(
      inputId = "HTTTempFactorLevelSelect",
      label = "Temperature:",
      choices = HTTTempFactorLevelChoices(),
      selected = HTTTempFactorLevelChoices()
    )
  })
  
  
  # Create radio button for data input type: Original or Cleaned (no repetitive cumulative fractions, keep only initial presence of a value)
  output$HTTdataInputType <- renderUI({
    radioButtons(
      inputId = "HTTdataInputType",
      label = "Select data type:",
      choices = c("Original" = "orig",
                  "Cleaned" = "clean")
    )
  })
  
  # create choices with validated column names and factors without GermTemp nor GermWP
  germTrtHTTChoices <- reactive({
    cols <- sapply(1:nCols, function(i) {
      if (rv$colStatus[i] == T && colValidation$Role[i] == "Factor" && colValidation$InputId[i] != "GermWP" && colValidation$InputId[i] != "GermTemp") colValidation$Column[i] 
    })
    cols <- compact(cols) #remove all null entries 
    setNames(as.list(c(cols)), c(cols))
  })
  
  # create choices with all available columns on dataset besides GermTemp 
  output$HTTFactorsSelect <- renderUI({
    checkboxGroupInput(
      inputId = "HTTFactorsSelect",
      label = "Filter factor levels for additional treatments:",
      choices = germTrtHTTChoices()
      #, selected = c("TrtID")
    )
  })
  
  # create choices with all TrtID levels
  HTTTrtIDFactorLevelChoices <- reactive({
    req(rv$modelReady$HydrothermalTime)
    req("TrtID" %in% input$HTTFactorsSelect)
    df <- as.factor(rv$data[[input$TrtID]])
    cols <- levels(df)
  })
  
  # Create checkbox with all TrtID levels to be included in the model analysis
  output$HTTTrtIDFactorLevelSelect <- renderUI({
    req(rv$modelReady$HydrothermalTime)
    req("TrtID" %in% input$HTTFactorsSelect)
    checkboxGroupInput(
      inputId = "HTTTrtIDFactorLevelSelect",
      label = "Treatment ID:",
      choices = HTTTrtIDFactorLevelChoices(),
      selected = HTTTrtIDFactorLevelChoices()
    )
  })
  
  # create choices with all TrtDesc levels
  HTTTrtDescFactorLevelChoices <- reactive({
    req(rv$modelReady$HydrothermalTime)
    req("TrtDesc" %in% input$HTTFactorsSelect)
    df <- as.factor(rv$data[[input$TrtDesc]])
    cols <- levels(df)
  })
  
  # Create checkbox with all TrtDesc levels to be included in the model analysis
  output$HTTTrtDescFactorLevelSelect <- renderUI({
    req(rv$modelReady$HydrothermalTime)
    req("TrtDesc" %in% input$HTTFactorsSelect)
    checkboxGroupInput(
      inputId = "HTTTrtDescFactorLevelSelect",
      label = "Treatment description:",
      choices = HTTTrtDescFactorLevelChoices(),
      selected = HTTTrtDescFactorLevelChoices()
    )
  })
  
  
  
  ### HTTModelWorkingDataset ###
  HTTModelWorkingDataset <- reactive({
    req(dataLoaded())
    req(rv$modelReady$HydrothermalTime)
    req(input$HTTMaxCumFrac)
    req(input$HTTgermFracRange)
    req(input$HTTdataInputType)
    
    # construct working dataset
    df <- tibble(TrtID = rv$data[[input$TrtID]])
    trts <- append(c("GermTemp","GermWP"), input$HTTFactorsSelect)  # get all selected other factors + GermWP
    for (trt in trts) { df[[trt]] <- as.factor(rv$data[[input[[trt]]]]) } #input all factors and respective data
    df <- df %>% mutate(
      CumTime = rv$data[[input$CumTime]],
      CumFrac = rv$data[[input$CumFraction]]
    )
    
    # group by the selected treatments
    df <- group_by_at(df, trts)
    
    # Update data input type based on radio button selection (Original or Cleaned)
    if (input$HTTdataInputType == "clean"){
      #eval(parse(text=paste("dplyr::distinct(df,",trts,",CumFrac, .keep_all = TRUE)", sep="")))
      df <- dplyr::distinct(df,TrtID,CumFrac, .keep_all = TRUE)
    } 
    
    # Filter working dataset based on treatment selections 
    df <- df %>%
      group_by(CumTime, .add = T) #%>%
    #summarise(FracDiff = sum(FracDiff), .groups = "drop_last") %>%
    #mutate(CumFrac = cumsum(FracDiff) / sum(FracDiff))
    
    if (length(input$HTTFactorsSelect)>0){
      
      if ("TrtID" %in% input$HTTFactorsSelect) {
        df <- subset(df, subset = TrtID %in% input$HTTTrtIDFactorLevelSelect)
      }
      if ("TrtDesc" %in% input$HTTFactorsSelect) {
        df <- subset(df, subset = TrtDesc %in% input$HTTTrtDescFactorLevelSelect)
      }
      
    }
    
    df <- df %>% 
      filter(CumFrac >= (input$HTTgermFracRange[1]/100) & CumFrac <= (input$HTTgermFracRange[2]/100))
    
    df <- subset(df, subset = GermWP %in% input$HTTWPFactorLevelSelect)
    df <- subset(df, subset = GermTemp %in% input$HTTTempFactorLevelSelect)
  })
  
  
  
  #### HTTModelResults ####
  HTTModelResults <- reactive({
    req(dataLoaded())
    req(rv$modelReady$HydrothermalTime)
    req(HTTModelWorkingDataset())
    req(input$HTTMaxCumFrac)
    
    #Call and load working dataset
    df <- HTTModelWorkingDataset()
    
    tryCatch({
      wp <- as.numeric(as.character(df$GermWP)) #rv$data[[input$GermWP]]
      temp <- as.numeric(as.character(df$GermTemp)) #rv$data[[input$GermTemp]]
      time <- as.numeric(as.character(df$CumTime)) #rv$data[[input$CumTime]]
      germ <- as.numeric(as.character(df$CumFrac)) #rv$data[[input$CumFraction]]
      max.cum.frac <- input$HTTMaxCumFrac/100
      base.temp <- input$HTTBaseTemp
      
      # model conditions
      start <- list(
        HT = 800,
        psib50 = -1,
        sigma = 0.4)
      lower <- list(
        HT = 1,
        psib50 = -5,
        sigma = 0.0001)
      upper <- list(
        HT = 5000,
        psib50 = 0,
        sigma = 10)
      
      if (is.na(base.temp)) {
        start$Tb <- 1
        lower$Tb <- 0
        upper$Tb <- 15
      } else {
        Tb <- base.temp
      }
      
      # run model
      model <- stats::nls(
        germ ~ max.cum.frac * stats::pnorm(
          wp - (HT / ((temp - Tb) * time)),
          psib50,
          sigma,
          log = FALSE),
        start = start,
        lower = lower,
        upper = upper,
        algorithm = "port")
      
      # get coefs
      corr <- stats::cor(germ, stats::predict(model)) ^ 2
      HT <- summary(model)$coefficients[[1]]
      Psib50 <- summary(model)$coefficients[[2]]
      Sigma <- summary(model)$coefficients[[3]]
      if (is.na(base.temp)) {
        Tb <- summary(model)$coefficients[[4]]
      } else {
        Tb <- base.temp
      }
      
      # return results
      list(
        HT = HT,
        Tb = Tb,
        Psib50 = Psib50,
        Sigma = Sigma,
        Correlation = corr)
    },
      error = function(cond) {
        paste("Unable to compute model, try adjusting parameters. ", str_to_sentence(cond[1]))
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
    req(dataLoaded())
    req(rv$modelReady$HydrothermalTime)
    req(HTTModelWorkingDataset())
    req(input$HTTMaxCumFrac)    
    
    #Call and load working dataset
    df <- HTTModelWorkingDataset()
    
    # generate the plot
    plt <- df %>%
      ggplot(aes(
        x = CumTime,
        y = CumFrac,
        color = GermWP,
        shape = GermTemp,
        linetype = GermTemp)) +
      annotate("rect", xmin = 0, xmax = Inf, ymin = input$HTTMaxCumFrac/100, ymax = 1, fill = "grey", alpha = 0.1) +
      geom_hline(yintercept = input$HTTMaxCumFrac/100, color = "darkgrey", linetype = "dashed") +
      geom_point(size = 2) + #geom_point(aes(shape = as.factor(.data[[input$GermTemp]])), size = 2)
      scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, 1.02)) +
      scale_x_continuous(expand = c(0, 0)) +
      expand_limits(x = 0, y = 0) +
      labs(
        title = "Hydrothermal Time Model",
        x = "Time",
        y = "Cumulative fraction germinated (%)",
        color = "Water Potential",
        shape = "Temperature") +
      guides(color = guide_legend(reverse = T, order = 1), linetype = FALSE) +
      theme_classic()
    
    # use try so it will still plot on model error
    try({
      req(is.list(HTTModelResults()))
      model <- HTTModelResults()
      
      maxCumFrac <- input$HTTMaxCumFrac
      ht <- model$HT
      psib50 <- model$Psib50
      tb <- model$Tb
      sigma <- model$Sigma
      corr <- model$Correlation
      
      # model params
      par1 <- paste("~~HT==", round(ht, 2))
      par2 <- paste("~~T[b]==", round(tb, 2))
      par3 <- paste("~~psi[b](50)==", round(psib50,3))
      par4 <- paste("~~sigma == ", round(sigma, 3))
      par5 <- paste("~~R^2 == ", round(corr, 2))
      
      # function to plot all predicted treatments by the hydro thermal time model
      
      Wp <- as.factor(as.numeric(as.character(df$GermWP)))
      Temp <- as.factor(as.numeric(as.character(df$GermTemp)))
      
      df1 <- tibble(GermWP = Wp, GermTemp = Temp)
      
      df1 <- df1 %>%
        distinct(GermWP, GermTemp, .keep_all = F) %>%
        arrange(GermWP, GermTemp)

      wps <- as.numeric(as.character(df1$GermWP))
      temps <- as.numeric(as.character(df1$GermTemp))
      
      modelLines <- mapply(function(wp, temp) {
        stat_function(
          fun = function(x) {
              stats::pnorm(
              wp - (ht / ((temp - tb) * x)),
              psib50,
              sigma,
              log = FALSE
            ) *  (input$HTTMaxCumFrac/100)
          },
          aes(color = as.factor(wp), linetype = as.factor(temp))
        )
      },
        wps,
        temps
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
  
  #### AgingUI ####
  output$AgingUI <- renderUI({
    validate(need(rv$modelReady$Aging, "Please load required data for the aging model analysis."))
    list(
      p(em("The aging model assumes a data set with aging (natural, controlled deterioration or accelerated aging) as a treatment condition. If you have additional treatments in your dataset, please filter out those as you may get unreliable or unexpected model results. Note: the model may fail to converge under certain max cumulative fraction values.")),
      br(),
      box(
        title = "Model data input",
        uiOutput("AgingFactorLevelSelect"),
        uiOutput("AgdataInputType")
      ),
      box(
        title = "Model results",
        tableOutput("AgResultsTable")
      ),
      box(
        width = 12,
        plotOutput("AgPlot")
      ),
      box(
        title = "Data input filter",
        uiOutput("AgingFactorsSelect"),
        uiOutput("AgingTrtIDFactorLevelSelect"),
        uiOutput("AgingTrtDescFactorLevelSelect")
      ),
      box(
        title = "Maximum germination (%) observed",
        sliderInput(
          inputId = "AgMaxCumFrac",
          label = NULL,
          min = 10,
          max = 100,
          value = 100,
          step = 1)
      ),
      box(
        title = "Included interval (%):",
        sliderInput(
          inputId = "AggermFracRange",
          label = NULL,
          min = 0,
          max = 100,
          value = c(0,100))
      )
    )
  })
  
  # create choices with all aging levels 
  AgingFactorLevelChoices <- reactive({
    req(rv$modelReady$Aging)
    ags <- as.factor(rv$data[[input$AgingTime]])
    cols <- levels(ags)
  })
  
  
  # Create checkbox with all aging levels to be included in the model analysis
  output$AgingFactorLevelSelect <- renderUI({
    checkboxGroupInput(
      inputId = "AgingFactorLevelSelect",
      label = "Included aging levels:",
      choices = AgingFactorLevelChoices(),
      selected = AgingFactorLevelChoices()
    )
  })
  
  # Create radio button for data input type: Original or Cleaned (no repetitive cumulative fractions, keep only initial presence of a value)
  output$AgdataInputType <- renderUI({
    radioButtons(
      inputId = "AgdataInputType",
      label = "Select data input type:",
      choices = c("Original" = "orig",
                  "Cleaned" = "clean")
    )
  })
  
  # create choices with validated column names and factors without AgingTime
  germTrtAgingChoices <- reactive({
    cols <- sapply(1:nCols, function(i) {
      if (rv$colStatus[i] == T && colValidation$Role[i] == "Factor" && colValidation$InputId[i] != "AgingTime") colValidation$Column[i] 
    })
    cols <- compact(cols) #remove all null entries 
    setNames(as.list(c(cols)), c(cols))
  })
  
  # create choices with all available columns on dataset besides AgingTime 
  output$AgingFactorsSelect <- renderUI({
    checkboxGroupInput(
      inputId = "AgingFactorsSelect",
      label = "Filter factor levels for additional treatments:",
      choices = germTrtAgingChoices()
      #, selected = c("TrtID")
    )
  })
  
  
  # create choices with all TrtID levels
  AgingTrtIDFactorLevelChoices <- reactive({
    req(rv$modelReady$Aging)
    req("TrtID" %in% input$AgingFactorsSelect)
    df <- as.factor(rv$data[[input$TrtID]])
    cols <- levels(df)
  })
  
  # Create checkbox with all TrtID levels to be included in the model analysis
  output$AgingTrtIDFactorLevelSelect <- renderUI({
    req(rv$modelReady$Aging)
    req("TrtID" %in% input$AgingFactorsSelect)
    checkboxGroupInput(
      inputId = "AgingTrtIDFactorLevelSelect",
      label = "Treatment ID:",
      choices = AgingTrtIDFactorLevelChoices(),
      selected = AgingTrtIDFactorLevelChoices()
    )
  })
  
  # create choices with all TrtDesc levels
  AgingTrtDescFactorLevelChoices <- reactive({
    req(rv$modelReady$Aging)
    req("TrtDesc" %in% input$AgingFactorsSelect)
    df <- as.factor(rv$data[[input$TrtDesc]])
    cols <- levels(df)
  })
  
  # Create checkbox with all TrtDesc levels to be included in the model analysis
  output$AgingTrtDescFactorLevelSelect <- renderUI({
    req(rv$modelReady$Aging)
    req("TrtDesc" %in% input$AgingFactorsSelect)
    checkboxGroupInput(
      inputId = "AgingTrtDescFactorLevelSelect",
      label = "Treatment description:",
      choices = AgingTrtDescFactorLevelChoices(),
      selected = AgingTrtDescFactorLevelChoices()
    )
  })
  
  
  
  ### AgingModelWorkingDataset ###
  
  AgingModelWorkingDataset <- reactive({
    req(dataLoaded())
    req(rv$modelReady$Aging)
    req(input$AgMaxCumFrac)
    req(input$AggermFracRange)
    req(input$AgdataInputType)
    
    
    # construct working dataset
    df <- tibble(TrtID = rv$data[[input$TrtID]])
    trts <- append("AgingTime", input$AgingFactorsSelect)  # get all selected other factors + AgingTime
    for (trt in trts) { df[[trt]] <- as.factor(rv$data[[input[[trt]]]]) } #input all factors and respective data
    df <- df %>% mutate(
      CumTime = rv$data[[input$CumTime]],
      CumFrac = rv$data[[input$CumFraction]]
    )
    
    # group by the selected treatments
    df <- group_by_at(df, trts)
    
    # Update data input type based on radio button selection (Original or Cleaned)
    if (input$AgdataInputType == "clean"){
      #eval(parse(text=paste("dplyr::distinct(df,",trts,",CumFrac, .keep_all = TRUE)", sep="")))
      df <- dplyr::distinct(df,TrtID,CumFrac, .keep_all = TRUE)
    } 
    
    # Filter working dataset based on treatment selections 
    df <- df %>%
      group_by(CumTime, .add = T) #%>%
    #summarise(FracDiff = sum(FracDiff), .groups = "drop_last") %>%
    #mutate(CumFrac = cumsum(FracDiff) / sum(FracDiff))
    
    if (length(input$AgingFactorsSelect)>0){
      
      if ("TrtID" %in% input$AgingFactorsSelect) {
        df <- subset(df, subset = TrtID %in% input$AgingTrtIDFactorLevelSelect)
      }
      if ("TrtDesc" %in% input$AgingFactorsSelect) {
        df <- subset(df, subset = TrtDesc %in% input$AgingTrtDescFactorLevelSelect)
      }
      
    }
    
    df <- df %>% 
      filter(CumFrac >= (input$AggermFracRange[1]/100) & CumFrac <= (input$AggermFracRange[2]/100))
    
    df <- subset(df, subset = AgingTime %in% input$AgingFactorLevelSelect)
  })
  
  
  
  
  
  #### AgingModelResults ####
  AgingModelResults <- reactive({
    req(dataLoaded())
    req(rv$modelReady$Aging)
    req(AgingModelWorkingDataset())
    req(input$AgMaxCumFrac)
    
    #Call and load working dataset
    df <- AgingModelWorkingDataset()
    
    tryCatch({
      # required data
      Atime <- as.numeric(as.character(df$AgingTime)) #rv$data[[input$AgingTime]]
      time <- as.numeric(as.character(df$CumTime)) #rv$data[[input$CumTime]]
      germ <- as.numeric(as.character(df$CumFrac)) #rv$data[[input$CumFraction]]
      max.cum.frac <- input$AgMaxCumFrac/100
      
      # run model
      model <- stats::nls(
        formula = germ ~ max.cum.frac * stats::pnorm(
          -(Atime + ThetaA / time), #wp - (HT / time),
          -Pmax50, #Psib50,
          Sigma,
          log = FALSE),
        start = list(
          ThetaA = 100,
          Pmax50 = 10,
          Sigma = 3),
        lower = list(
          ThetaA = 1,
          Pmax50 = 1,
          Sigma = 0.1),
        upper = list(
          ThetaA = 1000,
          Pmax50 = 1000,
          Sigma = 10),
        algorithm = "port")
      
      # grab coefs
      corr <- stats::cor(germ, stats::predict(model)) ^ 2
      ThetaA <- summary(model)$coefficients[[1]]
      Pmax50 <- summary(model)$coefficients[[2]]
      Sigma <- summary(model)$coefficients[[3]]
      
      # return results
      list(
        ThetaA = ThetaA,
        Pmax50 = Pmax50,
        Sigma = Sigma,
        Correlation = corr)
    },
    error = function(cond) {
      paste("Unable to compute model, try adjusting parameters. ", str_to_sentence(cond[1]))
    }
    )
  })
  
  #### AgResultsTable ####
  output$AgResultsTable <- renderTable({
    req(AgingModelResults())
    results <- AgingModelResults()
    
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
  
  #### AgPlot ####
  output$AgPlot <- renderPlot({
    req(dataLoaded())
    req(rv$modelReady$Aging)
    req(AgingModelWorkingDataset())
    req(input$AgMaxCumFrac)
    
    #Call and load working dataset
    df <- AgingModelWorkingDataset()
    
    # generate the plot
    plt <- df %>%
      ggplot(aes(
        x = CumTime,
        y = CumFrac,
        color = AgingTime)) +
      annotate("rect", xmin = 0, xmax = Inf, ymin = input$AgMaxCumFrac/100, ymax = 1, fill = "grey", alpha = 0.1) +
      geom_hline(yintercept = input$AgMaxCumFrac/100, color = "darkgrey", linetype = "dashed") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, 1.02)) +
      scale_x_continuous(expand = c(0, 0)) +
      expand_limits(x = 0, y = 0) +
      labs(
        title = "Aging Time Model",
        x = "Time",
        y = "Cumulative fraction germinated (%)",
        color = "Aging Time") +
      guides(color = guide_legend(reverse = F, order = 1)) +
      theme_classic()
    
    # use try so it will still plot on model error
    try({
      req(is.list(AgingModelResults()))
      model <- AgingModelResults()
      
      maxCumFrac <- model$MaxCumFrac
      thetaA <- model$ThetaA
      pmax50 <- model$Pmax50
      sigma <- model$Sigma
      corr <- model$Correlation
      
      par1 <- paste("~~", expression(theta~Age),"==", round(thetaA, 2))
      par2 <- paste("~~Pmax(50)==", round(pmax50, 3))
      par3 <- paste("~~sigma== ", round(sigma, 3))
      par4 <- paste("~~R^2== ", round(corr, 2))
      
      # Plot all predicted treatments by the hydrotime model
      Atimes <- as.factor(as.numeric(as.character(df$AgingTime)))
      atimes <- as.numeric(levels(Atimes))
      
      #df <- rv$data %>% distinct(.data[[input$AgingTime]], .keep_all = FALSE)
      modelLines <- mapply(function(atime) {
        stat_function(
          fun = function(x) {
            stats::pnorm(-(atime + thetaA / x), -pmax50, sigma, log = FALSE) * input$AgMaxCumFrac/100
          },
          aes(color = as.factor(atime))
        )
      },
      atimes
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
  
  
  
  # Promoter model ----
  
  
  
  # Inhibitor model ----
  
  
  
  # Gracefully exit ----
  
  session$onSessionEnded(function() { stopApp() })
  
}

