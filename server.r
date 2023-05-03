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
  
  trtChoices <- reactive({
    req(rv$colStatus)
    cols <- sapply(1:nCols, function(i) {
      if (rv$colStatus[i] == T && colValidation$Role[i] == "Factor") colValidation$Column[i]
    })
    compact(cols)
  })
  
  
  
  # Dashboard reactive UI elements ----
  
  ## Names and icons for dashboard tabs ----
  output$LoadMenu <- renderMenu({
    ready <- dataLoaded()
    menuItem(
      "Upload data",
      tabName = "LoadTab",
      badgeLabel = ifelse(ready, "OK", "!"),
      badgeColor = ifelse(ready, "green", "yellow")
    )
  })
  
  lapply(modelNames, function(m) {
    output[[paste0(m, "Menu")]] <- renderMenu({
      ready <- truthy(rv$modelReady[[m]])
      menuItem(
        to_any_case(m, case = "sentence"),
        tabName = paste0(m, "Tab"),
        badgeLabel = ifelse(ready, "OK", "X"),
        badgeColor = ifelse(ready, "green", "red")
      )
    })
  })
  

  # Modules ----
  
  ## Intro ----
  introServer()
  
  
  ## Load ----
  loadDataReturn <- loadDataServer()
  
  observe({
    rv$data <- loadDataReturn()$data
    rv$colStatus <- loadDataReturn()$colStatus
    rv$modelReady <- loadDataReturn()$modelReady
  })
  
  
  ## Germination ----
  germinationServer(
    data = reactive(rv$data),
    ready = reactive(truthy(rv$modelReady$Germination)),
    trtChoices = reactive(trtChoices())
  )
  
  
  ## Thermal Time ----
  thermalTimeServer(
    data = reactive(rv$data),
    ready = reactive(truthy(rv$modelReady$ThermalTime))
  )

  
  ## Hydro Time  ----
  hydroTimeServer(
    data = reactive(rv$data),
    ready = reactive(truthy(rv$modelReady$HydroTime))
  )
  
  
  ## Hydrothermal Time ----
  hydrothermalTimeServer(
    data = reactive(rv$data),
    ready = reactive(truthy(rv$modelReady$HydrothermalTime))
  )
  
  
  # Hydro Priming // under construction ----
  hydroPrimingServer(
    data = reactive(rv$data),
    ready = reactive(truthy(rv$modelReady$HydroPriming))
  )
  
  
  # Hydrothermal Priming // under construction ----
  
  hydrothermalPrimingServer(
    data = reactive(rv$data),
    ready = reactive(truthy(rv$modelReady$HydrothermalPriming))
  )
  
  
  # Aging ----
  
  agingServer(
    data = reactive(rv$data),
    ready = reactive(truthy(rv$modelReady$Aging))
  )
  
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

