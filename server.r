# ---- Server ---- #

#library(dplyr)
library(tidyverse)
library(shiny)
library(shinyjs)
library(DT)
#library(pbtmodels)

# remove.packages("pbtm")

#Install required package for GitHub repository
# install.packages("plyr") # for some reason needed to install pbtm... (CHECK)
# install.packages("dplyr")
# remove.packages("ggplot2")
# install.packages("ggplot2", force = TRUE, dependencies = TRUE)
# devtools::install_github("pedrobello/pbtm", force = TRUE, dependencies = TRUE)
# devtools::install_github("pedrobello/pbtmodels", dependencies = TRUE)
# devtools::install_github("pedrobello/pbtmodels")
# install.packages("tidyverse", force = TRUE, dependencies = TRUE)


server <- function(input, output, session) {
  
  # On load ----
  
  # this forces the bscollapse panels to load their content by starting open then closing
  updateCollapse(session, "data", close = c("view", "cols"))
  
  
  
  # Data definitions ----
  
  # - number of columns (rows) in the column validation file= 12
  nCols <- nrow(columnValidation)
  
  # Create object to store reactive values - full table
  rv <- reactiveValues(
    data = tibble(),
    colStatus = NULL
  )
  
  
  
  
  # Action Buttons ----
  
  observeEvent(input$loadSampleGermData, { # when clicked
    rv$data <- sampleGermData # load sample germ data inside rv$data
    updateCollapse(session, "data", open = "view", close = "load")
  })
  observeEvent(input$loadSamplePrimingData, {
    rv$data <- samplePrimingData # load sample priming data
    updateCollapse(session, "data", open = "view", close = "load")
  })
  observeEvent(input$userData, {
    rv$data <- read_csv(input$userData$datapath, col_types = cols()) %>%
      distinct() # remove any duplicate rows # load data uploaded from user 
    updateCollapse(session, "data", open = "view", close = "load")
  })
  observeEvent(input$clearData, {
    rv$data <- tibble() # clear data
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
  
  output$currentDataDisplay <- renderUI({ # show current data or data lacking message
    if (nrow(rv$data) == 0) { # check number of rows in the data list
      return("No data loaded.")
    } else {
      dataTableOutput("currentDataTable")
    }
  })
  
  
  # Column matching boxes ----
  
  columnNames <- reactive({ # Reactive function to check and load column names from current data
    req(rv$data) # check if rv$data is correct, no missing inputs or preconditions
    names(rv$data) # get names of columns 
  })
  
  columnChoices <- reactive({ # Reactive function to load combo choices based on current data 
    setNames(as.list(c(NA, columnNames())), c("Not specified", columnNames())) # create list (objects and actual names) for combo choices based on the reactive rv$data columnnames  
  })
  
  lapply(1:nCols, function(i) {
    output[[paste0("colSelect", i)]] <- renderUI({ # give the output names colSelect1 - 12 with renderUI
      selectInput( # function creates combobox
        inputId = columnValidation$InputId[i], # InputIds coming from the columnvalidation csv - df
        label = columnValidation$Description[i], # description coming from the columnvalidation csv - df (top of combobox)
        choices = columnChoices(), # populate combo choices using columnchoices function above based on user data input
        selected = columnValidation$Column[i] #show the selected column name from the columnvalidation csv in case it exists in the choices otherwise = not specified 
      )
    })
  })
  
  
  
  # Column validation messages ----
  
  lapply(1:nCols, function(i) {
    
    outCol <- paste0("colValidate", i) # give names to outCol = colValidate1 - colValidate12
    inputId <- columnValidation$InputId[i] # give inputIds from column validation df to inputId vector [1] to [12] = TrtId, TrtDesc and so on...
    expectedType <- columnValidation$Type[i] # give types from column validation df to expectedType vector [1] to [12] = null or numeric - max and min below  
    minValue <- columnValidation$Min[i]
    maxValue <- columnValidation$Max[i]
    ui <- NULL
    msg <- NULL
    add <- NULL
    
    output[[outCol]] <- renderUI({
      
      req(input[[inputId]])
      
      # check if no column selected
      if (input[[inputId]] == "NA") {
        msg <- list(span("No column specified.", style = "color:orange"))
        rv$colStatus[[paste0("col", i)]] <- F
        return(msg)
      }
      
      # sel <- input[[inputId]] # Temp
      
      col <- rv$data[[input[[inputId]]]]
      
      if (anyNA(col)) { # check for presence of any NA values in the whole data column
        msg <- list(br(), span("Warning: Missing value in data", style = "color:red"))
      }
      
      colType <- class(col)
      ui <- list(paste("Type =", colType))
      
      # column type mismatch
      if (!is.na(expectedType) & colType != expectedType) {
        newmsg <- list(br(), span("Error: Incorrect column type, expected", expectedType, style = "color:red"))
        msg <- append(msg, newmsg)
        
      } else if (colType == "numeric") {
        
        # min check
        if(!is.na(minValue) & min(col, na.rm = TRUE) < minValue) {
          newmsg <- list(br(), span("Error: Min value less than", minValue, style = "color:red"))
          msg <- append(msg, newmsg)
          add <- list(
            br(),
            paste0("Min = ", round(min(col, na.rm = TRUE), 2))
          )
        }
        
        # max check
        if(!is.na(maxValue) & max(col, na.rm = TRUE) > maxValue) {
          newmsg <- list(br(), span("Error: Max value greater than ", maxValue, style = "color:red"))
          msg <- append(msg, newmsg)
          add1 <- paste0("Max = ", round(max(col, na.rm = TRUE), 2))
          add <- append(add, list(
            br(),
            "Max = ", round(max(col, na.rm = TRUE), 2))
          )
        }
        
        ui <- append(ui, add) }
      
      if (is.null(msg)) { # in case no message was filed (no error found), output OK and give T value to colStatus[col1] - [12]
        msg <- list(span(strong("OK"), style = "color:blue"))
        rv$colStatus[[paste0("col", i)]] <- T
        
      } else {
        rv$colStatus[[paste0("col", i)]] <- F # False if msg has content (error found)
      }
      
      list(p(ui), p(msg)) #temp , p(sel)
      
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
  checkModelReadiness <- function(col) { # get the full column for each model (col) from the columnn-validation csv df
    compare <- sapply(1:nCols, function(i) { # for each rows in that column  
      test <- (col[i] == T & rv$colStatus[[paste0("col", i)]] == T) | (col[i] == F) # check if the row has a T (model needs that parameter) and the parameter is correct (rv$colStatus[colnumber] = T) or row has a F 
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
  
  
  # Update visible tabs based in the model readiness
  observeEvent(TTModelReady(), {
    if (TTModelReady ()) {
      showTab(inputId = "tabs", target = "Thermal time")
    } else {
      hideTab(inputId = "tabs", target = "Thermal time")
    }
  })
  
  observeEvent(HTModelReady(), {
    if (HTModelReady ()) {
      showTab(inputId = "tabs", target = "Hydrotime")
    } else {
      hideTab(inputId = "tabs", target = "Hydrotime")
    }
  })
  
  observeEvent(HTTModelReady(), {
    if (HTTModelReady ()) {
      showTab(inputId = "tabs", target = "Hydrothermal time")
    } else {
      hideTab(inputId = "tabs", target = "Hydrothermal time")
    }
  })
  
  observeEvent(AgingModelReady(), {
    if (AgingModelReady ()) {
      showTab(inputId = "tabs", target = "Aging")
    } else {
      hideTab(inputId = "tabs", target = "Aging")
    }
  })
  
  observeEvent(PromoterModelReady(), {
    if (PromoterModelReady ()) {
      showTab(inputId = "tabs", target = "Promoter")
    } else {
      hideTab(inputId = "tabs", target = "Promoter")
    }
  })
  
  observeEvent(InhibitorModelReady(), {
    if (InhibitorModelReady ()) {
      showTab(inputId = "tabs", target = "Inhibitor")
    } else {
      hideTab(inputId = "tabs", target = "Inhibitor")
    }
  })
  
  observeEvent(HPModelReady(), {
    if (HPModelReady ()) {
      showTab(inputId = "tabs", target = "Hydropriming")
    } else {
      hideTab(inputId = "tabs", target = "Hydropriming")
    }
  })
  
  observeEvent(HTPModelReady(), {
    if (HTPModelReady ()) {
      showTab(inputId = "tabs", target = "Hydrothermal priming")
    } else {
      hideTab(inputId = "tabs", target = "Hydrothermal priming")
    }
  })
  

  
  # Model readiness display ----
  output$modelStatus <- renderUI({
    
    status <- function(x) { # receive model validation status
      if (x) {
        span(strong("Ready"), style = "color:blue") # if T is ready
      } else {
        span(strong("Not ready"), "- required columns missing", style = "color:red") # if F not ready
      }
    }
    
    list(
      p(
        "Basic plots:", status(BasicDataReady()), br(),
        "Thermal time model:", status(TTModelReady()), br(),
        "Hydrotime model:", status(HTModelReady()), br(),
        "Hydrothermal time model:", status(HTTModelReady()), br(),
        "Aging model:", status(AgingModelReady()), br(),
        "Promoter model:", status(PromoterModelReady()), br(),
        "Inhibitor model:", status(InhibitorModelReady()), br(),
        "Hydropriming model:", status(HPModelReady()), br(),
        "Hydrothermal priming model:", status(HTPModelReady())
      )
    )
    
  })
  
  # Update visible tabs based in Model readiness
  
  observeEvent(input$hideTab, {
    status <- function(x) { # receive model validation status
      if (x) {
        TRUE  # if T is ready
      } else {
        FALSE # if F not ready
      }
    }
    
  if (status(TTModelReady())) { showTab(inputId = "tabs", target = "Thermal time")
  } else { hideTab(inputId = "tabs", target = "Thermal time")
  }
  
  if (status(HTModelReady())) { showTab(inputId = "tabs", target = "Hydrotime")
  } else { hideTab(inputId = "tabs", target = "Hydrotime")
  }

  if (status(HTTModelReady())) { showTab(inputId = "tabs", target = "Hydrothermal time")
  } else { hideTab(inputId = "tabs", target = "Hydrothermal time")
  }
    
  if (status(AgingModelReady())) { showTab(inputId = "tabs", target = "Aging")
  } else { hideTab(inputId = "tabs", target = "Aging")
  }
  
  if (status(PromoterModelReady())) { showTab(inputId = "tabs", target = "Promoter")
  } else { hideTab(inputId = "tabs", target = "Promoter")
    }
  
  if (status(InhibitorModelReady())) { showTab(inputId = "tabs", target = "Inhibitor")
  } else { hideTab(inputId = "tabs", target = "Inhibitor")
  }
    
  if (status(HPModelReady())) { showTab(inputId = "tabs", target = "Hydropriming")
  } else { hideTab(inputId = "tabs", target = "Hydropriming")
  }
  
  if (status(HTPModelReady())) { showTab(inputId = "tabs", target = "Hydrothermal priming")
  } else { hideTab(inputId = "tabs", target = "Hydrothermal priming")
  }
    
  })
  
  
  
  
  
  
  # Germination Plot and Speed ----
  
  output$germUI <- renderUI({
    validate(  #check if current data has basic requirements
      need(BasicDataReady(), "Please load a dataset and set required column types for germination analysis.")
    )
    list(
      h4("Cumulative germination:"),
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
      h4("Germination speed parameters:"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("germSpeedTrts"),
          uiOutput("germSpeedFracs")
          #    uiOutput("germSpeedType")
        ),
        mainPanel(
          div(
            dataTableOutput("germSpeedTable"),
            style = "overflow-x: auto"
          )
        )
      ),
      br(),
      h4("Germination rates over treatments: (in progress)"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("germRateTreat")
        ),
        mainPanel(
          div(
            plotOutput("germRateTrtPlot")
          )
        )
      )
    )
  })
  
  ## Germination rate plots -----
  
  # Treatment SelectInputs ---    
  
  germTrtChoices <- reactive({ # create choices with validated column names and factors 
    cols <- sapply(1:nCols, function(i) {
      if (rv$colStatus[[paste0("col", i)]] == T && columnValidation$Role[i] == "Factor") columnValidation$Column[i] # TODO: need to fix this to display users column names instead otherwise error with custom column names. 
    })
    cols <- compact(cols) #remove all null entries 
    setNames(as.list(c(NA, cols)), c("Not specified", cols))
  })
  
  output$germPlotTrt1 <- renderUI({
    # req(BasicDataReady()) #check if current data has basic requirements
    list(
      selectInput( #create combo box with validated columns and factors
        "germPlotTrt1", 
        "Treatment 1 (color)",
        choices = germTrtChoices()
      )
    )
  })
  
  output$germPlotTrt2 <- renderUI({
    req(input$germPlotTrt1) #check if combo treatment is filled
    req(input$germPlotTrt1 != "NA") #check if combo treatment is not NA
    list(
      selectInput(  #create combo box with validated columns and factors
        "germPlotTrt2",
        "Treatment 2 (shape)",
        choices = germTrtChoices()
      )
    )
  })
  
  # Germination rate plot ---
  
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
    
    if (trts == 1) { # setup plot data if treatment 1 informed only
      
      df <- df %>%
        group_by(Trt1, CumTime) %>%
        summarise(FracDiff = sum(FracDiff), .groups = "drop_last") %>%
        mutate(CumFrac = cumsum(FracDiff) / sum(FracDiff))
      
      plt <- df %>%
        ggplot(aes(x = CumTime, y = CumFrac, color = Trt1)) +
        geom_line() +
        geom_point(shape = 19, size = 2) +
        labs(color = input$germPlotTrt1)
      
    } else if (trts == 2) { # setup plot data if both treatments were informed
      
      df <- df %>%
        group_by(Trt1, Trt2, CumTime) %>%
        summarise(FracDiff = sum(FracDiff), .groups = "drop_last") %>%
        mutate(CumFrac = cumsum(FracDiff) / sum(FracDiff))
      
      plt <- df %>%
        ggplot(aes(x = CumTime, y = CumFrac, color = Trt1, shape = Trt2)) +
        geom_line() +
        geom_point(size = 2) +
        labs(color = input$germPlotTrt1, shape = input$germPlotTrt2)
      
    } else { # setup plot data if no treatments were informed on combos
      
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
    
    lines = seq(0, 1, by = input$germSpeedRes / 100)
    plt <- plt + geom_hline(yintercept = lines, color = "grey", size = 0.25, alpha = 0.5, linetype = "dashed")
    
    plt
  })
  
  
  ## Germination speed parameters ----
  
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
      label = "Germination (%) basis",
      min = 10,
      max = 100,
      step = 10,
      value = 50
    )
  })
  
  # TODO: I should have a second reactive dataset that includes only renamed columns from the primary dataset. That would simplify the references to that data.
  
  output$germSpeedTable <- renderDataTable({
    req(input$germSpeedRes)
    #req(input$germSpeedType)
    
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
    ### TODO: Is this dropping values at the same timepoint? We actually need to drop the last value of similar fraction that occur at different time points (no increase in germination since earlier observation) that is the function of the cleandata function from the pbtm package 
    df <- df %>%
      group_by(CumTime, .add = T) %>%
      summarise(FracDiff = sum(FracDiff), .groups = "drop_last") %>%
      mutate(CumFrac = cumsum(FracDiff) / sum(FracDiff))
    
    # interpolate the curves to get the estimated value at given fraction
    df <- df %>%
      arrange(CumTime) %>%
      summarise(
        {
          #df <- approx(CumFrac, CumTime, xout = seq(0, 1, by = input$germSpeedRes / 100), ties = "ordered", rule = 2) # rule 2 returns the closest data which can be misleading, better return NA with rule = 1
          df <- approx(CumFrac, CumTime, xout =  (input$germSpeedRes / 100), ties = "ordered", rule = 1)
          names(df) <- c("Frac", "Time") # saving x (Frac) and y (Time) 
          df <- as_tibble(df)
          drop_na(df)
        },
        .groups = "drop"
      )
    
    # Calculation of the time to desired germ % and respective rate.
    #if (input$germSpeedType == "Rate") {
    # show as rate
    df <- df %>%
      mutate(
        #Time = round(Frac / Time * 100, 2),
        Tx = paste0("T", Frac * 100),
        GRx = paste0("GR", Frac * 100),
        Time = round(Time, 1),
        Frac = round(1 / Time, 3)) %>%
      pivot_wider(
        names_from = c(Tx, GRx), # TODO: Need to fix these labels...
        values_from = c("Time", "Frac")
      )
     
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
  
  ## Germination rate plot against treatment ----
  
  germTrtChoicesRate <- reactive({ # create choices with validated column namesand numeric factors 
    colsRt <- sapply(1:nCols, function(i) {
      if (rv$colStatus[[paste0("col", i)]] == T && columnValidation$Role[i] == "Factor"  && !is.na(columnValidation$Type[i])) columnValidation$Column[i] # TODO: need to fix this to display users column names instead otherwise error with custom column names. 
    })
    colsRt <- compact(colsRt) #remove all null entries 
    setNames(colsRt, colsRt)
  })
  
  # construct working dataset
  output$germRateTreat <- renderUI({
    radioButtons(
      inputId = "germSpeedRateTrtSelect",
      label = "Select treatment for x axis:",
      choices = germTrtChoicesRate()
    )
    
  })
  
  # Plot selected GRx (from the section above) in the y-axis and the numeric factor in the x axis based in the radiobutton germSpeedRateTrtSelect

   output$germRateTrtPlot <- renderPlot({
    req(nrow(rv$data) > 0)
   req(BasicDataReady())
   
  # TODO: how to get the values from the table in the section above?
  pltRt <- df() %>%
    ggplot(aes(x = input$germSpeedRateTrtSelect, y = Frac)) +
    geom_point(shape = 19, size = 2) +
  labs(
    title = "Germination Rate Vs bbb",
    x = "Temp",
    y = "GRx") +
  theme_classic()
  
  pltRt
   })
  
  # Thermal time TAB -------------------
   
   # Treatment SelectInputs ---    
   
   germTrtThermalChoices <- reactive({ # create choices with validated column names and factors 
     cols <- sapply(1:nCols, function(i) {
       if (rv$colStatus[[paste0("col", i)]] == T && columnValidation$Role[i] == "Factor") columnValidation$Column[i] # TODO: need to fix this to display users column names instead otherwise error with custom column names. 
     })
     cols <- compact(cols) #remove all null entries 
     setNames(as.list(c(NA, cols)), c("Not specified", cols))
   })
   
   output$germThermalPlotTrt1 <- renderUI({
     # req(BasicDataReady()) #check if current data has basic requirements
     list(
       selectInput( #create combo box with validated columns and factors
         "germPlotTrt1", 
         "Treatment 1 (color)",
         choices = germTrtThermalChoices()
       )
     )
   })
   

   
   output$ThermaltimeUI <- renderUI({
     validate(  #check if current data has basic requirements
       need(TTModelReady(), "Please load a dataset and set required column types for the thermal time model analysis.")
     )
     list(
       h4("Thermal time model calculation:"),
       sidebarLayout(
         sidebarPanel(
           uiOutput("germThermalPlotTrt1"),
           uiOutput("germSpeedTrts")
         ),
         mainPanel(
           plotOutput("germPlot")
         )
       ),
       br(),
       h4("Germination speed parameters:"),
       sidebarLayout(
         sidebarPanel(
           #uiOutput("germSpeedTrts"),
           #uiOutput("germSpeedFracs")
           #    uiOutput("germSpeedType")
         ),
         mainPanel(
           div(
             dataTableOutput("germSpeedTable"),
             style = "overflow-x: auto"
           )
         )
       ),
       br(),
       h4("Germination rates over treatments: (in progress)"),
       sidebarLayout(
         sidebarPanel(
           uiOutput("germRateTreat")
         ),
         mainPanel(
           div(
             plotOutput("germRateTrtPlot")
           )
         )
       )
     )
   })
  
}


## Germination speed ----

#    output$PlotRateVsTreat <- renderPlot({ #temp
#     req(nrow(rv$data) > 0)
#     req(TTModelReady())
#     df <- rv$data # get all the user data
#     # revert Time and fraction column names to Cumtime and CumFraction
#     if (input$CumTime != "CumTime") {
#     names(df)[names(df) == input$CumTime] <- "CumTime" }
#     if (input$CumFraction != "CumFraction") {
#     names(df)[names(df) == input$CumFraction] <- "CumFraction" }
#     gr <- input$GRInput / 100
#     t1 <- input$GermWP # germ wp
#     t2 <- input$GermTemp # germ wp
#     try(speed <- calcspeed(df, gr, t1, t2))
#     # try(PlotRateVsTreat(speed, t2, paste0("GR", gr * 100)))
#   })

#   output$SpeedTbl <- renderTable({
#     df <- rv$data
#     gr <- input$GRInput / 100
#     t1 <- input$GermWP # germ wp
#     t2 <- input$GermTemp
#     try(speed <- calcspeed(df, gr, t1, t2))
#     speed})



#}

