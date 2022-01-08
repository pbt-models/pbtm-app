# ---- Server ---- #

library(dplyr)
library(tidyverse)
library(shiny)
library(shinyjs)
library(DT)
library(pbtmodels)

# remove.packages("pbtm")

#Install required package for GitHub repository
  # install.packages("plyr") # for some reason needed to install pbtm... (CHECK)
  # install.packages("dplyr")
  # remove.packages("ggplot2")
  # install.packages("ggplot2", force = TRUE, dependencies = TRUE)
  # devtools::install_github("pedrobello/pbtm", force = TRUE, dependencies = TRUE)
  # devtools::install_github("pedrobello/pbtm", dependencies = TRUE)
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
    rv$data <- read_csv(input$userData$datapath, col_types = cols()) # load data uploaded from user 
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
  
  
  # Plot drafts ----
  
  output$germPlotTrt1 <- renderUI({
    req(BasicDataReady()) #check if current data has basic requirements
    list(
      selectInput( #create combo box with available models
        "germPlotTrt1",
        "Treatment 1 (color)",
        choices = columnChoices() 
      )
    )
  })
  
  output$germPlotTrt2 <- renderUI({
    req(BasicDataReady()) #check if current data has basic requirements
    req(input$germPlotTrt1) #check if combo treatment is filled
    req(input$germPlotTrt1 != "NA") #check if combo treatment is not NA
    list(
      selectInput( #create combo box with available models
        "germPlotTrt2",
        "Treatment 2 (shape)",
        choices = columnChoices()
      )
    )
  })
  
  output$germPlot <- renderPlot({
    req(nrow(rv$data) > 0)
    req(BasicDataReady())
    
    trts <- 0
    
    df <- tibble(
      TrtId = as.factor(rv$data[[input$TrtId]]),
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
    
    if (trts == 0) { # setup plot data if no treatments were informed on combos
      plt <- df %>%
        group_by(TrtId) %>%
        arrange(TrtId, CumTime, CumFrac) %>%
        mutate(FracDiff = CumFrac - lag(CumFrac, default = 0)) %>%
        group_by(CumTime) %>%
        summarise(FracDiff = sum(FracDiff), .groups = "drop_last") %>%
        mutate(CumFrac = cumsum(FracDiff) / sum(FracDiff)) %>%
        ggplot(aes(x = CumTime, y = CumFrac)) +
        geom_line() +
        geom_point(size = 2)
    } else if (trts == 1) { # setup plot data if treatment 1 informed only
      plt <- df %>%
        group_by(TrtId) %>%
        arrange(TrtId, CumTime, CumFrac) %>%
        mutate(FracDiff = CumFrac - lag(CumFrac, default = 0)) %>%
        group_by(Trt1, CumTime) %>%
        summarise(FracDiff = sum(FracDiff), .groups = "drop_last") %>%
        mutate(CumFrac = cumsum(FracDiff) / sum(FracDiff)) %>%
        ggplot(aes(x = CumTime, y = CumFrac, color = Trt1)) +
        geom_line() +
        geom_point(shape = 19, size = 2) +
        labs(color = input$germPlotTrt1)
    } else if (trts == 2) { # setup plot data if both treatments were informed
      plt <- df %>%
        group_by(TrtId) %>%
        arrange(TrtId, CumTime, CumFrac) %>%
        mutate(FracDiff = CumFrac - lag(CumFrac, default = 0)) %>%
        group_by(Trt1, Trt2, CumTime) %>%
        summarise(FracDiff = sum(FracDiff), .groups = "drop_last") %>%
        mutate(CumFrac = cumsum(FracDiff) / sum(FracDiff)) %>%
        ggplot(aes(x = CumTime, y = CumFrac, color = Trt1, shape = Trt2)) +
        geom_line() +
        geom_point(size = 2) +
        labs(color = input$germPlotTrt1, shape = input$germPlotTrt2)
    }
    
    plt +
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
  

     output$PlotRateVsTreat <- renderPlot({ #temp
     req(nrow(rv$data) > 0)
     req(TTModelReady())
     df <- rv$data # get all the user data
     # revert Time and fraction column names to Cumtime and CumFraction
     if (input$CumTime != "CumTime") {
     names(df)[names(df) == input$CumTime] <- "CumTime" }
     if (input$CumFraction != "CumFraction") {
     names(df)[names(df) == input$CumFraction] <- "CumFraction" }
     gr <- input$GRInput / 100
     t1 <- input$GermWP # germ wp
     t2 <- input$GermTemp # germ wp
     try(speed <- CalcSpeed(df, gr, t1, t2))
     try(PlotRateVsTreat(speed, t2, paste0("GR", gr * 100)))
   })
  
   output$SpeedTbl <- renderTable({
     df <- rv$data
     gr <- input$GRInput / 100
     t1 <- input$GermWP # germ wp
     t2 <- input$GermTemp
     try(speed <- CalcSpeed(df, gr, t1, t2))
     speed})
   
     
     
}

