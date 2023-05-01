# ---- Load data tab ---- #

# UI ----

loadDataTabUI <- function() {
  ns <- NS("loadData")
  
  tagList(
    h3("Upload data", class = "tab-title"),
    p(em("Upload your own data here or select one of our sample datasets to get started.")),
    hr(),
    p(strong("Sample datasets:")),
    p(
      actionButton(ns("loadSampleGermData"), "Load germination sample data"),
      actionButton(ns("loadSamplePrimingData"), "Load priming sample data"),
      actionButton(ns("loadSampleAgingData"), "Load aging sample data")
    ),
    br(),
    p(
      fileInput(
        inputId = ns("userData"),
        "Upload your own data:",
        accept = c(".csv")
      )
    ),
    p(strong("Start over:")),
    actionButton(ns("clearData"), "Clear loaded data"),
    hr(),
    uiOutput(ns("currentDataUI"))
  )
}


# Helpers ----

validateCol <- function(col, expectedType, minValue, maxValue) {
  info <- NULL
  msg <- NULL
  valid <- FALSE
  
  if (anyNA(col)) {
    msg <- list(span("Warning: Missing value in data", style = "color:red"), br())
  }
    
  colType <- class(col)
  info <- list(paste("Type:", colType), br())
  
  # Numeric value checks
  if (colType == "numeric") {
    info <- append(info,
      list(paste0("Range: ", round(min(col), 2), " => ", round(max(col), 2)), br())
    )
    
    # column type mismatch
    if (!is.na(expectedType) & colType != expectedType) {
      msg <- append(msg,
        list(span("Error: Incorrect column type, expected", expectedType, style = "color:red"), br())
      )
    }
    
    # min check
    if (!is.na(minValue) & min(col) < minValue) {
      msg <- append(msg, 
        list(span("Error: Min value less than", minValue, style = "color:red"), br())
      )
    }
    
    # max check
    if (!is.na(maxValue) & max(col) > maxValue) {
      msg <- append(msg,
        list(span("Error: Max value greater than ", maxValue, style = "color:red"), br())
      )
    }
    
  } else if (!is.na(expectedType) & colType != expectedType) {
    newmsg <- list(span("Error: Incorrect column type, expected", expectedType, style = "color:red"), br())
    msg <- append(msg, newmsg)
  }
    
  # valid if no messages yet
  if (is.null(msg)) {
    msg <- list(span(strong("OK"), style = "color:blue"))
    valid <- TRUE
  } else {
    msg <- head(msg, -1) # remove the last br()
  }
  
  list(
    valid = valid,
    ui = tagList(info, msg)
  )
}


# Server ----

#' requires global vars:
#' - sampleGermData
#' - samplePrimingData
#' - sampleAgingData
#' - 

loadDataServer <- function() {
  moduleServer(
    id = "loadData",
    function(input, output, session) {
      ns <- session$ns
      
      
      ## Reactives ----
      
      rv <- reactiveValues(
        data = tibble(),
        colStatus = NULL
      )

      dataReady <- reactive({
        nrow(rv$data) > 0
      })
      
      columnNames <- reactive({
        names(rv$data)
      })
      
      columnChoices <- reactive({
        setNames(
          as.list(c(NA, columnNames())),
          c("Not specified", columnNames())
        )
      })

      
      ## Observers ----
      
      ### Load sample data ----
      
      observeEvent(input$loadSampleGermData, { rv$data <- sampleGermData })
      observeEvent(input$loadSamplePrimingData, { rv$data <- samplePrimingData })
      observeEvent(input$loadSampleAgingData, { rv$data <- sampleAgingData })
      
      
      ### Load user data ----
      
      observeEvent(input$userData, {
        try({
          df <- read_csv(input$userData$datapath, col_types = cols(), progress = F) %>%
            distinct()
          if (nrow(df) > 0) {
            rv$data <- df
          }
        })
      })
      
      
      ### Clear data button ----
      
      observeEvent(input$clearData, {
        rv$data <- tibble()
        reset(ns("userData")) # reset file input
      })
      
      
      
      ## Outputs ----
      
      ### currentDataUI // Data display and validation when data loaded ----
      
      output$currentDataUI <- renderUI({
        validate(need(dataReady(), "Please load a dataset."))
        
        tagList(
          h3("Current dataset:"),
          div(
            style = "overflow: auto;",
            dataTableOutput(ns("currentDataTable"))
          ),
          hr(),
          h3("Match column names to expected roles:"),
          p(em("If you used the same column names as the default data template, they will be automatically matched below. Otherwise, cast your column names into the appropriate data types. Warning messages will appear if your data doesn't match the expected type or range.")),
          div(
            class = "validation-container",
            lapply(1:nCols, function(i) {
              div(
                class = "well validation-box",
                uiOutput(paste0(ns("colSelect"), i)),
                uiOutput(paste0(ns("colValidate"), i))
              )
            })
          )
        )
      })
      
      
      ### currentDataTable ----
      
      output$currentDataTable <- renderDataTable(rv$data)
      
      
      ### colSelect // Renders the selectInput boxes for each column ----
      
      lapply(1:nCols, function(i) {
        output[[paste0("colSelect", i)]] <- renderUI({
          selectInput(
            inputId = ns(colValidation$InputId[i]),
            label = colValidation$Description[i],
            choices = columnChoices(),
            selected = colValidation$Column[i]
          )
        })
      })
      
      
      ### colValidate // Validation messages for each column ----
      
      lapply(1:nCols, function(i) {
        
        outCol <- paste0("colValidate", i)
        inputId <- colValidation$InputId[i]
        expectedType <- colValidation$Type[i]
        minValue <- colValidation$Min[i]
        maxValue <- colValidation$Max[i]
        
        output[[outCol]] <- renderUI({
          req(input[[inputId]])
          
          if (input[[inputId]] == "NA") {
            rv$colStatus[i] <- FALSE
            span("No column specified.", style = "color: orange")
          } else {
            col <- rv$data[[input[[inputId]]]]
            validation <- validateCol(col, expectedType, minValue, maxValue)
            rv$colStatus[i] <- validation$valid
            validation$ui
          }
        })
      })
      
      
      cleanData <- reactive({
        req(nrow(rv$data) > 0)
        req(length(rv$colStatus) == nCols)
        
        # collect user column names
        vars <- sapply(colValidation$InputId, \(id) { input[[id]] })
        names(vars) <- colValidation$Column
        vars <- vars[vars != "NA"]
        
        rv$data %>%
          select(any_of(vars)) %>%
          rename(any_of(vars))
      }) %>% bindEvent(rv$colStatus)
      
      observe({print(cleanData())})
      
      
      # Returns ----
      # rv$data
      # rv$colStatus
      return(reactive(list(
        data = cleanData(),
        colStatus = rv$colStatus
      )))
  
    }
  )
}
