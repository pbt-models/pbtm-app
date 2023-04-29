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
    uiOutput(ns("currentDataDisplay"))
  )
}


# Helpers ----




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
      
      rv <- reactiveValues(
        data = tibble(),
        colStatus = NULL
      )

      dataReady <- reactive({
        nrow(rv$data) > 0
      })

      
      ## Load sample data buttons ----
      
      observeEvent(input$loadSampleGermData, {
        rv$data <- sampleGermData
      })
      
      observeEvent(input$loadSamplePrimingData, {
        rv$data <- samplePrimingData
      })
      
      observeEvent(input$loadSampleAgingData, {
        rv$data <- sampleAgingData
      })
      
      observeEvent(input$userData, {
        try({
          df <- read_csv(input$userData$datapath, col_types = cols(), progress = F) %>%
            distinct()
          if (nrow(df) > 0) {
            rv$data <- df
          }
        })
      })
      
      observeEvent(input$clearData, {
        rv$data <- tibble()
        reset(ns("userData")) # reset file input
      })
      
      
      ## currentDataTable ----
      
      output$currentDataTable <- renderDataTable({rv$data})
      
      
      ## currentDataDisplay ----
      
      output$currentDataDisplay <- renderUI({
        validate(need(dataReady(), "Please load a dataset."))
        
        tagList(
          h3("Current dataset:"),
          div(style = "overflow: auto;", dataTableOutput("currentDataTable")),
          hr(),
          h3("Match column names to expected roles:"),
          p(em("If you used the same column names as the default data template, they will be automatically matched below. Otherwise, cast your column names into the appropriate data types. Warning messages will appear if your data doesn't match the expected type or range.")),
          div(style = "display: flex; flex-wrap: wrap;",
            lapply(1:nCols, function(i) {
              wellPanel(
                style = "flex: 1; vertical-align: top; min-width: 15em; margin: 5px;",
                uiOutput(paste0(ns("colSelect"), i)),
                uiOutput(paste0(ns("colValidate"), i))
              )
            })
          )
        )
      })
      
      
      ## columnNames ----
      
      columnNames <- reactive({
        names(rv$data)
      })
      
      
      ## columnChoices ----
      
      columnChoices <- reactive({
        setNames(
          as.list(c(NA, columnNames())),
          c("Not specified", columnNames())
        )
      })
      
      
      ## colSelect ----
      
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
      
      
      ## colValidate ----
      
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
      
      
      # Return data and validation ----
      # rv$data
      # rv$colStatus
      return(reactive(rv))
      
    }
  )
}
