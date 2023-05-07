# ---- Load data tab ---- #

# Static UI ----

LoadDataUI <- function() {
  ns <- NS("loadData")
  
  tagList(
    h3("Upload data", class = "tab-title"),
    p(em("Upload your own data here or select one of our sample datasets to get started.")),
    br(),
    p(strong("Expected column names and descriptions:")),
    bsCollapse(
      bsCollapsePanel(
        title = "Show/hide column descriptions",
        tableOutput(ns("columnDescriptions"))
      )
    ),
    p(strong("Sample datasets:")),
    div(
      class = "flex-btns",
      actionButton(ns("loadSampleGermData"), "Load germination sample data"),
      actionButton(ns("loadSamplePrimingData"), "Load priming sample data"),
      actionButton(ns("loadSampleAgingData"), "Load aging sample data")
    ),
    br(),
    p(strong("Upload your own data (csv):")),
    div(
      class = "flex-btns",
      fileInput(
        inputId = ns("userData"),
        label = NULL,
        accept = c(".csv")
      ),
      div(
        style = "height: min-content;",
        actionButton(ns("clearData"), "Clear loaded data")
      )
    ),
    uiOutput(ns("currentDataUI"))
  )
}


# Server ----

#' @references sampleGermData
#' @references samplePrimingData
#' @references sampleAgingData

LoadDataServer <- function() {
  moduleServer(
    id = "loadData",
    function(input, output, session) {
      ns <- session$ns
      
      
      # Reactives ----
      
      # data // raw data before cleaning or assigning columns ----
      rawData <- reactiveVal(tibble())
      
      rv <- reactiveValues(
        colStatus = NULL
      )
      
      columnNames <- reactive({
        names(rawData())
      })
      
      columnChoices <- reactive({
        setNames(
          as.list(c(NA, columnNames())),
          c("Not specified", columnNames())
        )
      })
      
      cleanData <- reactive({
        if ((nrow(rawData()) > 0) & (length(rv$colStatus) == nCols)) {
          
          # collect user column names
          vars <- sapply(colValidation$InputId, \(id) { input[[id]] })
          names(vars) <- colValidation$Column
          vars <- vars[vars != "NA"]
          
          rawData() %>%
            select(any_of(vars)) %>%
            rename(any_of(vars))
        } else {
          tibble()
        }
      }) %>% bindEvent(rv$colStatus)
      
      modelReady <- reactive({
        ready <- lapply(modelNames, function(m) {
          checkModelReady(colValidation[[m]], rv$colStatus)
        })
        names(ready) <- modelNames
        ready
      })

      
      # Button handlers ----
      
      ## Load sample data ----
      observeEvent(input$loadSampleGermData, rawData(sampleGermData))
      observeEvent(input$loadSamplePrimingData, rawData(samplePrimingData))
      observeEvent(input$loadSampleAgingData, rawData(sampleAgingData))
      
      ## Load user data ----
      observeEvent(input$userData, {
        try({
          df <- read_csv(
            input$userData$datapath,
            col_types = cols(),
            progress = F) %>%
            distinct()
          if (nrow(df) > 0) rawData(df)
        })
      })
      
      ## Clear data button ----
      observeEvent(input$clearData, {
        rawData(tibble())
        rv$colStatus <- NULL
        reset("userData") # reset file input
      })
      
      
      # Outputs ----
      
      ## currentDataUI // Data display and validation when data loaded ----
      output$currentDataUI <- renderUI({
        validate(need(nrow(rawData()) > 0, "Please load a dataset."))
        
        tagList(
          h3("Currently loaded data:"),
          bsCollapse(
            open = "tab",
            bsCollapsePanel(
              title = "Show/hide data table",
              value = "tab",
              div(
                class = "tbl-container margin-10",
                dataTableOutput(ns("currentDataTable"))
              )
            )
          ),
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
          ),
          h3("Final clean dataset for models:"),
          bsCollapse(
            open = "tab",
            bsCollapsePanel(
              title = "Show/hide data table",
              value = "tab",
              div(
                class = "tbl-container margin-10",
                dataTableOutput(ns("cleanDataTable"))
              )
            )
          )
        )
      })
      
      ## columnDescriptions // table showing column descriptions ----
      output$columnDescriptions <- renderTable({
        colValidation %>%
          select(
            `Default name` = Column,
            Description = LongDescription,
            `Data type` = TypeDescription,
            Usage
          )
      },
        spacing = "s"
      )
      
      ## currentDataTable // data as it was uploaded----
      output$currentDataTable <- renderDataTable(rawData())
      
      ## cleanDataTable // Shows data passed to models after column matching ----
      output$cleanDataTable <- renderDataTable(cleanData())
      
      
      ## colSelect[i] // Renders the selectInput boxes for each column ----
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
      
      
      ## colValidate[i] // Validation messages for each column ----
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
            col <- rawData()[[input[[inputId]]]]
            validation <- validateCol(col, expectedType, minValue, maxValue)
            rv$colStatus[i] <- validation$valid
            validation$ui
          }
        })
      })
      
      
      # Return values ----

      return(reactive(list(
        data = cleanData(),
        colStatus = rv$colStatus,
        modelReady = modelReady()
      )))
  
    } # end
  )
}
