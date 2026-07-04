# ---- Load data tab ---- #

# Functions ----

#' @description checks a data column and returns whether it's valid and a message about it
#' @param col data vector to evaluate
#' @param expectedType checks that `col` is this type
#' @param minValue if `col` is numeric, checks this constraint
#' @param maxValue if `col` is numeric, checks this constraint
#' @returns boolean
validateCol <- function(col, expectedType, minValue, maxValue) {
  span_red <- \(...) span(style = "color: red;", ...)
  info <- NULL
  msg <- NULL
  valid <- FALSE

  if (anyNA(col)) {
    msg <- list(
      span_red("Warning: Missing value in data"),
      br()
    )
  }

  colType <- class(col)
  info <- list(paste("Type:", colType), br())

  # column type mismatch
  if (!is.na(expectedType) & colType != expectedType) {
    msg <- append(
      msg,
      list(
        span_red("Error: Incorrect column type, expected", expectedType),
        br()
      )
    )
  }

  # Numeric value checks
  if (colType == "numeric") {
    info <- append(
      info,
      list(
        paste0("Range: ", round(min(col), 2), " 🡒 ", round(max(col), 2)),
        br()
      )
    )

    # min check
    if (!is.na(minValue) & min(col) < minValue) {
      msg <- append(
        msg,
        list(
          span_red("Error: Min value less than", minValue),
          br()
        )
      )
    }

    # max check
    if (!is.na(maxValue) & max(col) > maxValue) {
      msg <- append(
        msg,
        list(
          span_red("Error: Max value greater than ", maxValue),
          br()
        )
      )
    }
  }

  # valid if no messages yet
  if (is.null(msg)) {
    msg <- list(span(strong("OK"), style = "color: blue;"))
    valid <- TRUE
  } else {
    msg <- head(msg, -1) # remove the last br()
  }

  list(
    valid = valid,
    ui = tagList(info, msg)
  )
}

#' @description checks that all required columns are validated for each model
#' @param requirements boolean vector of column requirements
#' @param statuses boolean vector of current column readiness statuses
#' @returns T/F if statuses satisfy requirements
checkModelReady <- function(requirements, statuses) {
  compare <- sapply(1:nCols, function(i) {
    test <-
      (requirements[i] == TRUE & statuses[i] == TRUE) |
      (requirements[i] == FALSE)
    ifelse(length(test) == 0, FALSE, test)
  })
  all(compare)
}

# Static UI ----

loadDataUI <- function() {
  ns <- NS("load_data")

  layout_columns(
    col_widths = 12,

    tabHeader(
      title = "Upload data",
      subtitle = "Proper data preparation is required to avoid issues when working with this site or the PBTM package. The use of these data templates is not required, but if you use different column names you will have to match them to the expected names after uploading your data. Columns that specify treatment information (e.g. TrtID, GermWP) need to have the value of that treatment repeated for each member of the treatment. Each row (observation) also needs to have a value for CumTime (cumulative elapsed time) and CumFraction (fraction germinated as of that time point, ranges from 0 to 1)."
    ),

    accordion(
      open = FALSE,
      accordion_panel(
        title = "Templates and instructions",
        div(
          h3("Data templates:"),
          div(
            class = "flex-btns",
            downloadButton(ns("downloadTemplate"), "Empty data template"),
            downloadButton(
              ns("downloadGermData"),
              "Sample germination dataset"
            ),
            downloadButton(ns("downloadPrimingData"), "Sample priming dataset"),
            downloadButton(ns("downloadAgingData"), "Sample aging dataset")
          )
        ),
        div(
          h3("Expected column names and descriptions:"),
          tableOutput(ns("columnDescriptions"))
        )
      )
    ),

    div(
      h3("Load example datasets:"),
      div(
        class = "flex-btns",
        local({
          btns <- list(
            "load_germ" = "Germination data",
            "load_priming" = "Priming data",
            "load_aging" = "Aging data",
            "load_promoter" = "Promoter data",
            "load_inhibitor" = "Inhibitor data"
          )
          btn <- function(id, label) {
            actionButton(
              inputId = ns(id),
              label = label,
              icon = icon("arrow-up"),
              class = "btn-default"
            )
          }
          lapply(names(btns), function(nm) {
            btn(nm, btns[[nm]])
          })
        })
      ),
    ),

    div(
      h3("Upload your own data (csv):"),
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
    ),

    div(
      h3("Currently loaded data:"),
      uiOutput(ns("currentDataUI")),
    ),
  )
}


# Server ----

#' @references sampleGermData
#' @references samplePrimingData
#' @references sampleAgingData

loadDataServer <- function() {
  moduleServer(
    id = "load_data",
    function(input, output, session) {
      ns <- session$ns

      # Reactives ----

      # data // raw data before cleaning or assigning columns ----
      rv <- reactiveValues(
        raw_data = tibble(),
        col_status = NULL
      )

      columnNames <- reactive({
        names(rv$raw_data)
      })

      columnChoices <- reactive({
        setNames(
          as.list(c(NA, columnNames())),
          c("Not specified", columnNames())
        )
      })

      cleanData <- reactive({
        if ((nrow(rv$raw_data) > 0) & (length(rv$col_status) == nCols)) {
          # collect user column names
          vars <- sapply(colValidation$InputId, function(id) input[[id]])
          names(vars) <- colValidation$Column
          vars <- vars[vars != "NA"]

          df <- rv$raw_data |>
            select(any_of(vars)) |>
            rename(any_of(vars))

          if (all(c("TrtID", "TrtDesc") %in% names(df))) {
            df <- mutate(
              df,
              TrtLabel = str_trunc(sprintf("%s: %s", TrtID, TrtDesc), 30),
              .after = "TrtDesc"
            )
          }

          df
        } else {
          tibble()
        }
      }) |>
        bindEvent(rv$col_status)

      modelReady <- reactive({
        ready <- lapply(modelNames, function(m) {
          checkModelReady(colValidation[[m]], rv$col_status)
        })
        names(ready) <- modelNames
        ready
      })

      # Button handlers ----

      ## Load sample data ----
      observeEvent(input$load_germ, {
        rv$raw_data <- sampleGermData
      })
      observeEvent(input$load_priming, {
        rv$raw_data <- samplePrimingData
      })
      observeEvent(input$load_aging, {
        rv$raw_data <- sampleAgingData
      })
      observeEvent(input$load_promoter, {
        rv$raw_data <- samplePromoterData
      })
      observeEvent(input$load_inhibitor, {
        rv$raw_data <- sampleInhibitorData
      })

      ## Load user data ----
      observeEvent(input$userData, {
        try({
          df <- read_csv(
            input$userData$datapath,
            col_types = cols(),
            progress = F
          ) |>
            distinct()
          if (nrow(df) > 0) rv$raw_data <- df
        })
      })

      ## Clear data button ----
      observeEvent(input$clearData, {
        rv$raw_data <- tibble()
        rv$col_status <- NULL
        reset("userData") # reset file input
      })

      # Download handlers ----

      ## downloadTemplate ----
      output$downloadTemplate <- downloadHandler(
        filename = "PBTM Data Template.csv",
        content = function(file) {
          write_csv(sampleTemplate, file)
        }
      )

      ## downloadGermData ----
      output$downloadGermData <- downloadHandler(
        filename = "PBTM Sample Germination Data.csv",
        content = function(file) {
          write_csv(sampleGermData, file)
        }
      )

      ## downloadPrimingData ----
      output$downloadPrimingData <- downloadHandler(
        filename = "PBTM Sample Priming Data.csv",
        content = function(file) {
          write_csv(samplePrimingData, file)
        }
      )

      ## downloadAgingData ----
      output$downloadAgingData <- downloadHandler(
        filename = "PBTM Sample Aging Data.csv",
        content = function(file) {
          write_csv(sampleAgingData, file)
        }
      )

      # Outputs ----

      ## currentDataUI // Data display and validation when data loaded ----
      output$currentDataUI <- renderUI({
        validate(need(nrow(rv$raw_data) > 0, "Please load a dataset."))

        layout_columns(
          col_widths = 12,

          # show loaded raw data
          panelCard(
            title = "Uploaded data",
            div(
              class = "tbl-container",
              dataTableOutput(ns("currentDataTable"))
            )
          ),

          # column selectors and validation messages
          div(
            h3("Match column names to expected roles:"),
            p(em(
              "If you used the same column names as the default data template, they will be automatically matched below. Otherwise, cast your column names into the appropriate data types. Warning messages will appear if your data doesn't match the expected type or range."
            )),
            div(
              class = "validation-container",
              lapply(1:nCols, function(i) {
                div(
                  id = ns(paste0("valBox", i)),
                  class = "p-3 bg-light border rounded validation-box",
                  uiOutput(paste0(ns("colSelect"), i)),
                  uiOutput(paste0(ns("colValidate"), i))
                )
              })
            ),
          ),

          # show clean data
          div(
            h3("Final clean dataset for models:"),
            panelCard(
              title = "Clean dataset for models",
              div(
                class = "tbl-container",
                dataTableOutput(ns("cleanDataTable"))
              )
            )
          )
        )
      })

      ## columnDescriptions // table showing column descriptions ----
      output$columnDescriptions <- renderTable(
        {
          colValidation |>
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
      output$currentDataTable <- renderDataTable(rv$raw_data)

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

        # Create an observer to handle validation change
        observe({
          req(input[[inputId]])
          boxSel <- paste0("#", ns(paste0("valBox", i)))
          shinyjs::removeCssClass(
            selector = boxSel,
            class = "val-ok val-warn val-error"
          )
          if (input[[inputId]] == "NA") {
            rv$col_status[i] <- FALSE
            shinyjs::addCssClass(selector = boxSel, class = "val-warn")
          } else {
            col <- rv$raw_data[[input[[inputId]]]]
            validation <- validateCol(col, expectedType, minValue, maxValue)
            rv$col_status[i] <- validation$valid
            shinyjs::addCssClass(
              selector = boxSel,
              class = if (validation$valid) "val-ok" else "val-error"
            )
          }
        })

        # Set up outputs
        output[[outCol]] <- renderUI({
          req(input[[inputId]])

          if (input[[inputId]] == "NA") {
            span("No column specified.", style = "color: orange")
          } else {
            col <- rv$raw_data[[input[[inputId]]]]
            validation <- validateCol(col, expectedType, minValue, maxValue)
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
