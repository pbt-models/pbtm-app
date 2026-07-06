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

    div(
      h2("Upload data"),
      p(
        "Proper data preparation is required to avoid issues when working with this site or the PBTM package. The use of these data templates is not required, but if you use different column names you will have to match them to the expected names after uploading your data. Columns that specify treatment information (e.g. TrtID, GermWP) need to have the value of that treatment repeated for each member of the treatment. Each row (observation) also needs to have a value for CumTime (cumulative elapsed time) and CumFraction (fraction germinated as of that time point, ranges from 0 to 1)."
      )
    ),

    navset_card_pill(
      id = ns("data_tabs"),

      nav_panel(
        title = "Data format",
        div(
          h3("Data format"),
          tableOutput(ns("columnDescriptions")),
          downloadButton(ns("download_template"), "Empty data template")
        )
      ),

      nav_panel(
        title = "Sample data",
        div(
          h3("Sample data"),
          p(
            "Load any of the sample datasets below to quickly get started and explore the models provided in this app. Some datasets will allow fitting multiple types of models, but care must be taken that all treatment variables are accounted for in the model, or filtered appropriately before fitting. For example the hydrothermal time sample data has both temperature and water potential treatment factors, so may be used for thermal time, hydro time, or hydrothermal time. If used for thermal time or hydro time, you should filter to only one level of the other factor."
          ),
          tags$ul(
            style = "padding-right: 2rem;",
            lapply(names(sample_data), function(nm) {
              s <- sample_data[[nm]]
              tags$li(
                style = "margin-bottom: 1rem;",
                div(
                  style = "display: flex; flex-direction: column; gap: 0.25rem;",
                  strong(s$title),
                  s$info,
                  if (!is.null(s$data)) {
                    div(
                      style = "display: inline-flex; gap: 5px;",
                      downloadButton(
                        ns(sprintf("download_%s", nm)),
                        "Download",
                        class = "btn-sm"
                      ),
                      actionButton(
                        inputId = ns(sprintf("load_%s", nm)),
                        label = "Load this data",
                        icon = icon("arrow-up"),
                        class = "btn-default btn-sm"
                      )
                    )
                  }
                )
              )
            })
          )
        )
      ),

      nav_panel(
        title = "Load data",
        div(
          h3("Upload your own data (csv):"),
          div(
            class = "flex-btns",
            fileInput(
              inputId = ns("user_data"),
              label = NULL,
              accept = c(".csv")
            ),
            div(
              style = "height: min-content;",
              actionButton(
                inputId = ns("clear_data"),
                label = "Clear loaded data"
              )
            )
          ),
          uiOutput(ns("currentDataUI")),
        ),
      )
    ),
  )
}


# Server -----------------------------------------------------------------------

#' @param rv reactive values object from main server
loadDataServer <- function(rv) {
  moduleServer(
    id = "load_data",
    function(input, output, session) {
      ns <- session$ns

      build_reactable <- function(df) {
        reactable(
          df,
          sortable = TRUE,
          pagination = FALSE,
          height = 400,
          searchable = FALSE,
          class = "auto-width-table"
        )
      }

      # Reactives ----

      # # data // raw data before cleaning or assigning columns ----
      # rv$raw_data <- tibble()
      # # rv <- reactiveValues(
      # #   raw_data = tibble(),
      # #   col_status = NULL
      # # )

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
      })

      # |>
      #   bindEvent(rv$col_status)

      ## Set clean data for models ----
      observe({
        rv$data <- if (truthy(cleanData())) {
          cleanData()
        } else {
          tibble()
        }
      })

      observe({
        ready <- lapply(modelNames, function(m) {
          checkModelReady(colValidation[[m]], rv$col_status)
        })
        names(ready) <- modelNames

        if (!identical(ready, rv$model_ready)) {
          rv$model_ready <- ready
        }
      })

      # Outputs ----

      ## currentDataUI // Data display and validation when data loaded ----
      output$currentDataUI <- renderUI({
        validate(need(nrow(rv$raw_data) > 0, "Please load a dataset."))

        layout_columns(
          col_widths = 12,

          # show loaded raw data
          div(
            h3("Raw data:"),
            panelCard(
              reactableOutput(ns("currentDataTable"))
            )
          ),

          # column selectors and validation messages
          div(
            h3("Data validation:"),
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
            h3("Final dataset:"),
            card(
              card_body(
                uiOutput(ns("cleanDataTable"))
              )
            )
          )
        )
      })

      # Tab 1: Data format ----

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

      ## Template download ----
      output$download_template <- downloadHandler(
        "PBTM Data Template.csv",
        function(file) write_csv(sample_template, file)
      )

      # Tab 2: Sample data ----

      ## Sample dataset loaders ----
      lapply(names(sample_data), function(nm) {
        s <- sample_data[[nm]]

        btn_id <- sprintf("load_%s", nm)
        observeEvent(input[[btn_id]], {
          rv$raw_data <- s$data
          nav_select("data_tabs", "Load data")
        })

        dl_id <- sprintf("download_%s", nm)
        output[[dl_id]] <- downloadHandler(
          filename = sprintf("PBTM %s.csv", s$title),
          content = function(file) write_csv(s$data, file)
        )
      })

      # Tab 3: Load/validate data ----

      ## Load user data ----
      observeEvent(input$user_data, {
        try({
          df <- read_csv(
            input$user_data$datapath,
            col_types = cols(),
            progress = F
          ) |>
            distinct()
          if (nrow(df) > 0) rv$raw_data <- df
        })
      })

      ## Clear data button ----
      observeEvent(input$clear_data, {
        rv$raw_data <- tibble()
        rv$col_status <- NULL
        reset("user_data") # reset file input
      })

      ## currentDataTable // data as it was uploaded----
      output$currentDataTable <- renderReactable({
        df <- req(rv$raw_data)
        validate(need(nrow(df) > 0, "Dataset must have 1 or more rows."))
        build_reactable(df)
      })

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

      ## cleanDataTable // Shows data passed to models after column matching ----
      output$cleanDataTable <- renderUI({
        df <- cleanData()
        validate(need(truthy(df), "Error: Dataset is empty!"))

        tagList(
          renderReactable({
            build_reactable(df)
          }),
          downloadButton(
            outputId = ns("downloadClean"),
            label = "Download this data",
            class = "btn-sm"
          )
        )
      })

      ## Download clean data ----
      output$downloadClean <- downloadHandler(
        filename = "PBTM Data.csv",
        content = function(file) write_csv(cleanData(), file)
      )

      # Return values ----

      # return(reactive(list(
      #   data = cleanData(),
      #   colStatus = rv$colStatus,
      #   modelReady = modelReady()
      # )))
    } # end
  )
}
