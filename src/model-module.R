# ---- Model module factory ---- #
# Generates the UI and server for any model from its spec (see model-specs.R).
# Replaces the eight near-identical tab_04..tab_11 files. Behaviour is shared;
# only the spec differs.

#' @description build the static UI for a model tab from its spec
#' @param spec a model spec
modelUI <- function(spec) {
  ns <- NS(spec$id)
  tagList(
    h3(class = "tab-title", paste(spec$label, "analysis")),
    div(class = "tab-info", spec$tabInfo),
    docPanel(spec$doc, title = paste("About the", tolower(spec$label), "model")),
    uiOutput(ns("content"))
  )
}

#' @description build the server for a model tab from its spec
#' @param spec a model spec
#' @param data reactive() clean data frame
#' @param ready reactive() boolean: model has its required columns
#' @param id module id; defaults to the spec id. Kept as the first formal (and
#'   thus called by name in production) so shiny::testServer recognises this as a
#'   module server (isModuleServer requires the first formal to be `id`).
modelServer <- function(id = spec$id, spec, data, ready) {
  moduleServer(
    id = id,
    function(input, output, session) {
      ns <- session$ns
      params <- spec$paramNames

      rv <- reactiveValues(
        setParams = setNames(as.list(rep(NA, length(params))), params),
        heldParams = setNames(as.list(rep(FALSE, length(params))), params),
        lastGoodModel = NULL
      )

      # dosage transform (promoter/inhibitor only); identity otherwise
      transformFn <- reactive({
        if (!is.null(spec$transformCol) && identical(input$dataTransfSelect, "log")) {
          log10
        } else {
          identity
        }
      })

      # Reactives ----

      ## workingData: filtered (cdf) or fraction-differenced (rate) data
      workingData <- reactive({
        req(ready())
        df <- data()

        # filter by each treatment factor's checkbox selection
        for (f in spec$factors) {
          sel <- input[[paste0(f, "Select")]]
          req(sel)
          df <- df %>% filter(.data[[f]] %in% sel)
        }

        # additional treatment filters
        if (!is.null(input$trtFilterCols)) {
          for (col in input$trtFilterCols) {
            df <- filter(df, .data[[col]] %in% input[[paste0("trtSelect-", col)]])
          }
        }

        if (spec$family == "cdf") {
          req(input$dataCleanSelect, input$cumFracRange, input$maxCumFrac)
          if (input$dataCleanSelect == "clean") {
            df <- distinct(df, TrtID, CumFraction, .keep_all = TRUE)
          }
          df <- filter(df, between(
            CumFraction * 100, input$cumFracRange[1], input$cumFracRange[2]
          ))
        } else {
          df <- addFracDiff(df, spec$groups)
        }
        df
      })

      ## speedData: germination-rate table (rate models only)
      speedData <- reactive({
        req(ready(), spec$family == "rate", input$germSpeedBasis)
        req(nrow(workingData()) > 0)
        interpolateGermSpeed(workingData(), spec$groups, input$germSpeedBasis) %>%
          mutate(GR = round(1 / Time, 6))
      })

      ## data passed to the fit
      fitData <- reactive({
        if (spec$family == "rate") speedData() else workingData()
      })

      ## modelResults: results list, or an error string
      modelResults <- reactive({
        req(ready())
        if (!is.null(spec$transformCol)) req(input$dataTransfSelect)
        req(nrow(fitData()) > 0)

        resolved <- resolveParams(rv$setParams, spec$params)
        maxFrac <- if (spec$family == "cdf") input$maxCumFrac / 100 else 1
        tf <- transformFn()
        pred <- function(d, p) spec$predict(d, p, maxFrac = maxFrac, transform = tf)
        fitModel(pred, fitData(), resolved, spec$response)
      })

      # cache last successful fit; when a later fit fails, the results table and
      # plot keep showing this one and modelErrorUI notes that the displayed
      # coefficients are the last valid ones.
      observe({
        if (is.list(modelResults())) rv$lastGoodModel <- modelResults()
      })

      # Observers ----

      ## user-specified coefficient inputs
      lapply(params, function(p) {
        observeEvent(input[[paste0(p, "-set")]], {
          val <- input[[paste0(p, "-set")]]
          rv$setParams[[p]] <- if (truthy(val)) val else NA
          rv$heldParams[[p]] <- truthy(val)
        })
      })

      ## hold-coefficient checkboxes (set from the results table)
      lapply(params, function(p) {
        observeEvent(input[[paste0(p, "-hold")]], {
          held <- input[[paste0(p, "-hold")]]
          rv$heldParams[[p]] <- held
          updateNumericInput(
            inputId = paste0(p, "-set"),
            value = if (isTRUE(held)) rv$lastGoodModel[[p]] else ""
          )
        })
      })

      # Outputs ----

      ## main content
      output$content <- renderUI({
        reqCols <- colValidation$Column[colValidation[[spec$modelCol]]]
        validate(need(
          ready(),
          paste(
            "Please load a dataset with the required columns for", spec$label,
            "analysis. Minimum required columns are:", paste(reqCols, collapse = ", ")
          )
        ))

        otherTrtCols <- setdiff(names(data()), c(spec$factors, "CumTime", "CumFraction"))

        # data input options: factor checkbox groups + clean/transform controls
        dataOpts <- tagList(
          lapply(spec$factors, function(f) {
            choices <- getColChoices(data(), f)
            checkboxGroupInput(
              ns(paste0(f, "Select")),
              label = spec$factorLabels[[f]],
              choices = choices,
              selected = choices
            )
          })
        )
        if (spec$family == "cdf") {
          dataOpts <- tagList(dataOpts, dataCleanUI(ns))
          if (!is.null(spec$transformCol)) dataOpts <- tagList(dataOpts, dataTransfUI(ns))
        }

        rightCol <- if (spec$family == "cdf") germSlidersUI(ns) else germSpeedSliderUI(ns)

        fluidRow(
          primaryBox(
            title = "Data selection",
            fluidRow(
              column(6, namedWell(title = "Data input options", dataOpts)),
              column(6, rightCol),
              column(12, trtSelectUI(ns, otherTrtCols, reactive(data()))),
              column(12, div(
                class = "p-3 bg-light border rounded data-summary",
                textOutput(ns("dataSummary"))
              ))
            )
          ),
          primaryBox(
            title = "Model parameters",
            fluidRow(
              column(6, uiOutput(ns("modelResults"))),
              column(6, setParamsUI(ns, params)),
              column(12, uiOutput(ns("modelError")))
            )
          ),
          primaryBox(
            title = "Plot output",
            plotOutput(ns("plot"), height = "auto")
          )
        )
      })

      ## data summary text
      output$dataSummary <- renderText({
        n1 <- nrow(workingData())
        n2 <- nrow(data())
        sprintf("Using %s / %s data points (%s%%)", n1, n2, round(n1 / n2 * 100, 0))
      })

      ## results table + error message
      output$modelResults <- modelResultsUI(
        ns, reactive(rv$lastGoodModel), reactive(rv$heldParams), params
      )
      output$modelError <- modelErrorUI(reactive(modelResults()))

      ## plot
      output$plot <- renderPlot(
        {
          req(ready())
          validate(need(nrow(workingData()) > 0, "No data selected."))
          if (spec$family == "cdf") {
            buildCdfPlot(spec, workingData(), rv$lastGoodModel, input$maxCumFrac / 100, transformFn())
          } else {
            buildRatePlot(spec, speedData(), rv$lastGoodModel)
          }
        },
        height = 1000,
        width = 1500,
        res = 150
      )
    }
  )
}
