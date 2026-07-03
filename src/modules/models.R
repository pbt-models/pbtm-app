# ---- Model module factory ---- #
# Generates the UI and server for any model from its spec (see model-specs.R).
# Replaces the eight near-identical tab_04..tab_11 files. Behaviour is shared;
# only the spec differs.

# Helpers ----

#' @description generates a named list of unique values in a column and adds the count
#' @param df the data frame
#' @param col string specifying column name
#' @returns named list
getColChoices <- function(df, col) {
  df %>%
    count(.data[[col]]) |>
    select(n, everything()) |>
    mutate(n = sprintf("%s (n = %s)", .data[[col]], n)) |>
    deframe()
}

# UI blocks ----

#' @description a shared ui component
#' @param ns namespace function from calling server
dataCleanUI <- function(ns) {
  radioButtons(
    inputId = ns("dataCleanSelect"),
    label = "Select data cleaning:",
    choices = list(
      "Original" = "original",
      "Cleaned (remove duplicates)" = "clean"
    )
  )
}

#' @description a shared ui component
#' @param ns namespace function from calling server
dataTransfUI <- function(ns) {
  radioButtons(
    inputId = ns("dataTransfSelect"),
    label = "Select dosage transformation:",
    choices = list(
      "None" = "none",
      "Logarithmic" = "log"
    )
  )
}

#' @description a shared ui component
#' @param ns namespace function from calling server
germSlidersUI <- function(ns) {
  namedWell(
    title = "Germination constraints",
    sliderInput(
      inputId = ns("maxCumFrac"),
      label = "Maximum germination (%) observed",
      min = 10,
      max = 100,
      value = 100,
      step = 1
    ),
    sliderInput(
      inputId = ns("cumFracRange"),
      label = "Included interval (%):",
      min = 0,
      max = 100,
      value = c(0, 100)
    )
  )
}

#' @description a shared ui component
#' @param ns namespace function from calling server
germSpeedSliderUI <- function(ns) {
  namedWell(
    title = "Germination speed options",
    sliderInput(
      inputId = ns("germSpeedBasis"),
      label = "Germination (%) for speed calculation:",
      min = 10,
      max = 100,
      value = 50,
      step = 5
    )
  )
}

#' @description ui element showing model error if present
#' @param results object from modelResults either list or string error
#' @returns a rendered UI element or nothing
modelErrorUI <- function(results) {
  renderUI({
    if (is.list(results())) {
      return()
    }
    div(
      class = "model-error",
      sprintf(
        "Model failed under current settings: %s. Last valid model coefficients shown above.",
        results()
      )
    )
  })
}

#' @description results well for a single-population fit, with per-parameter
#'   hold checkboxes. Returns UI (call inside a renderUI); only parameter rows
#'   and the pseudo-R^2 are shown (mixture/stats fields like AIC are filtered).
#' @param ns namespace function
#' @param res a results list (params + PseudoR2, possibly extra stats)
#' @param held named list of held-param flags
#' @param paramNames fittable parameter names (these rows get a hold checkbox)
singleResultsWell <- function(ns, res, held, paramNames) {
  labelFor <- function(p) if (p == "PseudoR2") "Pseudo-R²" else p
  namedWell(
    title = "Model results",
    renderTable(
      {
        res %>%
          enframe() %>%
          filter(name %in% c(paramNames, "PseudoR2")) %>%
          unnest(value) %>%
          mutate(
            Hold = lapply(name, function(p) {
              if (p %in% paramNames) {
                HTML(sprintf(
                  "<input type='checkbox' %s onclick=\"Shiny.setInputValue('%s-hold', this.checked, {priority: 'event'})\">",
                  ifelse(isTRUE(held[[p]]), "checked", ""),
                  ns(p)
                ))
              } else {
                ""
              }
            }),
            name = vapply(name, labelFor, character(1))
          ) %>%
          rename(Parameter = name, Value = value)
      },
      digits = 4,
      width = "100%",
      striped = FALSE,
      hover = TRUE,
      sanitize.text.function = function(x) x,
      align = "llc"
    )
  )
}

#' @description results well for a k-component subpopulation mixture: a table of
#'   per-subpopulation coefficients + mixing weight, the fit stats, and (when
#'   provided) the AIC model-comparison table from auto-detect.
#' @param spec the model spec
#' @param res a mixture results list (componentParam names like Tb1, Tb2, ... + w*)
#' @param k number of subpopulations
#' @param aicTable optional comparison tibble (k, npar, PseudoR2, AIC, dAIC)
mixtureResultsWell <- function(spec, res, k, aicTable = NULL) {
  w <- mixtureWeights(res, k)
  comp <- purrr::map_dfr(seq_len(k), function(j) {
    vals <- lapply(spec$paramNames, function(nm) round(res[[paste0(nm, j)]], 4))
    tibble(Subpopulation = j) %>%
      bind_cols(as_tibble(setNames(vals, spec$paramNames))) %>%
      mutate(Weight = round(w[j], 3))
  })
  tagList(
    namedWell(
      title = sprintf("Subpopulation coefficients (k = %d)", k),
      renderTable(comp, digits = 4, width = "100%", hover = TRUE),
      div(
        class = "mt-2 text-muted",
        sprintf("Pseudo-R² = %.3f    AIC = %.1f", res$PseudoR2, res$AIC)
      )
    ),
    if (!is.null(aicTable)) {
      namedWell(
        title = "Subpopulation count comparison (lower AIC is better)",
        renderTable(
          aicTable %>%
            transmute(
              `Subpops (k)` = k,
              Parameters = npar,
              `Pseudo-R²` = round(PseudoR2, 3),
              AIC = round(AIC, 1),
              `ΔAIC` = round(dAIC, 1)
            ),
          digits = 2,
          width = "100%",
          hover = TRUE
        )
      )
    }
  )
}

#' @description a shared ui component
#' @param ns namespace function from calling server
#' @param params vector of param names
setParamsUI <- function(ns, params) {
  namedWell(
    title = "Specify model coefficients (optional)",
    div(
      class = "flex-row",
      lapply(params, function(p) {
        numericInput(
          inputId = ns(paste0(p, "-set")),
          label = p,
          value = NA,
          step = .1,
          width = "30%"
        )
      })
    ),
    em(
      "Specify individual model coefficients, or leave blank to allow the model to find a best-fit value."
    )
  )
}


# Static UI ----

#' @description build the static UI for a model tab from its spec
#' @param spec a model spec
modelUI <- function(spec) {
  ns <- NS(spec$id)
  tagList(
    h3(class = "tab-title", paste(spec$label, "analysis")),
    div(
      class = "tab-info",
      spec$tabInfo,
      build_modal_link(
        spec$doc,
        paste("About the", tolower(spec$label), "model")
      )
    ),
    uiOutput(ns("content"), style = "margin-top: 1rem;")
  )
}


# Server ----

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
        lastGoodModel = NULL,
        subpopTable = NULL
      )

      # dosage transform (promoter/inhibitor only); identity otherwise
      transformFn <- reactive({
        if (
          !is.null(spec$transformCol) &&
            identical(input$dataTransfSelect, "log")
        ) {
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
            df <- filter(
              df,
              .data[[col]] %in% input[[paste0("trtSelect-", col)]]
            )
          }
        }

        if (spec$family == "cdf") {
          req(input$dataCleanSelect, input$cumFracRange, input$maxCumFrac)
          if (input$dataCleanSelect == "clean") {
            df <- distinct(df, TrtID, CumFraction, .keep_all = TRUE)
          }
          df <- filter(
            df,
            between(
              CumFraction * 100,
              input$cumFracRange[1],
              input$cumFracRange[2]
            )
          )
        } else {
          df <- addFracDiff(df, spec$groups)
        }
        df
      })

      ## speedData: germination-rate table (rate models only)
      speedData <- reactive({
        req(ready(), spec$family == "rate", input$germSpeedBasis)
        req(nrow(workingData()) > 0)
        interpolateGermSpeed(
          workingData(),
          spec$groups,
          input$germSpeedBasis
        ) %>%
          mutate(GR = round(1 / Time, 6))
      })

      ## data passed to the fit
      fitData <- reactive({
        if (spec$family == "rate") speedData() else workingData()
      })

      ## number of subpopulations to fit (CDF specs only): "1" | "2" | "3" | "auto"
      nSub <- reactive({
        if (!isTRUE(spec$subpop)) {
          return("1")
        }
        input$nSubpop %||% "1"
      })

      ## fit: list(res = results-or-error, table = AIC comparison or NULL)
      ## k = 1 keeps the single-population fit (honours user-pinned params);
      ## k > 1 / auto run the subpopulation mixture.
      fitObj <- reactive({
        req(ready())
        if (!is.null(spec$transformCol)) {
          req(input$dataTransfSelect)
        }
        req(nrow(fitData()) > 0)

        maxFrac <- if (spec$family == "cdf") input$maxCumFrac / 100 else 1
        tf <- transformFn()
        mode <- nSub()

        if (identical(mode, "1")) {
          resolved <- resolveParams(rv$setParams, spec$params)
          pred <- function(d, p) {
            spec$predict(d, p, maxFrac = maxFrac, transform = tf)
          }
          res <- fitModel(pred, fitData(), resolved, spec$response)
          if (is.list(res)) {
            res$k <- 1L
          }
          list(res = res, table = NULL)
        } else if (identical(mode, "auto")) {
          det <- detectSubpops(
            spec,
            fitData(),
            maxK = 3,
            maxFrac = maxFrac,
            transform = tf
          )
          list(res = det$best, table = det$table)
        } else {
          kk <- as.integer(mode)
          base <- fitMixture(spec, fitData(), 1, NULL, maxFrac, tf)
          res <- fitMixture(
            spec,
            fitData(),
            kk,
            if (is.list(base)) base else NULL,
            maxFrac,
            tf
          )
          list(res = res, table = NULL)
        }
      })

      modelResults <- reactive(fitObj()$res)

      # cache last successful fit; when a later fit fails, the results table and
      # plot keep showing this one and modelErrorUI notes that the displayed
      # coefficients are the last valid ones.
      observe({
        if (is.list(fitObj()$res)) rv$lastGoodModel <- fitObj()$res
      })
      observe({
        rv$subpopTable <- fitObj()$table
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
            "Please load a dataset with the required columns for",
            spec$label,
            "analysis. Minimum required columns are:",
            paste(reqCols, collapse = ", ")
          )
        ))

        otherTrtCols <- setdiff(
          names(data()),
          c(spec$factors, "CumTime", "CumFraction")
        )

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
          if (!is.null(spec$transformCol)) {
            dataOpts <- tagList(dataOpts, dataTransfUI(ns))
          }
        }

        rightCol <- if (spec$family == "cdf") {
          germSlidersUI(ns)
        } else {
          germSpeedSliderUI(ns)
        }

        fluidRow(
          primaryBox(
            title = "Data selection",
            fluidRow(
              column(6, namedWell(title = "Data input options", dataOpts)),
              column(6, rightCol),
              column(12, trtSelectUI(ns, otherTrtCols, reactive(data()))),
              column(
                12,
                div(
                  class = "p-3 bg-light border rounded data-summary",
                  textOutput(ns("dataSummary"))
                )
              )
            )
          ),
          primaryBox(
            title = "Model parameters",
            fluidRow(
              if (isTRUE(spec$subpop)) {
                column(
                  12,
                  namedWell(
                    title = "Subpopulations",
                    radioButtons(
                      ns("nSubpop"),
                      label = "Fit a mixture of distinct seed subpopulations:",
                      choices = c(
                        "1 (single)" = "1",
                        "2" = "2",
                        "3" = "3",
                        "Auto-detect" = "auto"
                      ),
                      selected = "1",
                      inline = TRUE
                    ),
                    em(
                      "Auto-detect fits 1–3 subpopulations and selects the best by AIC. With >1 subpopulation, the optional coefficients below are not used."
                    )
                  )
                )
              },
              column(6, uiOutput(ns("modelResults"))),
              column(6, setParamsUI(ns, params)),
              column(12, uiOutput(ns("modelError")))
            )
          ),
          primaryBox(
            title = "Plot output",
            div(
              class = "plot-mode-toggle",
              radioButtons(
                ns("plotMode"),
                label = NULL,
                choices = c("Static" = "static", "Interactive" = "interactive"),
                selected = "static",
                inline = TRUE
              )
            ),
            uiOutput(ns("plotArea"))
          )
        )
      })

      ## data summary text
      output$dataSummary <- renderText({
        n1 <- nrow(workingData())
        n2 <- nrow(data())
        sprintf(
          "Using %s / %s data points (%s%%)",
          n1,
          n2,
          round(n1 / n2 * 100, 0)
        )
      })

      ## results table (single fit) or subpopulation table (mixture)
      output$modelResults <- renderUI({
        res <- rv$lastGoodModel
        validate(need(is.list(res), "No model results yet."))
        k <- res$k %||% 1
        if (k > 1) {
          mixtureResultsWell(spec, res, k, rv$subpopTable)
        } else {
          singleResultsWell(ns, res, rv$heldParams, params)
        }
      })
      output$modelError <- modelErrorUI(reactive(modelResults()))

      ## plot (shared builder; interactive flag drops static-only plotmath)
      buildPlot <- function(interactive) {
        if (spec$family == "cdf") {
          buildCdfPlot(
            spec,
            workingData(),
            rv$lastGoodModel,
            input$maxCumFrac / 100,
            transformFn(),
            interactive = interactive
          )
        } else {
          buildRatePlot(
            spec,
            speedData(),
            rv$lastGoodModel,
            interactive = interactive
          )
        }
      }

      # swap the output container based on the Static/Interactive toggle
      output$plotArea <- renderUI({
        if (identical(input$plotMode, "interactive")) {
          plotlyOutput(ns("plotly"), height = "600px")
        } else {
          plotOutput(ns("plot"), height = "auto")
        }
      })

      output$plot <- renderPlot(
        {
          req(ready())
          validate(need(nrow(workingData()) > 0, "No data selected."))
          buildPlot(FALSE)
        },
        height = 1000,
        width = 1500,
        res = 150
      )

      output$plotly <- renderPlotly({
        req(ready())
        validate(need(nrow(workingData()) > 0, "No data selected."))
        ggplotly(buildPlot(TRUE))
      })
    }
  )
}
