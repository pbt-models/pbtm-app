# ---- Thermal Time ---- #

# Static UI ----

ThermalTimeUI <- function() {
  ns <- NS("thermalTime")
  
  tagList(
    h3(class = "tab-title", "Thermal time analysis"),
    div(class = "tab-info", "The thermal time model assumes a data set with germination temperature as a treatment condition. If you have additional treatments in your dataset, the model will display those treatments and include them in the model calculation which can result on unreliable or unexpected results. Please inform and remove additonal treatments when pertinent in the lower left corner of this section. Note: the model may fail to converge under conditions, try adjusting data or model constraints."),
    uiOutput(ns("content"))
  )
}


# Server ----

#' @references colValidation
#' 
#' @param `data` a `reactive()` data frame containing the loaded clean data
#' @param `ready` a `reactive()` boolean indicating if the model is ready

ThermalTimeServer <- function(data, ready) {
  moduleServer(
    id = "thermalTime",
    function(input, output, session) {
      ns <- session$ns
      
      
      # Vars ----
      
      ## paramRangeDefaults ----
      # model constraints: (lower, start, upper)
      paramRangeDefaults <- list(
        Tb = c(0, 6, 20),
        ThetaT50 = c(0.5, 3, 50),
        Sigma = c(.0001, .09, 1.5)
      )
      
      ## params ----
      params <- names(paramRangeDefaults)
      
      ## rv $ setParams ----
      ## rv $ paramRanges ----
      rv <- reactiveValues(
        setParams = deframe(tibble(params, NA)),
        heldParams = deframe(tibble(params, FALSE)),
        paramRanges = paramRangeDefaults,
        lastGoodModel = NULL
      )
      
      
      # Reactives ----
      
      ## workingData ----
      # modified/filtered dataset for model and plot
      workingData <- reactive({
        req(
          ready(),
          input$dataCleanSelect,
          input$cumFracRange,
          input$maxCumFrac
        )
        
        df <- data() %>%
          filter(GermTemp %in% input$germTempSelect) %>%
          filter(TrtID %in% input$trtIdSelect)
        
        # optionally remove repeated measurements at same cumulative fraction
        if (input$dataCleanSelect == "clean") {
          df <- df %>%
            distinct(TrtID, CumFraction, .keep_all = TRUE)
        }
        
        # filter based on cumulative fraction cutoffs
        df %>% filter(between(CumFraction * 100, input$cumFracRange[1], input$cumFracRange[2]))
      })
      
      
      ## modelResults ----
      # list with model results or string with error
      modelResults <- reactive({
        req(ready())
        if (nrow(workingData()) == 0) return("No data")
        
        # check if a parameter is defined, or set its range constraints if not
        buildModelParams(rv$setParams, rv$paramRanges)
        
        # collect data
        df <- workingData()
        GermTemp <- df$GermTemp
        CumTime <- df$CumTime
        CumFraction <- df$CumFraction
        maxCumFrac <- input$maxCumFrac / 100
        
        # run model, return message if fails
        suppressWarnings(
          tryCatch({
            model <- nls(
              formula = CumFraction ~ maxCumFrac * pnorm(
                q = log10(CumTime),
                mean = ThetaT50 - log10(GermTemp - Tb),
                sd = Sigma
              ),
              start = start, lower = lower, upper = upper,
              algorithm = "port"
            )
            
            # grab coefs from model or user-set values
            buildModelResults(model, params, defined, CumFraction)
          },
            error = function(cond) { paste(cond[1]) }
          )
        )
      })
      
      # Save coefficients on success (in case model later fails)
      observe({
        if (is.list(modelResults())) rv$lastGoodModel <- modelResults()
      })
      
      
      
      # Event Reactives ----
      
      ## Model coefficient input observers ----
      lapply(params, function(p) {
        id <- paste0(p, "-set")
        observeEvent(input[[id]], {
          val <- input[[id]]
          rv$setParams[[p]] <- ifelse(val != "", val, NA)
          if (!truthy(val)) rv$heldParams[[p]] <- FALSE
        })
      })
      
      ## hold param checkbox observers ----
      lapply(params, function(p) {
        id <- paste0(p, "-hold")
        observeEvent(input[[id]], {
          rv$heldParams[[p]] <- input[[id]]
          updateNumericInput(
            inputId = paste0(p, "-set"),
            value = ifelse(input[[id]], modelResults()[[p]], "")
          )
        })
      })
      
      
      # Outputs ----
      
      ## content // Main UI ----
      output$content <- renderUI({
        req_cols <- colValidation$Column[colValidation$ThermalTime]
        validate(need(ready(), paste("Please load a dataset with required columns for thermal time analysis. Minimum required columns are:", paste(req_cols, collapse = ", "))))
        
        germTempChoices <- getColChoices(data(), "GermTemp")
        
        fluidRow(
          
          # Data selection
          primaryBox(
            title = "Data selection",
            fluidRow(
              column(6,
                namedWell(
                  title = "Data input options",
                  checkboxGroupInput(
                    inputId = ns("germTempSelect"),
                    label = "Included temperature levels:",
                    choices = germTempChoices,
                    selected = germTempChoices
                  ),
                  dataCleanUI(ns)
                )
              ),
              column(6, germSlidersUI(ns)),
              column(12, trtSelectUI(ns, reactive(data()))),
              column(12, wellPanel(class = "data-summary", textOutput(ns("dataSummary")))
              )
            )
          ),
          
          # Model parameters
          primaryBox(
            title = "Model parameters",
            fluidRow(
              column(6, uiOutput(ns("modelResults"))),
              column(6, setParamsUI(ns, params)),
              column(12, uiOutput(ns("modelError")))
            )
          ),
          
          # Plot
          primaryBox(
            title = "Plot output",
            plotOutput(ns("plot"), height = "450px")
          )
        )
      })
      
      ## dataSummary ----
      # displays data remaining after filter
      output$dataSummary <- renderText({
        n1 <- nrow(workingData())
        n2 <- nrow(data())
        pct <- round(n1 / n2 * 100, 0)
        sprintf("Using %s / %s data points (%s%%)", n1, n2, pct)
      })
      
      
      ## modelResults ----
      output$modelResults <- renderUI({
        modelResultsUI(ns, reactive(rv$lastGoodModel), reactive(rv$heldParams))
      })
      
      output$modelError <- renderUI({
        if (is.list(modelResults())) return() # no error
        div(
          class = "model-error",
          sprintf("Model failed under current settings: %s. Last valid model coefficients shown above.", modelResults())
        )
      })
      
      
      ## plot ----
      output$plot <- renderPlot({
        req(ready())
        validate(need(nrow(workingData()) > 0, "No data selected."))
        
        df <- workingData()
        model <- modelResults()
        maxFrac <- input$maxCumFrac / 100

        # generate the plot
        plt <- df %>%
          ggplot(aes(
            x = CumTime,
            y = CumFraction,
            color = as.factor(GermTemp))) +
          addFracToPlot(maxFrac) +
          geom_point(shape = 19, size = 2) +
          scale_y_continuous(
            labels = scales::percent,
            expand = expansion(),
            limits = c(0, 1.02)) +
          scale_x_continuous(expand = expansion()) +
          labs(
            title = "Cumulative germination",
            caption = "Generated with the PBTM app",
            x = "Time",
            y = "Cumulative fraction germinated (%)",
            color = "Temperature") +
          guides(color = guide_legend(reverse = T, order = 1)) +
          theme_classic() +
          theme(plot.title = element_text(face = "bold", size = 14))

        # add model results if successful
        if (is.list(model)) {
          tb <- model$Tb
          thetaT50 <- model$ThetaT50
          sigma <- model$Sigma
          corr <- model$Correlation
          
          plt <- plt +
            labs(title = "Cumulative germination and thermal time sub-optimal model fit") +
            mapply(
              function(temp) {
                stat_function(
                  fun = function(x) {
                    maxFrac * pnorm(
                      q = log10(x),
                      mean = thetaT50 - log10(temp - tb),
                      sd = sigma
                    )
                  },
                  aes(color = as.factor(temp))
                )
              },
              unique(df$GermTemp)
            )
          
          # add model annotation
          plt <- addParamsToPlot(plt, list(
            sprintf("~~T[b]==%.1f", tb),
            sprintf("~~theta[T][50]==%.3f", thetaT50),
            sprintf("~~sigma==%.3f", sigma),
            sprintf("~~R^2==%.2f", corr)
          )) 
        }

        plt
      })
      
    } # end
  )
}
