# ---- Hydrotime ---- #

# Static UI ----

HydrotimeUI <- function() {
  ns <- NS("hydrotime")
  
  tagList(
    h3(class = "tab-title", "Hydrotime analysis"),
    div(class = "tab-info", "The hydrotime model assumes a data set with germination temperature as a treatment condition. If you have additional treatments in your dataset, the model will average across those treatments and you may get unreliable or unexpected model results. Note: the model may fail to converge under conditions, try adjusting data or model constraints."),
    uiOutput(ns("content"))
  )
}


# Server ----

#' @references colValidation
#' 
#' @param `data` a `reactive()` data frame containing the loaded clean data
#' @param `ready` a `reactive()` boolean indicating if the model is ready

HydrotimeServer <- function(data, ready) {
  moduleServer(
    id = "hydrotime",
    function(input, output, session) {
      ns <- session$ns
      
      
      # Vars ----
      
      ## paramRangeDefaults ----
      # model constraints: (lower, start, upper)
      paramRangeDefaults <- list(
        ThetaH = c(1, 60, 1000),
        PsiB50 = c(-5, -.8, -1e-9),
        Sigma = c(1e-4, .2, 2)
      )
      
      ## params ----
      params <- names(paramRangeDefaults)
      
      ## rv ----
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
        
        rv$lastGoodModel <- NULL
        
        # filter by main vars
        df <- data() %>%
          filter(GermWP %in% input$germWPSelect)
        
        # filter by additional columns
        if (!is.null(input$trtFilterCols)) {
          for (col in input$trtFilterCols) {
            df <- filter(df, .data[[col]] %in% input[[paste0("trtSelect-", col)]])
          }
        }
        
        # optionally remove repeated measurements at same cumulative fraction
        if (input$dataCleanSelect == "clean") {
          df <- df %>% distinct(TrtID, CumFraction, .keep_all = TRUE)
        }
        
        # filter based on cumulative fraction cutoffs
        df %>% filter(between(CumFraction * 100, input$cumFracRange[1], input$cumFracRange[2]))
      })
      
      
      ## modelResults ----
      # list with model results or string with error
      modelResults <- reactive({
        req(ready())
        req(nrow(workingData()) > 0)

        # check if a parameter is defined, or set its range constraints if not
        buildModelParams(rv$setParams, rv$paramRanges)
        
        # collect data
        df <- workingData()
        GermWP <- df$GermWP
        CumTime <- df$CumTime
        CumFraction <- df$CumFraction
        maxCumFrac <- input$maxCumFrac / 100

        # run model, return message if fails
        suppressWarnings(
          tryCatch({
            model <- nls(
              formula = CumFraction ~ maxCumFrac * pnorm(
                q = GermWP - (ThetaH / CumTime),
                mean = PsiB50,
                sd = Sigma
              ),
              start = start, lower = lower, upper = upper,
              algorithm = "port",
              control = list(warnOnly = TRUE) # prevents false convergence error
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
          rv$heldParams[[p]] <- truthy(val)
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

      ## content // Main UI----
      output$content <- renderUI({
        req_cols <- colValidation$Column[colValidation$Hydrotime]
        validate(need(ready(), paste("Please load a dataset with required columns for hydrotime analysis. Minimum required columns are:", paste(req_cols, collapse = ", "))))
        
        germWPChoices <- getColChoices(data(), "GermWP")
        otherTrtCols <- setdiff(names(data()), c("GermWP", "CumTime", "CumFraction"))
        
        fluidRow(
          
          # Data selection
          primaryBox(
            title = "Data selection",
            fluidRow(
              column(6,
                namedWell(
                  title = "Data input options",
                  checkboxGroupInput(
                    inputId = ns("germWPSelect"),
                    label = "Included water potential levels:",
                    choices = germWPChoices,
                    selected = germWPChoices
                  ),
                  dataCleanUI(ns)
                )
              ),
              column(6, germSlidersUI(ns)),
              column(12, trtSelectUI(ns, otherTrtCols, reactive(data()))),
              column(12, wellPanel(class = "data-summary", textOutput(ns("dataSummary"))))
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
            plotOutput(ns("plot"), height = "auto")
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
      output$modelResults <- modelResultsUI(ns, reactive(rv$lastGoodModel), reactive(rv$heldParams))
      
      ## modelError ----
      output$modelError <- modelErrorUI(reactive(modelResults()))

      
      ## plot ----
      output$plot <- renderPlot({
        req(ready())
        validate(need(nrow(workingData()) > 0, "No data selected."))
        
        df <- workingData()
        model <- modelResults()
        maxFrac <- input$maxCumFrac / 100
        
        # generate the plot
        ymax <- 1
        plt <- df %>%
          ggplot(aes(
            x = CumTime,
            y = CumFraction,
            color = as.factor(GermWP))) +
          addFracToPlot(maxFrac) +
          geom_point(
            shape = 19,
            size = 2) +
          scale_y_continuous(
            labels = scales::percent,
            expand = expansion(c(0, .05))) +
          scale_x_continuous(
            breaks = scales::breaks_pretty(6),
            expand = expansion(c(0, .05))) +
          expand_limits(x = 0, y = 0) +
          coord_cartesian(ylim = c(0, ymax)) +
          labs(
            title = "Cumulative germination",
            caption = "Generated with the PBTM app",
            x = "Time",
            y = "Cumulative fraction germinated (%)",
            color = "Water potential") +
          guides(color = guide_legend(reverse = T, order = 1)) +
          theme_classic() +
          theme(plot.title = element_text(face = "bold", size = 14))
        
        # add model results if successful
        if (is.list(model)) {
          thetaH <- model$ThetaH
          psiB50 <- model$PsiB50
          sigma <- model$Sigma
          corr <- model$Correlation
          
          plt <- plt +
            labs(title = "Cumulative germination and hydrotime model fit") +
            mapply(
              function(wp) {
                stat_function(
                  fun = function(x) {
                    maxFrac * pnorm(
                      q = wp - (thetaH / x),
                      mean = psiB50,
                      sd = sigma
                    )
                  },
                  aes(color = as.factor(wp))
                )
              },
              unique(df$GermWP)
            )
          
          # add model annotation
          plt <- addParamsToPlot(plt, list(
            sprintf("~~theta~H==%.2f", thetaH),
            sprintf("~~Psi[b][50]==%.2f", psiB50),
            sprintf("~~sigma==%.3f", sigma),
            sprintf("~~R^2==%.2f", corr)
          ), ymax)
        }
        
        plt
      },
        height = 1000,
        width = 1500,
        res = 150
      )
      
    } # end
  )
}
