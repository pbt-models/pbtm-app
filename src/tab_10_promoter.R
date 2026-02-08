# ---- Promoter ---- #

# UI ----

PromoterUI <- function() {
  ns <- NS("promoter")
  
  tagList(
    h3(class = "tab-title", "Promoter analysis"),
    div(class = "tab-info", "The promoter model assumes a data set with a promoter at different dosages (increasing values will speed up germination) as a treatment condition. If you have additional treatments in your dataset, please filter those out as you may get unreliable or unexpected model results. Note: the model may fail to converge under conditions, try adjusting data or model constraints."),
    uiOutput(ns("content"))
  )
}


# Server ----

#' @references colValidation
#' 
#' @param `data` a `reactive()` data frame containing the loaded clean data
#' @param `ready` a `reactive()` boolean indicating if the model is ready

PromoterServer <- function(data, ready) {
  moduleServer(
    id = "promoter",
    function(input, output, session) {
      ns <- session$ns
      
      # Vars ----
      
      ## paramRangeDefaults ----
      # model constraints: (lower, start, upper)
      paramRangeDefaults <- list(
        ThetaP = c(1, 200, 1000),
        Pb50 = c(0.05, 5, 1000),
        Sigma = c(.001, 3, 10)
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
      
      ## workingDataset ----
      # modified/filtered dataset for model and plot
      workingData <- reactive({
        req(
          ready(),
          input$dataCleanSelect,
          input$dataTransfSelect,
          input$cumFracRange,
          input$maxCumFrac
        )
        
        rv$lastGoodModel <- NULL
        
        # filter by main vars
        df <- data() %>%
          filter(GermPromoterDosage %in% input$germPromoterDosageSelect)
        
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
      # list of coefficients or string error message
      modelResults <- reactive({
        req(ready())
        req(nrow(workingData()) > 0)
        req(input$dataTransfSelect)
        
        # check if a parameter is defined, or set its range constraints if not
        buildModelParams(rv$setParams, rv$paramRanges)
        
        # collect data
        df <- workingData()
        GermPromoterDosage <- df$GermPromoterDosage
        CumTime <- df$CumTime
        CumFraction <- df$CumFraction
        maxCumFrac <- input$maxCumFrac / 100
        
        # check for logarithmic transformation and replace zero / control values
        if (input$dataTransfSelect == "log") {
          dtTransf <- "log10"
        } else {
          dtTransf <- "as.numeric"
        }
        
        # run model
        suppressWarnings(
          tryCatch({
            model <- nls(
              formula = CumFraction ~ maxCumFrac * pnorm(
                q = (get(dtTransf)(GermPromoterDosage) - ThetaP / CumTime),
                mean = Pb50,
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
      
      
      # Observers ----
      
      
      # Outputs ----
      
      ## content // Main UI ----
      output$content <- renderUI({
        req_cols <- colValidation$Column[colValidation$Promoter]
        validate(need(ready(), paste("Please load required data for the promoter model analysis. Minimum required columns are:", paste(req_cols, collapse = ", "))))
        
        germPromoterDosageChoices <- getColChoices(data(), "GermPromoterDosage")
        otherTrtCols <- setdiff(names(data()), c("GermPromoterDosage", "CumTime", "CumFraction"))
        
        fluidRow(
          
          # Data selection
          primaryBox(
            title = "Data selection",
            fluidRow(
              column(6,
                namedWell(
                  title = "Data input options",
                  checkboxGroupInput(
                    inputId = ns("germPromoterDosageSelect"),
                    label = "Included promoter dosages:",
                    choices = germPromoterDosageChoices,
                    selected = germPromoterDosageChoices
                  ),
                  dataCleanUI(ns),
                  dataTransfUI(ns)
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
            color = as.factor(GermPromoterDosage))) +
          addFracToPlot(maxFrac) +
          geom_point(shape = 19, size = 2) +
          scale_y_continuous(
            labels = scales::percent,
            expand = expansion()) +
          scale_x_continuous(
            breaks = scales::breaks_pretty(6),
            expand = expansion(c(0, .05))) +
          coord_cartesian(ylim = c(0, ymax)) +
          labs(
            title = "Cumulative germination",
            caption = "Generated with the PBTM app",
            x = "Time",
            y = "Cumulative fraction germinated (%)",
            color = "Promoter Dosage") +
          guides(color = guide_legend(reverse = F, order = 1)) +
          theme_classic() +
          theme(plot.title = element_text(face = "bold", size = 14))
        
        
        # check for logarithmic transformation and replace zero / control values
        if (input$dataTransfSelect == "log") {
          dtTransf <- "log10"
        } else {
          dtTransf <- "as.numeric"
        }
        
        # plot model results if successful
        if (is.list(model)) {
          thetaP <- model$ThetaP
          pb50 <- model$Pb50
          sigma <- model$Sigma
          corr <- model$Correlation
          
          plt <- plt +
            labs(title = "Cumulative germination and promoter model fit") +
            mapply(
              function(promoterdosage) {
                stat_function(
                  fun = function(x) {
                    maxFrac * pnorm(
                      q = (get(dtTransf)(promoterdosage) - thetaP / x),
                      mean = pb50,
                      sd = sigma
                    )
                  },
                  aes(color = as.factor(promoterdosage))
                )
              },
              unique(df$GermPromoterDosage)
            )
          
          
          # add model annotation
          plt <- addParamsToPlot(plt, list(
            sprintf("~~theta[P]==%.2f", thetaP),
            sprintf("~~P[b](50)==%.2f", 10^pb50),
            sprintf("~~sigma==%.3f", sigma),
            sprintf("~~R^2==%.2f", corr)
          ), ymax)
        }
        
        plt
      })
      
      
    } # end
  )
}
