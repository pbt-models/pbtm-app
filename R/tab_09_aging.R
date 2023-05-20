# ---- Aging ---- #

# UI ----

AgingUI <- function() {
  ns <- NS("aging")
  
  tagList(
    h3(class = "tab-title", "Aging analysis"),
    div(class = "tab-info", "The aging model assumes a data set with aging (natural, controlled deterioration or accelerated aging) as a treatment condition. If you have additional treatments in your dataset, please filter those out as you may get unreliable or unexpected model results. Note: the model may fail to converge under conditions, try adjusting data or model constraints."),
    uiOutput(ns("content"))
  )
}


# Server ----

#' @references colValidation
#' 
#' @param `data` reactive data frame containing the loaded clean data
#' @param `ready` reactive boolean indicating if the model is ready

AgingServer <- function(data, ready) {
  moduleServer(
    id = "aging",
    function(input, output, session) {
      ns <- session$ns
      
      
      # Vars ----
      
      ## paramRangeDefaults ----
      # model constraints: (lower, start, upper)
      paramRangeDefaults <- list(
        ThetaA = c(1, 100, 1000),
        Pmax50 = c(1, 10, 1000),
        Sigma = c(.1, 3, 10)
      )
      
      ## params ----
      params <- names(paramRangeDefaults)
      
      ## rv $ setParams ----
      ## rv $ paramRanges ----
      rv <- reactiveValues(
        setParams = deframe(tibble(params, NA)),
        paramRanges = paramRangeDefaults
      )
      
      
      # Reactives ----
      
      ## workingDataset ----
      # modified/filtered dataset for model and plot
      workingData <- reactive({
        req(
          ready(),
          input$dataCleanSelect,
          input$cumFracRange,
          input$maxCumFrac
        )
        
        df <- data() %>%
          filter(AgingTime %in% input$agingTimeSelect) %>%
          filter(TrtID %in% input$trtIdSelect)
        
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
        if (nrow(workingData()) == 0) return("No data")
        
        # check if a parameter is defined, or set its range constraints if not
        buildModelParams(rv$setParams, rv$paramRanges)
        
        # collect data
        df <- workingData()
        AgingTime <- df$AgingTime
        CumTime <- df$CumTime
        CumFraction <- df$CumFraction
        maxCumFrac <- input$maxCumFrac / 100
        
        # run model
        suppressWarnings(
          tryCatch({
            model <- nls(
              formula = CumFraction ~ maxCumFrac * pnorm(
                q = -(AgingTime + ThetaA / CumTime),
                mean = -Pmax50,
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
      
      
      # Event Reactives ----
      
      ## Model coefficient input observers ----
      lapply(params, function(p) {
        id <- paste0(p, "_set")
        observeEvent(input[[id]], {
          val <- input[[id]]
          rv$setParams[[p]] <- ifelse(val != "", val, NA)
        })
      })
      
      
      
      # Outputs ----
      
      ## content // Main UI ----
      output$content <- renderUI({
        req_cols <- colValidation$Column[colValidation$Aging]
        validate(need(ready(), paste("Please load required data for the aging model analysis. Minimum required columns are:", paste(req_cols, collapse = ", "))))
        
        agingTimeChoices <- getColChoices(data(), "AgingTime")
        
        fluidRow(
          
          # Data selection
          primaryBox(
            title = "Data selection",
            fluidRow(
              column(6,
                namedWell(
                  title = "Data input options",
                  checkboxGroupInput(
                    inputId = ns("agingTimeSelect"),
                    label = "Included aging times:",
                    choices = agingTimeChoices,
                    selected = agingTimeChoices
                  ),
                  dataCleanUI(ns)
                )
              ),
              column(6, germSlidersUI(ns)),
              column(12, trtSelectUI(ns, reactive(data()))),
              column(12, wellPanel(class = "data-summary", textOutput(ns("dataSummary"))))
            )
          ),
          
          # Model parameters
          primaryBox(
            title = "Model parameters",
            fluidRow(
              column(6, setParamsUI(ns, params)),
              column(6, modelResultsUI(ns, reactive(modelResults())))
            )
          ),
          
          # Plot
          primaryBox(
            title = "Plot output",
            plotOutput(ns("plot"))
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
            color = as.factor(AgingTime))) +
          addFracToPlot(maxFrac) +
          geom_point(shape = 19, size = 2) +
          scale_y_continuous(
            labels = scales::percent,
            expand = expansion(),
            limits = c(0, 1.02)) +
          scale_x_continuous(expand = expansion()) +
          labs(
            title = "Cumulative germination",
            caption = "Generated with the PBTM shiny app",
            x = "Time",
            y = "Cumulative fraction germinated (%)",
            color = "Aging Time") +
          guides(color = guide_legend(reverse = F, order = 1)) +
          theme_classic() +
          theme(plot.title = element_text(face = "bold", size = 14))
        
        # plot model results if successful
        if (is.list(model)) {
          thetaA <- model$ThetaA
          pmax50 <- model$Pmax50
          sigma <- model$Sigma
          corr <- model$Correlation
          
          plt <- plt +
            labs(title = "Cumulative germination and aging model fit") +
            mapply(
              function(aging) {
                stat_function(
                  fun = function(x) {
                    maxFrac * pnorm(
                      q = -(aging + thetaA / x),
                      mean = -pmax50,
                      sd = sigma
                    )
                  },
                  aes(color = as.factor(aging))
                )
              },
              unique(df$AgingTime)
            )
          
          # add model annotation
          plt <- addParamsToPlot(plt, list(
            paste("~~", expression(theta~Age), "==", round(thetaA, 2)),
            sprintf("~~Pmax[50]==%.3f", pmax50),
            sprintf("~~sigma==%.3f", sigma),
            sprintf("~~R^2==%.2f", corr)
          ))
        }
        
        plt
      })
      
    } # end
  )
}