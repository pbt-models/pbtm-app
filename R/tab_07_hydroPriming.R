# ---- Hydropriming ---- #

# UI ----

HydroprimingUI <- function() {
  ns <- NS("hydropriming")
  
  tagList(
    h3(class = "tab-title", "Hydropriming analysis"),
    div(class = "tab-info", "This hydropriming model assumes a germination data set with priming water potential and priming duration as treatment conditions. If you have additional treatments in your dataset, the model will average across those treatments and you may get unreliable or unexpected model results. Note: the model may fail to converge under conditions, try adjusting data or model constraints."),
    uiOutput(ns("content"))
  )
}


# Server ----

#' @references colValidation
#' 
#' @param `data` a `reactive()` data frame containing the loaded clean data
#' @param `ready` a `reactive()` boolean indicating if the model is ready

HydroprimingServer <- function(data, ready) {
  moduleServer(
    id = "hydropriming",
    function(input, output, session) {
      ns <- session$ns
      
      # Vars ----
      
      ## defaultGermSpeeds ----
      defaultGermSpeeds <- c(50)
      
      ## rv $ germSpeedBasis ----
      rv <- reactiveValues(
        germSpeedBasis = defaultGermSpeeds
      )
      
      ## paramRangeDefaults ----
      # model constraints: (lower, start, upper)
      paramRangeDefaults <- list(
        PsiMin = c(-10, -1, -.5),
        GRi = c(.00000001, .001, .1),
        Slope = c(.00000001, .1, 1)
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
      # modified/filtered data for model and plot
      workingData <- reactive({
        req(
          ready(),
        )
        
        #rv$lastGoodModel <- NULL
        
        # filter by main vars
        df <- data() %>%
          group_by(TrtID, PrimingWP, PrimingDuration) %>% 
          filter( PrimingWP %in% input$primingWPSelect,
                  PrimingDuration %in% input$primingDurationSelect)
        
        # filter by additional columns
        if (!is.null(input$trtFilterCols)) {
          for (col in input$trtFilterCols) {
            df <- filter(df, .data[[col]] %in% input[[paste0("trtSelect-", col)]])
          }
        }
        
          df <- df %>% 
          arrange(TrtID, PrimingWP, PrimingDuration, CumTime, CumFraction) %>%
          mutate(FracDiff = CumFraction - lag(CumFraction, default = 0)) %>%
          ungroup()
        
      })
      
      ## germSpeedData ----
      # used by germination speed table
      germSpeedData <- reactive({
        req(
          ready(),
          input$germSpeedBasis,
          input$dataCleanSelect
        )
        req(nrow(workingData()) > 0)
        
        workingData() %>%
          mutate(
            MaxCumFrac = max(CumFraction),
            .by = c(TrtID, PrimingWP, PrimingDuration)) %>%
          arrange(CumTime) %>%
          summarise(
            MaxCumFrac = max(MaxCumFrac),
            FracDiff = sum(FracDiff),
            .by = c(TrtID, PrimingWP, PrimingDuration, CumTime)) %>%
          mutate(
            CumFraction = cumsum(FracDiff) / sum(FracDiff) * MaxCumFrac,
            .by = c(TrtID, PrimingWP, PrimingDuration)) %>%
          group_by(across(c(TrtID, PrimingWP, PrimingDuration))) %>%
          arrange(CumTime) %>%
          reframe(
            {
              approx(CumFraction, CumTime, xout = rv$germSpeedBasis / 100, ties = "ordered", rule = 2) %>%
                setNames(c("Frac", "Time")) %>%
                as_tibble() %>%
                drop_na()
            }
          )
      })
      
      
      
      ## modelResults ----
      # list of model coefficients, or an error message
      modelResults <- reactive({
        req(ready())
        req(nrow(germSpeedData()) > 0)
        
        # check if a parameter is defined, or set its range constraints if not
        buildModelParams(rv$setParams, rv$paramRanges)
        
        # collect data
        df <- germSpeedData()
        PrimingWP <- df$PrimingWP
        PrimingDuration <- df$PrimingDuration
        GR <- round(1 / df$Time, 6)
        
        # try to run the model
        suppressWarnings(
          tryCatch({
            
            #Function to calculate Theta Hydro Priming
            fθHTP <- function(PM50){ (PrimingWP - PM50) * PrimingDuration }
            
            model <- nls(
              formula = GR ~ GRi + fθHTP(PsiMin) * Slope,
              start = start, lower = lower, upper = upper,
              algorithm = "port",
              control = list(warnOnly = TRUE) # prevents false convergence error
            )
            
            # grab coefs from model or user-set values
            buildModelResults(model, params, defined, GR)
          },
          error = function(cond) { paste(cond[1]) }
          )
        )
      })
      
      # Save coefficients on success (in case model later fails)
      observe({
        if (is.list(modelResults())) rv$lastGoodModel <- modelResults()
      })
      
      
      
      # Observers ----

      ## set new germ speeds ----
      observeEvent(input$germSpeedBasis, {
        rv$germSpeedBasis <- input$germSpeedBasis
      })
      
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
            value = ifelse(input[[id]], rv$lastGoodModel[[p]], "")
          )
        })
      })      
      
      
      
      # Outputs ----
      
      ## content // Main UI ----
      output$content <- renderUI({
        req_cols <- colValidation$Column[colValidation$Hydropriming]
        validate(need(ready(), paste("Please load required data for hydropriming analysis. Minimum required columns are:", paste(req_cols, collapse = ", "))))
        primingWPChoices <- getColChoices(data(), "PrimingWP")
        primingDurationChoices <- getColChoices(data(), "PrimingDuration")
        otherTrtCols <- setdiff(names(data()), c("PrimingWP", "PrimingDuration", "CumTime", "CumFraction"))
        
        fluidRow(
          
          # Data selection
          primaryBox(
            title = "Data selection",
            fluidRow(
              column(6,
                namedWell(
                  title = "Data input options",
                  checkboxGroupInput(
                    inputId = ns("primingWPSelect"),
                    label = "Included priming water potential levels:",
                    choices = primingWPChoices,
                    selected = primingWPChoices
                  ),
                  checkboxGroupInput(
                    inputId = ns("primingDurationSelect"),
                    label = "Included priming duration levels:",
                    choices = primingDurationChoices,
                    selected = primingDurationChoices
                  )
                )
              ),
              column(6, germSpeedSliderUI(ns), dataCleanUI(ns)),
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
        
        #df <- workingData()
        df <- germSpeedData()
        model <- modelResults()
        
        PsiMin <- model$PsiMin
        
        # Update Theta Hydropriming values
        df <- df %>%
          as_tibble() %>%
          mutate(Theta = (PrimingWP - PsiMin) * PrimingDuration)
        
        # generate the plot
        ymax <- max(1/df$Time, na.rm = T)
        plt <- df %>%
          ggplot(aes(
            x = Theta,
            y = 1/Time,
            color = as.factor(PrimingWP))) +
          geom_point(size = 4,
            aes(shape = as.factor(PrimingDuration))) +
          scale_y_continuous(
            expand = expansion(c(0, .05))) +
          scale_x_continuous(
            breaks = scales::breaks_pretty(6),
            expand = expansion(c(0, .05))) +
          coord_cartesian(ylim = c(0, ymax)) +
          labs(
            caption = "Generated with the PBTM app",
            x = "Hydropriming time",
            y = "Germination rate",
            color = "Water Potential",
            shape = "Duration") +
          guides(
            color = guide_legend(reverse = T, order = 1),
            linetype = "none") +
          theme_classic() +
          theme(plot.title = element_text(face = "bold", size = 14))
        
        # add model results if successful
        if (is.list(model)) {
          PsiMin <- model$PsiMin
          GRi <-  model$GRi
          Slope <- model$Slope
          corr <- model$Correlation
          
          # get combinations of wp and temp
          fcts <- df %>% distinct(PrimingWP, PrimingDuration)
          
          plt <- plt +
            labs(title = "Germination rates and hydropriming model fit") +
            geom_abline(intercept = GRi, slope = Slope, color = "blue")
          
          
          # add model annotation
          plt <- addParamsToPlot(plt, list(
            sprintf("psi[min](50)==%.2f", PsiMin),
            sprintf("GRi==%.4f", GRi),
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
