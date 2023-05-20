# ---- Hydro Time ---- #

# Static UI ----

HydroTimeUI <- function() {
  ns <- NS("hydroTime")
  
  tagList(
    h3(class = "tab-title", "Hydro time analysis"),
    div(class = "tab-info", "The hydro time model assumes a data set with germination temperature as a treatment condition. If you have additional treatments in your dataset, the model will average across those treatments and you may get unreliable or unexpected model results. Note: the model may fail to converge under certain max cumulative fraction values."),
    uiOutput(ns("content"))
  )
}


# Server ----

#' @references colValidation
#' 
#' @param `data` a `reactive()` data frame containing the loaded clean data
#' @param `ready` a `reactive()` boolean indicating if the model is ready

HydroTimeServer <- function(data, ready) {
  moduleServer(
    id = "hydroTime",
    function(input, output, session) {
      ns <- session$ns
      
      
      # Vars ----
      
      ## params ----
      params <- c("Ht", "PsiB50", "Sigma")
      
      ## paramRangeDefaults ----
      # (lower, start, upper)
      paramRangeDefaults <- list(
        Ht = c(1, 60, 1000),
        PsiB50 = c(-5, -.8, -1e-9),
        Sigma = c(1e-4, .2, 2)
      )
      
      ## rv $ setParams ----
      ## rv $ paramRanges ----
      rv <- reactiveValues(
        setParams = deframe(tibble(params, NA)),
        paramRanges = paramRangeDefaults
      )
      
      
      # Reactives ----
      
      ## workingData ----
      ## for model info and plot
      workingData <- reactive({
        req(ready())
        req(
          input$germWPSelect,
          input$dataCleanSelect,
          input$trtIdSelect,
          input$cumFracRange,
          input$maxCumFrac
        )
        
        df <- data() %>%
          filter(GermWP %in% input$germWPSelect) %>%
          filter(TrtID %in% input$trtIdSelect)
        
        # optionally remove repeated measurements at same cumulative fraction
        if (input$dataCleanSelect == "clean") {
          df <- df %>% distinct(TrtID, CumFraction, .keep_all = TRUE)
        }
        
        # filter based on cumulative fraction cutoffs
        df %>% filter(between(CumFraction * 100, input$cumFracRange[1], input$cumFracRange[2]))
      })
      
      # observe(print(workingData()))
      
      
      ## modelResults ----
      ## - list with model results or string with error
      modelResults <- reactive({
        req(ready())
        if (nrow(workingData()) == 0) return("No data")
        
        # collect data
        df <- workingData()
        wpData <- df$GermWP
        timeData <- df$CumTime
        germData <- df$CumFraction
        maxFrac <- input$maxCumFrac / 100
        defined <- lower <- start <- upper <- list()
        
        # check if a parameter is defined, or set its range constraints if not
        for (p in params) {
          paramValue <- rv$setParams[[p]]
          if (truthy(paramValue)) {
            defined[[p]] <- paramValue
            assign(p, paramValue)
          } else {
            if (exists(c(p))) remove(list = c(p))
            ranges <- rv$paramRanges[[p]]
            lower[[p]] <- ranges[1]
            start[[p]] <- ranges[2]
            upper[[p]] <- ranges[3]
          }
        }

        # run model, return message if fails
        tryCatch({
          model <- stats::nls(
            formula = germData ~ maxFrac * stats::pnorm(
              q = wpData - (Ht / timeData),
              mean = PsiB50,
              sd = Sigma
            ),
            start = start,
            lower = lower,
            upper = upper,
            algorithm = "port"
          )
          
          # grab coefs
          coefs <- summary(model)$coefficients %>%
            as_tibble(rownames = "Param") %>%
            select(1:2) %>%
            deframe() %>%
            as.list()
          
          # return results
          results <- list()
          for (p in params) {
            results[[p]] <- c(defined[[p]], coefs[[p]])[1]
          }
          results$Correlation <- stats::cor(germData, stats::predict(model)) ^ 2
          results
        },
          error = function(cond) { paste(cond[1]) }
        )
      })
      
      # observe(print(modelResults()))
      
      # Event Reactives ----
      
      ## Model coefficient inputs ----
      lapply(params, function(p) {
        id <- paste0(p, "_set")
        observeEvent(input[[id]], {
          val <- input[[id]]
          rv$setParams[[p]] <- ifelse(val != "", val, NA)
        })
      })
      
      
      
      # Outputs ----

      ## content // Main UI----
      output$content <- renderUI({
        req_cols <- colValidation$Column[colValidation$HydroTime]
        validate(need(ready(), paste("Please load a dataset with required columns for hydro time analysis. Minimum required columns are:", paste(req_cols, collapse = ", "))))
        
        germWPChoices <- colChoices(data(), "GermWP")
        
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
              column(12, trtSelectUI(ns, reactive(data()))),
              column(12, uiOutput(ns("dataSummary")))
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
            title = "Germination data and hydro time model fit",
            plotOutput(ns("plot"))
          )
        )
      })
      
      
      ## dataSummary ----
      ## data rows after filter
      output$dataSummary <- renderUI({
        n1 <- nrow(workingData())
        n2 <- nrow(data())
        pct <- round(n1 / n2 * 100, 0)
        wellPanel(
          align = "center",
          style = "font-size: larger; font-weight: bold;",
          sprintf("Using %s / %s data points (%s%%)", n1, n2, pct)
        )
      })


      
      ## plot ----
      output$plot <- renderPlot({
        req(ready())
        req(input$maxCumFrac)
        
        df <- workingData()
        model <- modelResults()
        maxGerm <- input$maxCumFrac / 100
        
        # generate the plot
        plt <- df %>%
          ggplot(aes(x = CumTime, y = CumFraction, color = as.factor(GermWP))) +
          annotate(
            "rect",
            xmin = 0,
            xmax = Inf,
            ymin = maxGerm,
            ymax = 1,
            fill = "grey",
            alpha = 0.1) +
          geom_hline(
            yintercept = maxGerm,
            color = "darkgrey",
            linetype = "dashed") +
          geom_point(
            shape = 19,
            size = 2) +
          scale_y_continuous(
            labels = scales::percent,
            expand = expansion(),
            limits = c(0, 1.02)) +
          scale_x_continuous(
            expand = expansion()) +
          expand_limits(x = 0, y = 0) +
          labs(
            x = "Time",
            y = "Cumulative fraction germinated (%)",
            color = "Water potential") +
          guides(color = guide_legend(reverse = T, order = 1)) +
          theme_classic()
        
        # add model results if successful
        if (is.list(model)) {
          ht <- model$Ht
          psiB50 <- model$PsiB50
          sigma <- model$Sigma
          corr <- model$Correlation
          
          # Plot all predicted treatments by the hydrotime model
          modelLines <- mapply(function(y) {
            stat_function(
              fun = function(x) {
                stats::pnorm(
                  q = y - (ht / x),
                  mean = psiB50,
                  sd = sigma
                ) * maxGerm
              },
              aes(color = as.factor(y))
            )
          },
            unique(df$GermWP)
          )
          
          plt <- plt + modelLines
          
          # add model annotation
          plt <- addParamsToPlot(plt, list(
            sprintf("~~HT==%.2f", ht),
            sprintf("~~Psi[b](50)==%.3f", psiB50),
            sprintf("~~sigma==%.3f", sigma),
            sprintf("~~R^2==%.2f", corr)
          ))
        }
        
        plt
      })
      
    } # end
  )
}
