# ---- Thermal Time ---- #

# Static UI ----

ThermalTimeUI <- function() {
  ns <- NS("thermalTime")
  
  tagList(
    h3(class = "tab-title", "Thermal time analysis"),
    div(class = "tab-info", "The thermal time model assumes a data set with germination temperature as a treatment condition. If you have additional treatments in your dataset, the model will display those treatments and include them in the model calculation which can result on unreliable or unexpected results. Please inform and remove additonal treatments when pertinent in the lower left corner of this section. Note: the model may fail to converge under certain max cumulative fraction values."),
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
      
      
      # Vars -------------------------------------------------------------------
      
      ## params ----
      params <- c("Tb", "ThetaT50", "Sigma")
      
      ## paramRangeDefaults ----
      # (lower, start, upper)
      paramRangeDefaults <- list(
        Tb = c(0, 6, 20),
        ThetaT50 = c(0.5, 3, 50),
        Sigma = c(.0001, .09, 1.5)
      )
      
      ## rv $ setParams ----
      ## rv $ paramRanges ----
      rv <- reactiveValues(
        setParams = list(
          Tb = NA,
          ThetaT50 = NA,
          Sigma = NA
        ),
        paramRanges = paramRangeDefaults
      )
      
      
      # Reactives --------------------------------------------------------------
      
      ## workingData ----
      ## modified dataset for model run
      workingData <- reactive({
        req(ready())
        req(
          input$germTempSelect,
          input$dataCleanSelect,
          input$trtIdSelect,
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
      
      # observe(print(workingData()))
      
      
      ## modelResults ----
      ## list with model results or string with error
      modelResults <- reactive({
        req(ready())
        
        if (nrow(workingData()) == 0) return("No data")
        
        # collect data
        df <- workingData()
        tempVec <- df$GermTemp
        timeVec <- df$CumTime
        germVec <- df$CumFraction
        maxCumFrac <- input$maxCumFrac / 100
        defined <- lower <- start <- upper <- list()
        
        # model params: Tb, ThetaT50, Sigma
        # check if a parameter is defined, or set its range constraints
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
        suppressWarnings(
          tryCatch({
            model <- stats::nls(
              formula = germVec ~ maxCumFrac * stats::pnorm(
                q = log10(timeVec),
                mean = ThetaT50 - log10(tempVec - Tb),
                sd = Sigma
              ),
              start = start,
              lower = lower,
              upper = upper,
              algorithm = "port"
            )
            
            # grab coefs
            # TODO: also return param p-values etc?
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
            results$Correlation <- stats::cor(germVec, stats::predict(model)) ^ 2
            results
          },
            error = function(cond) { paste(cond[1]) }
          )
        )
      })
      
      # observe(print(modelResults()))
      
      
      
      # Event Reactives --------------------------------------------------------
      
      ## Model coefficient inputs ----
      lapply(params, function(p) {
        id <- paste0(p, "_set")
        observeEvent(input[[id]], {
          val <- input[[id]]
          rv$setParams[[p]] <- ifelse(val != "", val, NA)
        })
      })
      
      
      
      # Outputs ----------------------------------------------------------------
      
      ## content // main UI ----
      output$content <- renderUI({
        req_cols <- colValidation$Column[colValidation$ThermalTime]
        validate(need(ready(), paste("Please load a dataset with required columns for thermal time analysis. Minimum required columns are:", paste(req_cols, collapse = ", "))))
        
        germTempChoices <- unique(data()$GermTemp)
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            title = "Data selection",
            fluidRow(
              column(6,
                div(class = "well-title", "Data input options"),
                div(
                  class = "well",
                  checkboxGroupInput(
                    inputId = ns("germTempSelect"),
                    label = "Included temperature levels:",
                    choices = germTempChoices,
                    selected = germTempChoices
                  ),
                  dataCleanSelect(ns)
                )
              ),
              column(6, cumFracSliders(ns)),
              column(12, trtIdSelect(ns, reactive(data())))
            ),
            div(class = "well", uiOutput(ns("dataSummary")))
          ),
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            title = "Model parameters",
            fluidRow(
              column(6,
                div(class = "well-title", "Specify model coefficients (optional)"),
                div(
                  class = "well",
                  uiOutput(ns("setParams")),
                  p(em("Specify individual model coefficients, or leave blank to allow the model to find a best-fit value."))
                ),
              ),
              column(6, resultsTable(ns, reactive(modelResults())))
            )
          ),
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            title = "Germination plot and thermal time sub-optimal model fit",
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
        div(
          align = "center",
          style = "font-size: larger; font-weight: bold;",
          sprintf("Using %s / %s data points (%s%%)", n1, n2, pct)
        )
      })
      
      ## setParams ----
      ## numeric inputs for each param
      output$setParams <- renderUI({
        div(
          class = "flex-row",
          lapply(params, function(p) {
            numericInput(
              inputId = ns(paste0(p, "_set")),
              label = p,
              value = NA,
              step = .1,
              width = "30%"
            )
          })
        )
      })
      
      
      ## Plot ----
      output$plot <- renderPlot({
        req(ready())
        
        df <- workingData()
        model <- modelResults()
        max_cum_frac <- input$maxCumFrac / 100

        # generate the plot
        plt <- df %>%
          ggplot(aes(x = CumTime, y = CumFraction, color = as.factor(GermTemp))) +
          annotate(
            "rect",
            xmin = 0,
            xmax = Inf,
            ymin = max_cum_frac,
            ymax = 1,
            fill = "grey",
            alpha = 0.1) +
          geom_hline(
            yintercept = max_cum_frac,
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
          labs(
            x = "Time",
            y = "Cumulative fraction germinated (%)",
            color = "Temperature") +
          guides(color = guide_legend(reverse = T, order = 1)) +
          theme_classic()

        # add model results if successful
        if (is.list(model)) {
          maxCumFrac <- model$MaxCumFrac
          tb <- model$Tb
          thetaT50 <- model$ThetaT50
          sigma <- model$Sigma
          corr <- model$Correlation
          
          # Plot all predicted treatments by the thermal time model
          modelLines <- mapply(function(temp) {
            stat_function(
              fun = function(x) {
                stats::pnorm(
                  q = log10(x),
                  mean = thetaT50 - log10(temp - tb),
                  sd = sigma
                ) * max_cum_frac
              },
              aes(color = as.factor(temp))
            )
          },
            unique(df$GermTemp)
          )
          
          plt <- plt + modelLines
          
          # add model annotation
          plt <- addParamsToPlot(plt, list(
            sprintf("~~T[b]==%.1f", tb),
            sprintf("~~ThetaT(50)==%.3f", thetaT50),
            sprintf("~~sigma==%.3f", sigma),
            sprintf("~~R^2==%.2f", corr)
          ))
        }

        plt
      })
      
    } # end
  )
}
