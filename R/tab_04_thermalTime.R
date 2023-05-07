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
      
      
      # content // Rendered UI ----
      
      output$content <- renderUI({
        req_cols <- colValidation$Column[colValidation$ThermalTime]
        validate(need(ready(), paste("Please load a dataset with required columns for thermal time analysis. Minimum required columns are:", paste(req_cols, collapse = ", "))))
        
        germTempChoices <- unique(data()$GermTemp)
        
        fluidRow(
          box(
            width = 6,
            title = "Model data input",
            checkboxGroupInput(
              inputId = ns("germTempSelect"),
              label = "Included temperature levels:",
              choices = germTempChoices,
              selected = germTempChoices
            ),
            dataCleanSelect(ns)
          ),
          resultsTable(ns, reactive(modelResults())),
          box(
            width = 12,
            title = "Germination plot and thermal time sub-optimal model fit",
            plotOutput(ns("plot"))
          ),
          trtIdSelect(ns, reactive(data())),
          cumFracSliders(ns)
        )
      })
      
      
      # workingData // modified dataset for model run ----
      
      workingData <- reactive({
        req(
          ready(),
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
      
      
      # modelResults // list with model results or string with error ----
      
      modelResults <- reactive({
        
        # collect data
        df <- workingData()
        temp <- df$GermTemp
        time <- df$CumTime
        germ <- df$CumFraction
        max_cum_frac <- input$maxCumFrac / 100
        
        # model params
        lower <- list(tb = 0, thetaT50 = 0.5, sigma = 0.0001)
        start <- list(tb = 6, thetaT50 = 3, sigma = 0.09)
        upper <- list(tb = 20, thetaT50 = 50, sigma = 1.50)
        
        # run model, return message if fails
        tryCatch({
          model <- stats::nls(
            formula = germ ~ max_cum_frac * stats::pnorm(
              log10(time),
              thetaT50 - log10(temp - tb),
              sigma
            ),
            start = start,
            lower = lower,
            upper = upper,
            algorithm = "port"
          )
          
          # grab coefs
          corr <- stats::cor(germ, stats::predict(model)) ^ 2
          tb <- summary(model)$coefficients[[1]]
          thetaT50 <- summary(model)$coefficients[[2]]
          sigma <- summary(model)$coefficients[[3]]
          
          # return results
          list(
            Tb = tb,
            ThetaT50 = thetaT50,
            Sigma = sigma,
            Correlation = corr
          )
        },
          error = function(cond) {
            paste("Unable to compute model, try adjusting parameters. Reason:", str_to_sentence(cond[1]))
          }
        )
      })
      
      # observe(print(modelResults()))
      
      
      # plot ----
      
      output$plot <- renderPlot({
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
          
          # model parameters
          par1 <- paste("~~T[b]==", round(tb, 1))
          par2 <- paste("~~ThetaT(50)==", round(thetaT50, 3))
          par3 <- paste("~~sigma==", round(sigma, 3))
          par4 <- paste("~~R^2==", round(corr, 2))
          
          plt <- plt +
            modelLines +
            annotate("text", x = -Inf, y = 0.95, label = " Model parameters:", color = "grey0", hjust = 0) +
            annotate("text", x = -Inf, y = 0.9, label = par1, color = "grey0", hjust = 0, parse = T) +
            annotate("text", x = -Inf, y = 0.85, label = par2, color = "grey0", hjust = 0, parse = T) +
            annotate("text", x = -Inf, y = 0.8, label = par3, color = "grey0", hjust = 0, parse = T) +
            annotate("text", x = -Inf, y = 0.75, label = par4, color = "grey0", hjust = 0, parse = T)
        }

        plt
      })
      
    } # end
  )
}
