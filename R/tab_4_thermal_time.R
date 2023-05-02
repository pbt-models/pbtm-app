# ---- Thermal Time ---- #

# UI ----

thermalTimeUI <- function() {
  ns <- NS("thermalTime")
  
  tagList(
    h3(class = "tab-title", "Thermal time analysis"),
    uiOutput(ns("content"))
  )
}


# Server ----

#' @references colValidation
#' 
#' @param `data` a `reactive()` data frame containing the loaded clean data
#' @param `ready` a `reactive()` boolean indicating if the model is ready

thermalTimeServer <- function(data, ready) {
  moduleServer(
    id = "thermalTime",
    function(input, output, session) {
      ns <- session$ns
      
      
      # Reactives ----
      
      ## dataReady ----
      dataReady <- reactive({
        truthy(data()) & ready()
      })
      
      ## ttsoData // Thermal time suboptimal model data ----
      ttsoData <- reactive({
        req(
          dataReady(),
          # input$germTempSelect,
          input$dataCleanSelect,
          input$trtIdSelect,
          input$cumFracRange,
          input$maxCumFrac
        )
        
        df <- data() %>%
          # filter(GermTemp %in% input$germTempSelect) %>%
          filter(TrtID %in% input$trtIdSelect)
        
        # optionally remove repeated measurements at same cumulative fraction
        if (input$dataCleanSelect == "clean") {
          df <- df %>%
            distinct(TrtID, CumFraction, .keep_all = TRUE)
        }
        
        # filter based on cumulative fraction cutoffs
        df %>% filter(between(CumFraction * 100, input$cumFracRange[1], input$cumFracRange[2]))
      })
      
      
      ## ttsoResults // list with TTSO model results ----
      ttsoResults <- reactive({
        df <- ttsoData()
        temp <- df$GermTemp
        time <- df$CumTime
        germ <- df$CumFraction
        max_cum_frac <- input$maxCumFrac / 100
        
        # run model, return message if fails
        tryCatch({
          model <- stats::nls(
            formula = germ ~ max_cum_frac * stats::pnorm(
              log(time, base = 10),
              mean = thetaT50 - log(temp - Tb, base = 10),
              sd = sigma,
              log = FALSE),
            start = list(
              Tb = 6,
              thetaT50 = 3,
              sigma = 0.09),
            lower = list(
              Tb = 0,
              thetaT50 = 0.5,
              sigma = 0.0001),
            upper = list(
              Tb = 20,
              thetaT50 = 50,
              sigma = 1.50),
            algorithm = "port")
          
          # grab coefs
          Corr <- stats::cor(germ, stats::predict(model)) ^ 2
          Tb <- summary(model)$coefficients[[1]]
          ThetaT50 <- summary(model)$coefficients[[2]]
          Sigma <- summary(model)$coefficients[[3]]
          
          # return results
          list(
            Tb = Tb,
            ThetaT50 = ThetaT50,
            Sigma = Sigma,
            Correlation = Corr
          )
        },
          error = function(cond) {
            paste("Unable to compute model, try adjusting parameters. Reason:", str_to_sentence(cond[1]))
          }
        )
      })
      
      
      # Observers ----
      
      # observe(print(ttsoData()))
      observe(print(ttsoResults()))
      
      
      # Outputs ----
      
      ## content // main UI ----
      output$content <- renderUI({
        req_cols <- colValidation$Column[colValidation$ThermalTime]
        validate(need(dataReady(), paste("Please load a dataset with required columns for thermal time analysis. Minimum required columns are:", paste(req_cols, collapse = ", "))))
        
        germTempChoices <- unique(data()$GermTemp)
        
        fluidRow(
          column(12, p(em("The thermal time model assumes a data set with germination temperature as a treatment condition. If you have additional treatments in your dataset, the model will display those treatments and include them in the model calculation which can result on unreliable or unexpected results. Please inform and remove additonal treatments when pertinent in the lower left corner of this section. Note: the model may fail to converge under certain max cumulative fraction values."))),
          box(
            title = "Model data input",
            checkboxGroupInput(
              inputId = ns("germTempSelect"),
              label = "Included temperature levels:",
              choices = germTempChoices,
              selected = germTempChoices
            ),
            radioButtons(
              inputId = ns("dataCleanSelect"),
              label = "Select data cleaning:",
              choices = list(
                "Original" = "original",
                "Cleaned (remove duplicates)" = "clean"
              )
            )
          ),
          box(
            title = "Model results",
            tableOutput(ns("ttsoTable"))
          ),
          box(
            width = 12,
            plotOutput(ns("ttPlot"))
          ),
          box(
            title = "Additional data filters",
            uiOutput(ns("trtIdSelect"))
          ),
          box(
            title = "Maximum germination (%) observed",
            sliderInput(
              inputId = ns("maxCumFrac"),
              label = NULL,
              min = 10,
              max = 100,
              value = 100,
              step = 1)
          ),
          box(
            title = "Included interval (%):",
            sliderInput(
              inputId = ns("cumFracRange"),
              label = NULL,
              min = 0,
              max = 100,
              value = c(0, 100))
          )
        )
      })
      
      
      ## trtIdSelect // Create checkbox with all TrtID levels to be included in the model analysis ----
      output$trtIdSelect <- renderUI({
        if ("TrtDesc" %in% names(data())) {
          choices <- data() %>%
            filter(GermTemp %in% input$germTempSelect) %>%
            mutate(Label = paste(TrtID, TrtDesc, sep = ": ")) %>%
            distinct(Label, TrtID) %>%
            deframe()
        } else {
          choices <- unique(data()$TrtID)
        }

        
        checkboxGroupInput(
          inputId = ns("trtIdSelect"),
          label = "Treatment ID:",
          choices = choices,
          selected = choices
        )
      })
      
      
      ## ttsoTable // TTSO model results in table format ----
      output$ttsoTable <- renderTable({
        results <- ttsoResults()

        # print error message if model fails
        validate(need(is.list(results), results))

        # convert results list to data frame
        results %>%
          enframe() %>%
          unnest(value) %>%
          rename(
            Parameter = name,
            Value = value
          )
      },
        digits = 4,
        width = "100%"
      )

      
      ## ttPlot // TTSO plot ----
      output$ttPlot <- renderPlot({
        df <- ttsoData()
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
            title = "Thermal Time Sub-optimal Model",
            x = "Time",
            y = "Cumulative fraction germinated (%)",
            color = "Temperature") +
          guides(color = guide_legend(reverse = T, order = 1)) +
          theme_classic()

        # add model results if successful
        if (is.list(ttsoResults())) {
          try({
            model <- ttsoResults()
            maxCumFrac <- model$MaxCumFrac
            tb <- model$Tb
            thetaT50 <- model$ThetaT50
            sigma <- model$Sigma
            corr <- model$Correlation
            
            # Plot all predicted treatments by the thermal time model
            modelLines <- mapply(function(temp) {
              stat_function(
                fun = function(x) {
                  stats::pnorm(log(x, base = 10), thetaT50 - log(temp - tb, base = 10),  sigma, log = F) * max_cum_frac
                },
                aes(color = as.factor(temp))
              )
            },
              unique(df$GermTemp)
            )
            
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
          })
        }

        plt
      })
      
      
    } # end
  )
}
