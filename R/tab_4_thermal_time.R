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
#' @references nCols
#' 
#' @param `data` a `reactive()` data frame containing the loaded clean data
#' @param `ready` a `reactive()` boolean indicating if the model is ready
#' @param `trtChoices` a `reactive()` vector listing the factor columns in the dataset

thermalTimeServer <- function(data, ready, trtChoices) {
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
          input$germTempSelect,
          input$trtIdSelect,
          input$germWPSelect,
          input$dataCleanSelect,
          input$germFracRange
        )
        
        df <- data() %>%
          filter(GermTemp %in% input$germTempSelect) %>%
          filter(TrtID %in% input$trtIdSelect) %>%
          filter(GermWP %in% input$germWPSelect)
        
        # Update data input type based on radio button selection (Original or Cleaned)
        if (input$dataCleanSelect == "clean") {
          df <- df %>%
            distinct(TrtID, CumFraction, .keep_all = TRUE)
        }
        
        # filter based on cumulative fraction cutoffs
        df <- df %>%
          filter(between(CumFraction * 100, input$germFracRange[1], input$germFracRange[2]))
      })
      
      
      ## ttsoResults // list with TTSO model results ----
      ttsoResults <- reactive({
        req(input$ttsoMaxCumFrac)
        
        df <- ttsoData()
        temp <- df$GermTemp
        time <- df$CumTime
        germ <- df$CumFraction
        max_cum_frac <- input$ttsoMaxCumFrac / 100
        
        tryCatch({
          # run model
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
            paste("Unable to compute model, try adjusting parameters. ", str_to_sentence(cond[1]))
          }
        )
      })
      
      
      # Observers ----
      
      # observe(print(ttsoData()))
      # observe(print(ttsoResults()))
      
      
      # Outputs ----
      
      ## content // main UI ----
      output$content <- renderUI({
        validate(
          need(
            dataReady(),
            paste("Please load a dataset and set required column types for thermal time analysis. Minimum required columns are:", paste(colValidation$Column[colValidation$ThermalTime], collapse = ", "))
          )
        )
        
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
            uiOutput(ns("trtIdSelect")),
            uiOutput(ns("germWPSelect"))
          ),
          box(
            title = "Maximum germination (%) observed",
            sliderInput(
              inputId = ns("ttsoMaxCumFrac"),
              label = NULL,
              min = 10,
              max = 100,
              value = 100,
              step = 1)
          ),
          box(
            title = "Included interval (%):",
            sliderInput(
              inputId = ns("germFracRange"),
              label = NULL,
              min = 0,
              max = 100,
              value = c(0,100))
          )
        )
      })
      

      ## trtIdSelect // Create checkbox with all TrtID levels to be included in the model analysis ----
      output$trtIdSelect <- renderUI({
        choices <- data() %>%
          filter(GermTemp %in% input$germTempSelect) %>%
          mutate(Label = paste(TrtID, TrtDesc, sep = ": ")) %>%
          distinct(Label, TrtID) %>%
          deframe()
        
        checkboxGroupInput(
          inputId = ns("trtIdSelect"),
          label = "Treatment ID:",
          choices = choices,
          selected = choices
        )
      })
      
      ## germWPSelect // Create checkbox with all WP levels to be included in the model analysis ----
      output$germWPSelect <- renderUI({
        req("GermWP" %in% trtChoices())
        choices <- unique(data()$GermWP)
          
        checkboxGroupInput(
          inputId = ns("germWPSelect"),
          label = "Water Potential:",
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
        req(input$ttsoMaxCumFrac)
        max_cum_frac <- input$ttsoMaxCumFrac / 100

        # Load working dataset
        df <- ttsoData()

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
          # expand_limits(x = 0, y = 0) +
          labs(
            title = "Thermal Time Sub-optimal Model",
            x = "Time",
            y = "Cumulative fraction germinated (%)",
            color = "Temperature") +
          guides(color = guide_legend(reverse = T, order = 1)) +
          theme_classic()

        # use try so it will still plot on model error
        
        if (is.list(ttsoResults())) {
          try({
            model <- ttsoResults()
            
            maxCumFrac <- model$MaxCumFrac
            tb <- model$Tb
            thetaT50 <- model$ThetaT50
            sigma <- model$Sigma
            corr <- model$Correlation
            
            # Plot all predicted treatments by the thermal time model
            temps <- unique(df$GermTemp)
            modelLines <- mapply(function(temp) {
              stat_function(
                fun = function(x) {
                  stats::pnorm(log(x, base = 10), thetaT50 - log(temp - tb, base = 10),  sigma, log = F) * max_cum_frac
                },
                aes(color = as.factor(temp))
              )
            },
              temps
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
      
    }
  )
}
