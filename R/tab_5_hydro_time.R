# ---- Hydro Time ---- #

# UI ----

hydroTimeUI <- function() {
  ns <- NS("hydroTime")
  
  tagList(
    h3(class = "tab-title", "Hydro time analysis"),
    uiOutput(ns("content"))
  )
}


# Server ----

#' @references colValidation
#' @references nCols
#' 
#' @param `data` a `reactive()` data frame containing the loaded clean data
#' @param `ready` a `reactive()` boolean indicating if the model is ready

hydroTimeServer <- function(data, ready) {
  moduleServer(
    id = "hydroTime",
    function(input, output, session) {
      ns <- session$ns
      
      
      # Reactives ----
      
      ## dataReady ----
      dataReady <- reactive({
        truthy(data()) & ready()
      })

      
      ## htData // base data for model run ----
      htData <- reactive({
        req(
          dataReady(),
          # input$germWPSelect,
          input$dataCleanSelect,
          input$trtIdSelect,
          input$cumFracRange,
          input$maxCumFrac
        )
        
        df <- data() %>%
          # filter(GermWP %in% input$germWPSelect) %>%
          filter(TrtID %in% input$trtIdSelect)
        
        # optionally remove repeated measurements at same cumulative fraction
        if (input$dataCleanSelect == "clean") {
          df <- df %>%
            distinct(TrtID, CumFraction, .keep_all = TRUE)
        }
        
        # filter based on cumulative fraction cutoffs
        df %>% filter(between(CumFraction * 100, input$cumFracRange[1], input$cumFracRange[2]))
      })
      
      
      ## htResults // hydro time model results ----
      htResults <- reactive({
        df <- htData()
        wp <- df$GermWP
        time <- df$CumTime
        germ <- df$CumFraction
        max_cum_frac <- input$maxCumFrac / 100
        
        # run model, return message if fails
        tryCatch({
          model <- stats::nls(
            formula = germ ~ max_cum_frac * stats::pnorm(
              wp - (HT / time),
              Psib50,
              Sigma,
              log = FALSE),
            start = list(
              HT = 60,
              Psib50 = -0.8,
              Sigma = 0.2),
            lower = list(
              HT = 1,
              Psib50 = -5,
              Sigma = 0.0001),
            upper = list(
              HT = 1000,
              Psib50 = -0.000000001,
              Sigma = 2),
            algorithm = "port")
          
          # grab coefs
          corr <- stats::cor(germ, stats::predict(model)) ^ 2
          HT <- summary(model)$coefficients[[1]]
          Psib50 <- summary(model)$coefficients[[2]]
          Sigma <- summary(model)$coefficients[[3]]
          
          # return results
          list(
            HT = HT,
            Psib50 = Psib50,
            Sigma = Sigma,
            Correlation = corr)
        },
          error = function(cond) {
            paste("Unable to compute model, try adjusting parameters. Reason:", str_to_sentence(cond[1]))
          }
        )
      })
      
      
      # Observers ----
      
      # observe(print(htData()))
      observe(print(htResults()))
      
      
      # Outputs ----
      
      ## content // main UI ----
      output$content <- renderUI({
        req_cols <- colValidation$Column[colValidation$HydroTime]
        validate(need(dataReady(), paste("Please load a dataset with required columns for hydro time analysis. Minimum required columns are:", paste(req_cols, collapse = ", "))))
        
        germWPChoices <- unique(data()$GermWP)
        
        fluidRow(
          column(12, p(em("The hydrotime model assumes a data set with germination temperature as a treatment condition. If you have additional treatments in your dataset, the model will average across those treatments and you may get unreliable or unexpected model results. Note: the model may fail to converge under certain max cumulative fraction values."))),
          box(
            title = "Model data input",
            checkboxGroupInput(
              inputId = ns("germWPSelect"),
              label = "Included water potential levels:",
              choices = germWPChoices,
              selected = germWPChoices
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
            tableOutput(ns("htTable"))
          ),
          box(
            width = 12,
            plotOutput(ns("htPlot"))
          ),
          box(
            title = "Additional data filters",
            uiOutput(ns("trtIdSelect")),
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
            filter(GermWP %in% input$germWPSelect) %>%
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
      

      ## htTable // render model results as table ----
      output$htTable <- renderTable({
        results <- htResults()
        
        # print error message if model fails
        validate(need(is.list(results), results))
        
        # convert list to simple data frame
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
      
      ## htPlot // Hydro time plot ----
      output$htPlot <- renderPlot({
        req(htData(), htResults(), input$maxCumFrac)
        
        df <- htData()
        germ_cutoff <- input$maxCumFrac / 100
        
        # generate the plot
        plt <- df %>%
          ggplot(aes(x = CumTime, y = CumFraction, color = as.factor(GermWP))) +
          annotate(
            "rect",
            xmin = 0,
            xmax = Inf,
            ymin = germ_cutoff,
            ymax = 1,
            fill = "grey",
            alpha = 0.1) +
          geom_hline(
            yintercept = germ_cutoff,
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
            title = "Hydro Time Model",
            x = "Time",
            y = "Cumulative fraction germinated (%)",
            color = "Water potential") +
          guides(color = guide_legend(reverse = T, order = 1)) +
          theme_classic()
        
        # add model results if successful
        if (is.list(htResults())) {
          try({
            model <- htResults()
            maxCumFrac <- model$MaxCumFrac
            ht <- model$HT
            psib50 <- model$Psib50
            sigma <- model$Sigma
            corr <- model$Correlation
            
            # Plot all predicted treatments by the hydrotime model
            modelLines <- mapply(function(wp) {
              stat_function(
                fun = function(x) {
                  stats::pnorm(wp - (ht / x), psib50, sigma, log = FALSE) * germ_cutoff
                },
                aes(color = as.factor(wp))
              )
            },
              unique(df$GermWP)
            )
            
            par1 <- paste("~~HT==", round(ht, 2))
            par2 <- paste("~~Psi[b](50)==", round(psib50, 3))
            par3 <- paste("~~sigma== ", round(sigma, 3))
            par4 <- paste("~~R^2== ", round(corr, 2))
            
            plt <- plt +
              modelLines +
              annotate("text", x = -Inf, y = 0.95, label = " Model parameters", color = "grey0", hjust = 0) +
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
