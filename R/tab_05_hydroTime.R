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
      
      
      # content // Rendered UI ----
      
      output$content <- renderUI({
        req_cols <- colValidation$Column[colValidation$HydroTime]
        validate(need(ready(), paste("Please load a dataset with required columns for hydro time analysis. Minimum required columns are:", paste(req_cols, collapse = ", "))))
        
        germWPChoices <- unique(data()$GermWP)
        
        fluidRow(
          box(
            width = 6,
            title = "Model parameters",
            checkboxGroupInput(
              inputId = ns("germWPSelect"),
              label = "Included water potential levels:",
              choices = germWPChoices,
              selected = germWPChoices
            ),
            dataCleanSelect(ns)
          ),
          resultsTable(ns, reactive(modelResults())),
          box(
            width = 12,
            title = "Germination data and hydro time model fit",
            plotOutput(ns("plot"))
          ),
          trtIdSelect(ns, reactive(data())),
          cumFracSliders(ns)
        )
      })

      
      # workingData // for model and plot ----
      
      workingData <- reactive({
        req(
          ready(),
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
      
      
      # modelResults // list with model results or string with error ----
      
      modelResults <- reactive({
        
        # collect data
        df <- workingData()
        wp <- df$GermWP
        time <- df$CumTime
        germ <- df$CumFraction
        max_cum_frac <- input$maxCumFrac / 100
        
        # model params
        lower = list(ht = 1, psib50 = -5, sigma = 0.0001)
        start = list(ht = 60, psib50 = -0.8, sigma = 0.2)
        upper = list(ht = 1000, psib50 = -0.000000001, sigma = 2)
        
        # run model, return message if fails
        tryCatch({
          model <- stats::nls(
            formula = germ ~ max_cum_frac * stats::pnorm(
              wp - (ht / time),
              psib50,
              sigma
            ),
            start = start,
            lower = lower,
            upper = upper,
            algorithm = "port"
          )
          
          # grab coefs
          corr <- stats::cor(germ, stats::predict(model)) ^ 2
          ht <- summary(model)$coefficients[[1]]
          psib50 <- summary(model)$coefficients[[2]]
          sigma <- summary(model)$coefficients[[3]]
          
          # return results
          list(
            HT = ht,
            Psib50 = psib50,
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
        req(input$maxCumFrac)
        
        df <- workingData()
        model <- modelResults()
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
        if (is.list(model)) {
          ht <- model$HT
          psib50 <- model$Psib50
          sigma <- model$Sigma
          corr <- model$Correlation
          
          # Plot all predicted treatments by the hydrotime model
          modelLines <- mapply(function(wp) {
            stat_function(
              fun = function(x) {
                stats::pnorm(
                  q = wp - (ht / x),
                  mean = psib50,
                  sd = sigma
                ) * germ_cutoff
              },
              aes(color = as.factor(wp))
            )
          },
            unique(df$GermWP)
          )
          
          # model params
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
        }
        
        plt
      })
      
    } # end
  )
}
