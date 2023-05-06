# ---- Hydrothermal Time ---- #

# Static UI ----

hydrothermalTimeUI <- function() {
  ns <- NS("hydrothermalTime")
  
  tagList(
    h3(class = "tab-title", "Hydro time analysis"),
    div(class = "tab-info", "The hydrothermal time model assumes a data set with germination temperature and germination water potential as treatment conditions. If you have additional treatments in your dataset, the model will average across those treatments and you may get unreliable or unexpected model results. Note: the model may fail to converge under certain max cumulative fraction values."),
    uiOutput(ns("content"))
  )
}


# Server ----

#' @references colValidation
#' 
#' @param `data` a `reactive()` data frame containing the loaded clean data
#' @param `ready` a `reactive()` boolean indicating if the model is ready

hydrothermalTimeServer <- function(data, ready) {
  moduleServer(
    id = "hydrothermalTime",
    function(input, output, session) {
      ns <- session$ns
      
      
      # content // Rendered UI ----
      output$content <- renderUI({
        req_cols <- colValidation$Column[colValidation$HydrothermalTime]
        validate(need(ready(), "Please load required data for hydrothermal time analysis. Minimum required columns are:", paste(req_cols, collapse = ", ")))
        
        germWPChoices <- unique(data()$GermWP)
        germTempChoices <- unique(data()$GermTemp)
        
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
            checkboxGroupInput(
              inputId = ns("germTempSelect"),
              label = "Included temperature levels:",
              choices = germTempChoices,
              selected = germTempChoices
            ),
            dataCleanSelect(ns),
            numericInput(
              inputId = ns("baseTemp"),
              label = "Base temperature:",
              value = NULL
            )
          ),
          resultsTable(ns, reactive(modelResults())),
          box(
            width = 12,
            title = "Germination data and hydrothermal time model fit",
            plotOutput(ns("plot"))
          ),
          trtIdSelect(ns, reactive(data())),
          cumFracSliders(ns)
        )
      })
      
      
      # workingData // modified data for model ----
      workingData <- reactive({
        req(
          ready(),
          input$dataCleanSelect,
          input$germWPSelect,
          input$germTempSelect,
          input$trtIdSelect,
          input$cumFracRange,
          input$maxCumFrac
        )
        
        df <- data() %>%
          filter(GermWP %in% input$germWPSelect) %>%
          filter(GermTemp %in% input$germTempSelect) %>%
          filter(TrtID %in% input$trtIdSelect)
        
        # optionally remove repeated measurements at same cumulative fraction
        if (input$dataCleanSelect == "clean") {
          df <- df %>% distinct(TrtID, CumFraction, .keep_all = TRUE)
        }
        
        # filter based on cumulative fraction cutoffs
        df %>% filter(between(CumFraction * 100, input$cumFracRange[1], input$cumFracRange[2]))
      })
      
      # observe(print(workingData()))
      
      
      # modelResults // list of model coefficients, or an error message ----
      modelResults <- reactive({
        
        # collect data
        df <- workingData()
        wp <- df$GermWP
        temp <- df$GermTemp
        time <- df$CumTime
        germ <- df$CumFraction
        max_cum_frac <- input$maxCumFrac / 100
        base_temp <- input$baseTemp
        
        # set model params
        lower <- list(ht = 1, psib50 = -5, sigma = 0.0001)
        start <- list(ht = 800, psib50 = -1, sigma = 0.4)
        upper <- list(ht = 5000, psib50 = 0, sigma = 10)
        
        # if base temp supplied, use it, or add defaults to params
        if (truthy(base_temp)) {
          tb <- base_temp
        } else {
          start$tb <- 1
          lower$tb <- 0
          upper$tb <- 15
        }
        
        # try to run the model
        tryCatch({
          model <- stats::nls(
            germ ~ max_cum_frac * stats::pnorm(
              wp - (ht / ((temp - tb) * time)),
              psib50,
              sigma
            ),
            start = start,
            lower = lower,
            upper = upper,
            algorithm = "port"
          )
          
          # get coefs
          corr <- stats::cor(germ, stats::predict(model)) ^ 2
          ht <- summary(model)$coefficients[[1]]
          psib50 <- summary(model)$coefficients[[2]]
          sigma <- summary(model)$coefficients[[3]]
          if (truthy(base_temp)) {
            tb <- base_temp
          } else {
            tb <- summary(model)$coefficients[[4]]
          }
          
          # return results
          list(
            HT = ht,
            Tb = tb,
            Psib50 = psib50,
            Sigma = sigma,
            Correlation = corr
          )
        },
          error = function(cond) {
            paste("Unable to compute model, try adjusting parameters. ", str_to_sentence(cond[1]))
          }
        )
      })
      
      # observe(print(modelResults()))
      
      
      # plot ----
      output$plot <- renderPlot({
        req(
          workingData(),
          modelResults(),
          input$maxCumFrac,
          input$cumFracRange
        )
        
        df <- workingData()
        germ_cutoff <- input$maxCumFrac / 100
        model <- modelResults()
        
        # generate the plot
        plt <- df %>%
          ggplot(aes(
            x = CumTime,
            y = CumFraction,
            color = as.factor(GermWP))) +
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
            aes(shape = as.factor(GermTemp)),
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
            color = "Water Potential",
            shape = "Temperature") +
          guides(
            color = guide_legend(reverse = T, order = 1),
            linetype = "none") +
          theme_classic()
        
        # use try so it will still plot on model error
        if (is.list(model)) {
          ht <- model$HT
          psib50 <- model$Psib50
          tb <- model$Tb
          sigma <- model$Sigma
          corr <- model$Correlation
          
          # get combinations of wp and temp
          fcts <- df %>% distinct(GermWP, GermTemp)
          
          # function to plot all predicted treatments by the hydro thermal time model
          modelLines <- mapply(function(wp, temp) {
            stat_function(
              fun = function(x) {
                stats::pnorm(
                  wp - (ht / ((temp - tb) * x)),
                  psib50,
                  sigma
                ) * germ_cutoff
              },
              aes(
                color = as.factor(wp),
                linetype = as.factor(temp),
                group = interaction(wp, temp)
              )
            )
          },
            fcts$GermWP,
            fcts$GermTemp
          )
          
          # model params
          par1 <- paste("~~HT==", round(ht, 2))
          par2 <- paste("~~T[b]==", round(tb, 2))
          par3 <- paste("~~psi[b][50]==", round(psib50,3))
          par4 <- paste("~~sigma == ", round(sigma, 3))
          par5 <- paste("~~R^2 == ", round(corr, 2))
          
          plt <- plt +
            modelLines +
            annotate("text", x = -Inf, y = 0.95, label = " Model Parameters:", color = "grey0", hjust = 0) +
            annotate("text", x = -Inf, y = 0.9, label = par1, color = "grey0", hjust = 0, parse = T) +
            annotate("text", x = -Inf, y = 0.85, label = par2, color = "grey0", hjust = 0, parse = T) +
            annotate("text", x = -Inf, y = 0.8, label = par3, color = "grey0", hjust = 0, parse = T) +
            annotate("text", x = -Inf, y = 0.75, label = par4, color = "grey0", hjust = 0, parse = T) +
            annotate("text", x = -Inf, y = 0.7, label = par5, color = "grey0", hjust = 0, parse = T)
        }
        
        plt
      })
      
      
      
      
      
      
      
      
      
      
      
    } # end
  )
}


