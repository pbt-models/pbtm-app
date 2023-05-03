# ---- Hydrothermal Time ---- #

# UI ----

hydrothermalTimeUI <- function() {
  ns <- NS("hydrothermalTime")
  
  tagList(
    h3(class = "tab-title", "Hydro time analysis"),
    div(class = "tab-info", "The hydrothermal time model assumes a data set with germination temperature and germination water potential as treatment conditions. If you have additional treatments in your dataset, the model will average across those treatments and you may get unreliable or unexpected model results. Note: the model may fail to converge under certain max cumulative fraction values."),
    br(),
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
      
      # Reactives ----
      
      ## dataReady ----
      dataReady <- reactive({
        truthy(data()) & ready()
      })
      
      ## workingData // modified data for model ----
      workingData <- reactive({
        req(
          dataReady(),
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
      
      
      ## modelResults // list of model coefficients, or an error message ----
      modelResults <- reactive({
        
        # collect data
        df <- workingData()
        wp <- df$GermWP
        temp <- df$GermTemp
        time <- df$CumTime
        germ <- df$CumFraction
        max_cum_frac <- input$maxCumFrac / 100
        base_temp <- input$baseTemp
        
        # set model conditions
        start <- list(
          HT = 800,
          psib50 = -1,
          sigma = 0.4)
        
        lower <- list(
          HT = 1,
          psib50 = -5,
          sigma = 0.0001)
        
        upper <- list(
          HT = 5000,
          psib50 = 0,
          sigma = 10)
        
        if (truthy(base_temp)) {
          Tb <- base.temp
        } else {
          start$Tb <- 1
          lower$Tb <- 0
          upper$Tb <- 15
        }
        
        # try to run the model
        tryCatch({
          model <- stats::nls(
            germ ~ max_cum_frac * stats::pnorm(
              wp - (HT / ((temp - Tb) * time)),
              psib50,
              sigma,
              log = FALSE),
            start = start,
            lower = lower,
            upper = upper,
            algorithm = "port"
          )
          
          # get coefs
          corr <- stats::cor(germ, stats::predict(model)) ^ 2
          HT <- summary(model)$coefficients[[1]]
          Psib50 <- summary(model)$coefficients[[2]]
          Sigma <- summary(model)$coefficients[[3]]
          if (truthy(base_temp)) {
            Tb <- base.temp
          } else {
            Tb <- summary(model)$coefficients[[4]]
          }
          
          # return results
          list(
            HT = HT,
            Tb = Tb,
            Psib50 = Psib50,
            Sigma = Sigma,
            Correlation = corr
          )
        },
          error = function(cond) {
            paste("Unable to compute model, try adjusting parameters. ", str_to_sentence(cond[1]))
          }
        )
      })
      
      
      # Observers ----
      
      # observe(print(workingData()))
      # observe(print(modelResults()))
      
      
      # Outputs ----
      
      ## content // main UI ----
      output$content <- renderUI({
        req_cols <- colValidation$Column[colValidation$HydrothermalTime]
        validate(need(dataReady(), "Please load required data for hydrothermal time analysis. Minimum required columns are:", paste(req_cols, collapse = ", ")))
        
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
            radioButtons(
              inputId = ns("dataCleanSelect"),
              label = "Select data cleaning:",
              choices = list(
                "Original" = "original",
                "Cleaned (remove duplicates)" = "clean"
              )
            ),
            numericInput(
              inputId = ns("baseTemp"),
              label = "Base temperature:",
              value = NULL
            )
          ),
          box(
            width = 6,
            title = "Model results",
            tableOutput(ns("resultsTable"))
          ),
          box(
            width = 12,
            title = "Germination data and hydrothermal time model fit",
            plotOutput(ns("plot"))
          ),
          box(
            width = 6,
            title = "Additional treatment filters",
            uiOutput(ns("trtIdSelect")),
          ),
          box(
            width = 6,
            title = "Additional model constraints",
            sliderInput(
              inputId = ns("maxCumFrac"),
              label = "Maximum germination (%) observed",
              min = 10,
              max = 100,
              value = 100,
              step = 1
            ),
            sliderInput(
              inputId = ns("cumFracRange"),
              label = "Included interval (%):",
              min = 0,
              max = 100,
              value = c(0, 100)
            )
          )
        )
      })
      
      
      ## trtIdSelect // Create checkbox with all TrtID levels to be included in the model analysis ----
      output$trtIdSelect <- renderUI({
        if ("TrtDesc" %in% names(data())) {
          choices <- data() %>%
            mutate(Label = paste(TrtID, TrtDesc, sep = ": ")) %>%
            distinct(Label, TrtID) %>%
            mutate(Label = str_trunc(Label, 20)) %>%
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
      
      
      ## resultsTable ----
      output$resultsTable <- renderTable({
        results <- modelResults()
        
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
      
      
      ## plot ----
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
          try({
            ht <- model$HT
            psib50 <- model$Psib50
            tb <- model$Tb
            sigma <- model$Sigma
            corr <- model$Correlation
            
            # model params
            par1 <- paste("~~HT==", round(ht, 2))
            par2 <- paste("~~T[b]==", round(tb, 2))
            par3 <- paste("~~psi[b](50)==", round(psib50,3))
            par4 <- paste("~~sigma == ", round(sigma, 3))
            par5 <- paste("~~R^2 == ", round(corr, 2))
            
            # get combinations of wp and temp
            fcts <- df %>% distinct(GermWP, GermTemp)
            
            # function to plot all predicted treatments by the hydro thermal time model
            modelLines <- mapply(function(wp, temp) {
              stat_function(
                fun = function(x) {
                  stats::pnorm(
                    wp - (ht / ((temp - tb) * x)),
                    psib50,
                    sigma,
                    log = FALSE
                  ) *  germ_cutoff
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
            
            plt <- plt +
              modelLines +
              annotate("text", x = -Inf, y = 0.95, label = " Model Parameters:", color = "grey0", hjust = 0) +
              annotate("text", x = -Inf, y = 0.9, label = par1, color = "grey0", hjust = 0, parse = T) +
              annotate("text", x = -Inf, y = 0.85, label = par2, color = "grey0", hjust = 0, parse = T) +
              annotate("text", x = -Inf, y = 0.8, label = par3, color = "grey0", hjust = 0, parse = T) +
              annotate("text", x = -Inf, y = 0.75, label = par4, color = "grey0", hjust = 0, parse = T) +
              annotate("text", x = -Inf, y = 0.7, label = par5, color = "grey0", hjust = 0, parse = T)
          })
        }
        
        plt
      })
      
      
      
      
      
      
      
      
      
      
      
    } # end
  )
}


