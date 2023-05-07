# ---- Aging ---- #

# UI ----

AgingUI <- function() {
  ns <- NS("aging")
  
  tagList(
    h3(class = "tab-title", "Aging analysis"),
    div(class = "tab-info", "The aging model assumes a data set with aging (natural, controlled deterioration or accelerated aging) as a treatment condition. If you have additional treatments in your dataset, please filter out those as you may get unreliable or unexpected model results. Note: the model may fail to converge under certain max cumulative fraction values."),
    uiOutput(ns("content"))
  )
}


# Server ----

#' @references colValidation
#' 
#' @param `data` reactive data frame containing the loaded clean data
#' @param `ready` reactive boolean indicating if the model is ready

AgingServer <- function(data, ready) {
  moduleServer(
    id = "aging",
    function(input, output, session) {
      ns <- session$ns
      
      
      ## content // Rendered UI ----
      output$content <- renderUI({
        req_cols <- colValidation$Column[colValidation$Aging]
        validate(need(ready(), "Please load required data for the aging model analysis. Minimum required columns are:", paste(req_cols, collapse = ", ")))
        
        agingTimeChoices <- unique(data()$AgingTime)
        
        fluidRow(
          box(
            width = 6,
            title = "Model parameters",
            checkboxGroupInput(
              inputId = ns("agingTimeSelect"),
              label = "Included aging times:",
              choices = agingTimeChoices,
              selected = agingTimeChoices
            ),
            dataCleanSelect(ns)
          ),
          resultsTable(ns, reactive(modelResults())),
          box(
            width = 12,
            title = "Germination data and aging model fit",
            plotOutput(ns("plot"))
          ),
          trtIdSelect(ns, reactive(data())),
          cumFracSliders(ns)
        )
      })
      
      
      # workingDataset // for model and plot ----
      workingData <- reactive({
        req(
          ready(),
          input$agingTimeSelect,
          input$dataCleanSelect,
          input$trtIdSelect,
          input$cumFracRange,
          input$maxCumFrac
        )
        
        df <- data() %>%
          filter(AgingTime %in% input$agingTimeSelect) %>%
          filter(TrtID %in% input$trtIdSelect)

        # optionally remove repeated measurements at same cumulative fraction
        if (input$dataCleanSelect == "clean") {
          df <- df %>% distinct(TrtID, CumFraction, .keep_all = TRUE)
        }
        
        # filter based on cumulative fraction cutoffs
        df %>% filter(between(CumFraction * 100, input$cumFracRange[1], input$cumFracRange[2]))
      })
      
      # observe(print(workingData()))
      
      
      # modelResults // list of coefficients or string error message ----
      modelResults <- reactive({
        
        # collect data
        df <- workingData()
        aging <- df$AgingTime
        time <- df$CumTime
        germ <- df$CumFraction
        max_cum_frac <- input$maxCumFrac / 100
        
        # set model conditions
        lower <- list(thetaA = 1, pmax50 = 1, sigma = 0.1)
        start <- list(thetaA = 100, pmax50 = 10, sigma = 3)
        upper <- list(thetaA = 1000, pmax50 = 1000, sigma = 10)
        
        # run model
        tryCatch({
          model <- stats::nls(
            formula = germ ~ max_cum_frac * stats::pnorm(
              -(aging + thetaA / time),
              -pmax50,
              sigma
            ),
            start = start,
            lower = lower,
            upper = upper,
            algorithm = "port"
          )
          
          # grab coefs
          corr <- stats::cor(germ, stats::predict(model)) ^ 2
          thetaA <- summary(model)$coefficients[[1]]
          pmax50 <- summary(model)$coefficients[[2]]
          sigma <- summary(model)$coefficients[[3]]
          
          # return results
          list(
            ThetaA = thetaA,
            Pmax50 = pmax50,
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
          ggplot(aes(
            x = CumTime,
            y = CumFraction,
            color = as.factor(AgingTime))) +
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
          labs(
            x = "Time",
            y = "Cumulative fraction germinated (%)",
            color = "Aging Time") +
          guides(color = guide_legend(reverse = F, order = 1)) +
          theme_classic()
        
        # plot model results if successful
        if (is.list(model)) {
          maxCumFrac <- model$MaxCumFrac
          thetaA <- model$ThetaA
          pmax50 <- model$Pmax50
          sigma <- model$Sigma
          corr <- model$Correlation
          
          modelLines <- mapply(function(aging) {
            stat_function(
              fun = function(x) {
                stats::pnorm(
                  -(aging + thetaA / x),
                  -pmax50,
                  sigma
                ) * germ_cutoff
              },
              aes(color = as.factor(aging))
            )
          },
            unique(df$AgingTime)
          )
          
          # add model results
          par1 <- paste("~~", expression(theta~Age), "==", round(thetaA, 2))
          par2 <- paste("~~Pmax[50]==", round(pmax50, 3))
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