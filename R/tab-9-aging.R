# ---- Aging ---- #

# UI ----

agingUI <- function() {
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
#' @param `data` a `reactive()` data frame containing the loaded clean data
#' @param `ready` a `reactive()` boolean indicating if the model is ready

agingServer <- function(data, ready) {
  moduleServer(
    id = "aging",
    function(input, output, session) {
      ns <- session$ns
      
      
      # Reactives ----
      
      ## dataReady ----
      dataReady <- reactive({
        truthy(data()) & ready()
      })
      
      ## workingDataset // for model and plot ----
      workingData <- reactive({
        req(
          dataReady(),
          input$dataCleanSelect,
          input$trtIdSelect,
          input$cumFracRange,
          input$maxCumFrac
        )
        
        df <- data() %>%
          filter(TrtID %in% input$trtIdSelect)

        # optionally remove repeated measurements at same cumulative fraction
        if (input$dataCleanSelect == "clean") {
          df <- df %>% distinct(TrtID, CumFraction, .keep_all = TRUE)
        }
        
        # filter based on cumulative fraction cutoffs
        df %>% filter(between(CumFraction * 100, input$cumFracRange[1], input$cumFracRange[2]))
      })
      
      
      ## modelResults // list or error message ----
      modelResults <- reactive({
        df <- workingData()
        Atime <- df$AgingTime
        time <- df$CumTime
        germ <- df$CumFraction
        max_cum_frac <- input$maxCumFrac / 100
        
        # set model conditions
        lower <- list(ThetaA = 1   , Pmax50 = 1   , Sigma = 0.1)
        start <- list(ThetaA = 100 , Pmax50 = 10  , Sigma = 3  )
        upper <- list(ThetaA = 1000, Pmax50 = 1000, Sigma = 10 )
        
        # run model
        tryCatch({
          model <- stats::nls(
            formula = germ ~ max_cum_frac * stats::pnorm(
              -(Atime + ThetaA / time), #wp - (HT / time),
              -Pmax50, #Psib50,
              Sigma,
              log = FALSE),
            start = start,
            lower = lower,
            upper = upper,
            algorithm = "port"
          )
          
          # grab coefs
          corr <- stats::cor(germ, stats::predict(model)) ^ 2
          ThetaA <- summary(model)$coefficients[[1]]
          Pmax50 <- summary(model)$coefficients[[2]]
          Sigma <- summary(model)$coefficients[[3]]
          
          # return results
          list(
            ThetaA = ThetaA,
            Pmax50 = Pmax50,
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
        req_cols <- colValidation$Column[colValidation$Aging]
        validate(need(dataReady(), "Please load required data for the aging model analysis. Minimum required columns are:", paste(req_cols, collapse = ", ")))
        
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
            radioButtons(
              inputId = ns("dataCleanSelect"),
              label = "Select data cleaning:",
              choices = list(
                "Original" = "original",
                "Cleaned (remove repeats)" = "clean"
              )
            )
          ),
          box(
            width = 6,
            title = "Model results",
            tableOutput(ns("resultsTable"))
          ),
          box(
            width = 12,
            title = "Germination data and aging model fit",
            plotOutput(ns("plot"))
          ),
          box(
            width = 6,
            title = "Additional treatment filters",
            uiOutput(ns("trtIdSelect"))
          ),
          box(
            width = 6,
            title = "Additional model contraints",
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
            mutate(Label = str_trunc(Label, 30)) %>%
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
          
          modelLines <- mapply(function(atime) {
            stat_function(
              fun = function(x) {
                stats::pnorm(-(atime + thetaA / x), -pmax50, sigma, log = FALSE) * germ_cutoff
              },
              aes(color = as.factor(atime))
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