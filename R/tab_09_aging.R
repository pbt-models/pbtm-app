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
        validate(need(ready(), paste("Please load required data for the aging model analysis. Minimum required columns are:", paste(req_cols, collapse = ", "))))
        
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
        req(nrow(data()) > 0)
        req(ready())
        req(
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
        req(ready())
        
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
          error = function(cond) { paste(cond[1]) }
        )
      })
      
      # observe(print(modelResults()))
      
      
      ## plot ----
      output$plot <- renderPlot({
        req(ready())
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
          
          plt <- plt + modelLines
          
          # add model annotation
          plt <- addParamsToPlot(plt, list(
            paste("~~", expression(theta~Age), "==", round(thetaA, 2)),
            sprintf("~~Pmax[50]==%.3f", pmax50),
            sprintf("~~sigma==%.3f", sigma),
            sprintf("~~R^2==%.2f", corr)
          ))
        }
        
        plt
      })
      
    } # end
  )
}