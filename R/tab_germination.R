# ---- Germination analysis ---- #

GerminationUI <- function() {
  ns <- NS("germination")
  
  tagList(
    h3(class = "tab-title", "Germination analysis"),
    uiOutput(ns("content"))
  )
}


# Server ----
#' requires global vars:
#' - colValidation
#' - nCols

GerminationServer <- function(data, ready, trtChoices) {
  moduleServer(
    id = "germination",
    function(input, output, session) {
      ns <- session$ns
      
      defaultGermSpeedFracs <- c(10, 16, 50, 84, 90)
      
      
      # Reactives ----
      
      ## dataReady ----
      dataReady <- reactive({
        print(data())
        truthy(data()) & ready()
      })
      
      
      # Main UI ----
      
      output$content <- renderUI({
        validate(
          need(dataReady(), paste(
            "Please load a dataset and set required column types for germination analysis. Minimum required columns for germination analysis are",
            paste(colValidation$Column[colValidation$Germination], collapse = ", ")
            )
          )
        )
        
        tagList(
          fluidRow(
            box(
              title = "Cumulative germination plot",
              status = "primary",
              solidHeader = T,
              width = 12,
              sidebarLayout(
                sidebarPanel(
                  uiOutput(ns("plotTrt1")),
                  uiOutput(ns("plotTrt2"))
                ),
                mainPanel(
                  plotOutput("plot")
                )
              )
            ),
            box(
              title = "Germination time analysis",
              status = "primary",
              solidHeader = T,
              width = 12,
              sidebarLayout(
                sidebarPanel(
                  uiOutput("germSpeedTrtsUI"),
                  uiOutput("germSpeedFracsUI"),
                  uiOutput("germSpeedTypeUI")
                ),
                mainPanel(
                  div(
                    style = "overflow-x: auto",
                    dataTableOutput("germSpeedTable")
                  )
                )
              )
            )
          )
        )
      })
      
      
      # Plot ----
      
      ## Trt 1 selection ----
      
      output$plotTrt1 <- renderUI({
        selectInput(
          inputId = ns("plotTrt1"),
          label = "Treatment 1 (color)",
          choices = trtChoices()
        )
      })
      
      
      ## Trt 2 selection ----
      
      output$plotTrt2 <- renderUI({
        req(input$plotTrt1)
        req(input$plotTrt1 != "NA")
        
        selectInput(
          inputId = ns("plotTrt2"),
          label = "Treatment 2 (shape)",
          choices = trtChoices() - input$plotTrt1
        )
      })
      
      
      ## Plot ----
      
      output$plot <- renderPlot({
        req(dataReady())
        
        trts <- 0
        df <- data() %>%
          group_by(TrtID) %>%
          arrange(TrtID, CumTime, CumFrac)
        
        if (req(input$plotTrt1) != "NA") {
          df <- mutate(df, Trt1 = as.factor(df[[input$plotTrt1]]))
          trts <- 1
          
          if (req(input$plotTrt2) != "NA") {
            df <- mutate(df, Trt2 = as.factor(df[[input$plotTrt2]]))
            trts <- 2
          }
        }
        
        if (trts == 1) {
          plt <- df %>%
            ggplot(aes(x = CumTime, y = CumFrac, group = TrtID, color = Trt1)) +
            geom_line() +
            geom_point(shape = 19, size = 2) +
            labs(color = input$plotTrt1)
        } else if (trts == 2) {
          plt <- df %>%
            ggplot(aes(x = CumTime, y = CumFrac, group = TrtID, color = Trt1, shape = Trt2)) +
            geom_line() +
            geom_point(size = 2) +
            labs(color = input$plotTrt1, shape = input$plotTrt2)
        } else {
          plt <- df %>%
            ggplot(aes(x = CumTime, y = CumFrac, group = TrtID)) +
            geom_line() +
            geom_point(size = 2)
        } 
        
        plt <- plt +
          scale_x_continuous(breaks = scales::pretty_breaks()) +
          scale_y_continuous(labels = scales::percent) +
          labs(
            title = "Cumulative germination",
            x = "Time",
            y = "Cumulative (%)"
          ) +
          theme_classic()
        
        lines = rv$germSpeedFracs / 100
        plt + geom_hline(yintercept = lines, color = "grey", size = 0.25, alpha = 0.5, linetype = "dashed")
      })
      
      # #### germSpeedTrtChoices ####
      # germSpeedTrtChoices <- reactive({
      #   cols <- sapply(1:nCols, function(i) {
      #     if (rv$colStatus[i] == T && colValidation$Role[i] == "Factor") colValidation$Column[i]
      #   })
      #   cols <- compact(cols)
      # })
      # 
      # #### germSpeedTrts ####
      # output$germSpeedTrtsUI <- renderUI({
      #   checkboxGroupInput(
      #     inputId = "germSpeedTrtSelect",
      #     label = "Select all treatment factors:",
      #     choices = germSpeedTrtChoices(),
      #     selected = c("TrtID")
      #   )
      # })
      # 
      # #### germSpeedFracsUI ####
      # output$germSpeedFracsUI <- renderUI({
      #   list(
      #     textInput(
      #       inputId = "addGermSpeedFracs",
      #       label = "Set cumulative percent (separate with commas):",
      #       value = ""
      #     ),
      #     div(
      #       style = "margin-top: 1em; margin-bottom: 1em;",
      #       actionButton("setGermSpeedFracs", "Apply"),
      #       actionButton("resetGermSpeedFracs", "Reset")
      #     )
      #   )
      # })
      # 
      # # handle apply button
      # observeEvent(input$setGermSpeedFracs, {
      #   try({
      #     fracs <- suppressWarnings(sort(parse_number(unlist(strsplit(input$addGermSpeedFracs, ",")))))
      #     fracs <- unique(as.integer(fracs))
      #     fracs <- fracs[fracs > 0]
      #     fracs <- fracs[fracs <= 100]
      #     if (length(fracs) > 0) {rv$germSpeedFracs <- fracs}
      #   })
      #   updateTextInput(inputId = "addGermSpeedFracs", value = "")
      # })
      # 
      # # handle reset button
      # observeEvent(input$resetGermSpeedFracs, {
      #   updateTextInput(inputId = "addGermSpeedFracs", value = "")
      #   rv$germSpeedFracs = defaultGermSpeedFracs
      # })
      # 
      # #### germSpeedTypeUI ####
      # output$germSpeedTypeUI <- renderUI({
      #   radioButtons(
      #     inputId = "germSpeedType",
      #     label = "Report values as:",
      #     choiceNames = c("Time (to % germinated)", "Rate (1 / time)"),
      #     choiceValues = c("Time", "Rate")
      #   )
      # })
      # 
      # #### germSpeedTable ####
      # output$germSpeedTable <- renderDataTable({
      #   req(dataLoaded())
      #   req(input$germSpeedType)
      #   
      #   # construct working dataset
      #   df <- tibble(TrtID = rv$data[[input$TrtID]])
      #   trts <- input$germSpeedTrtSelect
      #   for (trt in trts) { df[[trt]] <- rv$data[[input[[trt]]]] }
      #   df <- df %>% mutate(
      #     CumTime = rv$data[[input$CumTime]],
      #     CumFrac = rv$data[[input$CumFraction]]
      #   )
      #   
      #   # regenerate cumulative fractions depending on grouping trts
      #   df <- df %>%
      #     group_by(TrtID) %>%
      #     arrange(TrtID, CumTime, CumFrac) #%>%
      #   #mutate(FracDiff = CumFrac - lag(CumFrac, default = 0))
      #   
      #   # group by the selected treatments AND TrtID to calculate speed individually
      #   df <- group_by_at(df, vars(TrtID,trts))
      #   
      #   # merge values that occur at the same timepoint
      #   #df <- df %>%
      #   #group_by(CumTime, .add = T) #%>%
      #   #summarise(FracDiff = sum(FracDiff), .groups = "drop_last") %>%
      #   #mutate(CumFrac = cumsum(FracDiff) / sum(FracDiff))
      #   
      #   # interpolate the curves to get the estimated value at given fraction
      #   df <- df %>%
      #     arrange(CumTime) %>%
      #     summarise(
      #       {
      #         approx(CumFrac, CumTime, xout = rv$germSpeedFracs / 100, ties = "ordered", rule = 2) %>%
      #           setNames(c("Frac", "Time")) %>%
      #           as_tibble() %>%
      #           drop_na()
      #       },
      #       .groups = "drop"
      #     )
      #   
      #   # group by the selected treatments for next step
      #   df <- group_by_at(df, vars(trts, Frac))
      #   
      #   # Calculate average speed and standard deviations (Future) for all speed fractions
      #   df <- df %>%
      #     summarise(
      #       Time = mean(Time), # Time_sd = sd(Time), ADD SD in the future
      #       .groups = "drop"
      #     )
      #   
      #   if (input$germSpeedType == "Rate") {
      #     # show as rate
      #     df <- df %>%
      #       mutate(
      #         Time = round(1 / Time, 6),
      #         Frac = paste0("GR", Frac * 100)) %>%
      #       pivot_wider(
      #         names_from = "Frac",
      #         values_from = "Time"
      #       )
      #   } else {
      #     # show as cumulative fraction
      #     df <- df %>%
      #       mutate(
      #         Frac = paste0("T", Frac * 100),
      #         Time = round(Time, 1)) %>%
      #       pivot_wider(
      #         names_from = "Frac",
      #         values_from = "Time"
      #       )
      #   }
      #   df
      # },
      #   rownames = F,
      #   server = F,
      #   extensions = c("Buttons", "Select"),
      #   selection = "none",
      #   options = list(
      #     searching = F,
      #     paging = F,
      #     select = T,
      #     dom = "Bfrtip",
      #     buttons = list(
      #       list(
      #         extend = "copy",
      #         text = 'Copy table to clipboard'
      #       )
      #     )
      #   )
      # )
      
      
      
      
      
      
      
      
      
      
      
  })
}
