# ---- Germination analysis ---- #

# Helpers ----

parseSpeeds <- function(x) {
  parsed <- NULL
  suppressWarnings(
    try({
      vals <- strsplit(x, ",") %>%
        unlist() %>%
        parse_number() %>%
        as.integer() %>%
        unique() %>%
        sort()
      vals <- vals[vals > 0]
      vals <- vals[vals <= 100]
      parsed <- vals
    })
  )
  return(parsed)
}


# UI ----

GerminationUI <- function() {
  ns <- NS("germination")
  
  tagList(
    h3(class = "tab-title", "Germination analysis"),
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

GerminationServer <- function(data, ready, trtChoices) {
  moduleServer(
    id = "germination",
    function(input, output, session) {
      ns <- session$ns
      
      defaultGermSpeeds <- c(10, 16, 50, 84, 90)
      germSpeeds <- reactiveVal(defaultGermSpeeds)
      plotTrtChoices <- reactive({
        vals <- trtChoices()
        setNames(as.list(c(NA, vals)), c("Not specified", vals))
      })
      
      
      # Reactives ----
      
      ## dataReady ----
      dataReady <- reactive({
        truthy(data()) & ready()
      })
      
      ## workingData ----
      # slightly modified dataset used by plot and table
      workingData <- reactive({
        req(dataReady())
        
        data() %>%
          group_by(TrtID) %>%
          arrange(TrtID, CumTime, CumFraction) %>%
          mutate(FracDiff = CumFraction - lag(CumFraction, default = 0)) %>%
          ungroup()
      })
      
      ## germSpeedData ----
      # used by germination speed table
      germSpeedData <- reactive({
        workingData() %>%
          mutate(
            MaxCumFrac = max(CumFraction),
            .by = all_of(input$germSpeedTrts)) %>%
          arrange(CumTime) %>%
          summarise(
            MaxCumFrac = max(MaxCumFrac),
            FracDiff = sum(FracDiff),
            .by = c(all_of(input$germSpeedTrts), CumTime)) %>%
          mutate(CumFraction = cumsum(FracDiff) / sum(FracDiff) * MaxCumFrac, .by = all_of(input$germSpeedTrts)) %>%
          group_by(across(all_of(input$germSpeedTrts))) %>%
          arrange(CumTime) %>%
          reframe(
            {
              approx(CumFraction, CumTime, xout = germSpeeds() / 100, ties = "ordered", rule = 2) %>%
                setNames(c("Frac", "Time")) %>%
                as_tibble() %>%
                drop_na()
            }
          )
      })
      
      
      # Observers ----
      
      ## set new germ speeds ----
      observeEvent(input$setGermSpeeds, {
        parsed <- parseSpeeds(input$newGermSpeeds)
        if (length(parsed) > 0) germSpeeds(parsed)
        updateTextInput(inputId = "newGermSpeeds", value = "")
      })
      
      ## reset germ speeds ----
      observeEvent(input$resetGermSpeeds, {
        germSpeeds(defaultGermSpeeds)
        updateTextInput(inputId = "newGermSpeeds", value = "")
      })
      
      
      # Outputs ----
      
      ## content // main UI ----
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
                  selectInput(
                    inputId = ns("plotTrt1"),
                    label = "Treatment 1 (color)",
                    choices = plotTrtChoices()
                  ),
                  selectInput(
                    inputId = ns("plotTrt2"),
                    label = "Treatment 2 (shape)",
                    choices = plotTrtChoices()
                  ),
                  checkboxInput(
                    inputId = ns("mergeTrts"),
                    label = "Rescale cumulative germination?"
                  ),
                  checkboxInput(
                    inputId = ns("showSpeeds"),
                    label = "Show speed estimates?"
                  )
                ),
                mainPanel(
                  plotOutput(ns("plot"))
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
                  checkboxGroupInput(
                    inputId = ns("germSpeedTrts"),
                    label = "Select all treatment factors:",
                    choices = trtChoices(),
                    selected = c("TrtID")
                  ),
                  textInput(
                    inputId = ns("newGermSpeeds"),
                    label = "Set cumulative percent (separate with commas):",
                    value = ""
                  ),
                  div(
                    style = "margin-top: 1em; margin-bottom: 1em;",
                    actionButton(ns("setGermSpeeds"), "Apply"),
                    actionButton(ns("resetGermSpeeds"), "Reset")
                  ),
                  radioButtons(
                    inputId = ns("germSpeedType"),
                    label = "Report values as:",
                    choices = list(
                      "Time (to % germinated)" = "Time",
                      "Rate (1 / time)" = "Rate"
                    )
                  )
                ),
                mainPanel(
                  div(
                    style = "overflow-x: auto",
                    dataTableOutput(ns("germSpeedTable"))
                  )
                )
              )
            )
          )
        )
      })
      

      ## plot // germination curves ----
      output$plot <- renderPlot({
        df <- workingData()
        trt1 <- input$plotTrt1
        trt2 <- input$plotTrt2
        req(trt1, trt2)
        
        # collect treatments
        trts <- c()
        if (trt1 != "NA") {
          trts <- c(trt1)
          if (trt2 != "NA") {
            trts <- c(trt1, trt2)
          }
        }
        
        # rescales cumulative fraction across retained treatments
        if (input$mergeTrts) {
          df <- df %>%
            mutate(
              MaxCumFrac = max(CumFraction),
              .by = all_of(trts)) %>%
            arrange(CumTime) %>%
            summarise(
              MaxCumFrac = max(MaxCumFrac),
              FracDiff = sum(FracDiff),
              .by = c(all_of(trts), CumTime)) %>%
            mutate(CumFraction = cumsum(FracDiff) / sum(FracDiff) * MaxCumFrac, .by = all_of(trts))
        }
        
        # plots by number of trts
        if (length(trts) == 1) {
          plt <- df %>%
            ggplot(aes(
              x = CumTime,
              y = CumFraction,
              color = as.factor(.data[[trt1]]))) +
            geom_point(shape = 19, size = 2.5) +
            labs(color = trt1)
        } else if (length(trts) == 2) {
          plt <- df %>%
            ggplot(aes(
              x = CumTime,
              y = CumFraction,
              color = as.factor(.data[[trt1]]),
              shape = as.factor(.data[[trt2]]))) +
            geom_point(size = 2.5) +
            labs(
              color = trt1,
              shape = trt2)
        } else {
          plt <- df %>%
            ggplot(aes(x = CumTime, y = CumFraction)) +
            geom_point(size = 2)
        }
        
        # use TrtID to group lines if not rescaled
        if (input$mergeTrts) {
          plt <- plt + geom_line()
        } else {
          plt <- plt + geom_line(aes(group = TrtID))
        }
        
        # set theme etc
        plt <- plt +
          scale_x_continuous(
            breaks = scales::pretty_breaks(),
            expand = expansion(c(.1, .1))) +
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, 1),
            expand = expansion(c(0, .1))) +
          labs(
            title = "Cumulative germination",
            x = "Time",
            y = "Cumulative (%)"
          ) +
          theme_classic()
        
        # show speed fractions on plot
        lines = germSpeeds() / 100
        plt <- plt + geom_hline(
          yintercept = lines,
          color = "grey",
          linewidth = 0.35,
          linetype = "dashed")
        
        # optionally, show time estimates for selected speeds
        if (input$showSpeeds) {
          plt <- plt +
            geom_linerange(
              data = germSpeedData(),
              aes(x = Time, ymax = Frac),
              inherit.aes = F,
              ymin = 0,
              color = "red",
              linewidth = 0.25
            ) +
            geom_point(
              data = germSpeedData(),
              aes(x = Time, y = Frac),
              inherit.aes = F,
              color = "red",
              size = 2
            )
        }
        
        plt
      })
      
      ## germSpeedTable ----
      output$germSpeedTable <- renderDataTable({
        req(input$germSpeedType)

        # show as rate or cumulative fraction
        if (input$germSpeedType == "Rate") {
          germSpeedData() %>%
            mutate(
              Time = round(1 / Time, 6),
              Frac = paste0("GR", Frac * 100)) %>%
            pivot_wider(
              names_from = "Frac",
              values_from = "Time"
            )
        } else {
          germSpeedData() %>%
            mutate(
              Frac = paste0("T", Frac * 100),
              Time = round(Time, 1)) %>%
            pivot_wider(
              names_from = "Frac",
              values_from = "Time"
            )
        }
      },
        rownames = F,
        server = F,
        extensions = c("Buttons", "Select"),
        selection = "none",
        options = list(
          searching = F,
          paging = F,
          select = T,
          dom = "Bfrtip",
          buttons = list(
            list(
              extend = "copy",
              text = 'Copy table to clipboard'
            )
          )
        )
      )
      
  })
}
