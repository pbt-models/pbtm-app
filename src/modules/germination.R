# ---- Germination analysis ---- #

# Static UI ----

GerminationUI <- function() {
  ns <- NS("germination")

  layout_columns(
    col_widths = 12,
    div(
      model_docs$germination
    ),
    uiOutput(ns("content"))
  )
}


# Server ----

#' @references colValidation
#' @references nCols
#'
#' @param `data` a `reactive()` data frame containing the loaded clean data
#' @param `ready` a `reactive()` boolean indicating if the model is ready

# `id` is the first formal (defaulting to "germination") so shiny::testServer
# can drive this module; production calls it by name (data = , ready = ).
GerminationServer <- function(id = "germination", data, ready) {
  moduleServer(
    id = id,
    function(input, output, session) {
      ns <- session$ns

      # Vars ----

      ## defaultGermSpeeds ----
      defaultGermSpeeds <- c(10, 16, 50, 84, 90)

      ## rv $ germSpeeds ----
      rv <- reactiveValues(
        germSpeeds = defaultGermSpeeds
      )

      # Reactives ----

      ## trtChoices ----
      # all column names except CumTime and CumFraction
      trtChoices <- reactive({
        req(ready())

        x <- names(data())
        x[!x %in% c("CumTime", "CumFraction")]
      })

      ## uniqueValueCount ----
      # number of unique values for each factor column
      uniqueValueCount <- reactive({
        req(ready())

        data() %>%
          select(all_of(trtChoices())) %>%
          lapply(function(x) {
            length(unique(x))
          }) %>%
          unlist() %>%
          enframe() %>%
          mutate(label = sprintf("%s (n=%s)", name, value))
      })

      ## plotColorChoices ----
      plotColorChoices <- reactive({
        df <- uniqueValueCount()
        as.list(c(NA, df$name)) %>%
          setNames(c("Not specified", df$label))
      })

      ## plotShapeChoices ----
      plotShapeChoices <- reactive({
        df <- uniqueValueCount() %>%
          filter(value <= 6)
        as.list(c(NA, df$name)) %>%
          setNames(c("Not specified", df$label))
      })

      ## workingData ----
      # slightly modified dataset used by plot and table
      workingData <- reactive({
        req(ready())

        data() %>%
          group_by(TrtID) %>%
          arrange(TrtID, CumTime, CumFraction) %>%
          mutate(FracDiff = CumFraction - lag(CumFraction, default = 0)) %>%
          ungroup()
      })

      ## germSpeedData ----
      # used by germination speed table
      germSpeedData <- reactive({
        req(ready())

        workingData() %>%
          mutate(
            MaxCumFrac = max(CumFraction),
            .by = all_of(input$germSpeedTrts)
          ) %>%
          arrange(CumTime) %>%
          summarise(
            MaxCumFrac = max(MaxCumFrac),
            FracDiff = sum(FracDiff),
            .by = c(all_of(input$germSpeedTrts), CumTime)
          ) %>%
          mutate(
            CumFraction = cumsum(FracDiff) / sum(FracDiff) * MaxCumFrac,
            .by = all_of(input$germSpeedTrts)
          ) %>%
          group_by(across(all_of(input$germSpeedTrts))) %>%
          arrange(CumTime) %>%
          reframe(
            {
              approx(
                CumFraction,
                CumTime,
                xout = rv$germSpeeds / 100,
                ties = "ordered",
                rule = 2
              ) %>%
                setNames(c("Frac", "Time")) %>%
                as_tibble() %>%
                drop_na()
            }
          )
      })

      # Event Reactives ----

      ## set new germ speeds ----
      observeEvent(input$setGermSpeeds, {
        parsed <- parseSpeeds(input$newGermSpeeds)
        if (length(parsed) > 0) {
          rv$germSpeeds <- parsed
        }
        updateTextInput(inputId = "newGermSpeeds", value = "")
      })

      ## reset germ speeds ----
      observeEvent(input$resetGermSpeeds, {
        rv$germSpeeds <- defaultGermSpeeds
        updateTextInput(inputId = "newGermSpeeds", value = "")
      })

      # Outputs ----

      # content // Rendered UI ----
      output$content <- renderUI({
        req_cols <- paste(
          colValidation$Column[colValidation$Germination],
          collapse = ", "
        )
        validate(need(
          ready(),
          paste(
            "Please load a dataset and set required column types for germination analysis. Minimum required columns for germination analysis are",
            req_cols
          )
        ))

        layout_sidebar(
          sidebar = sidebar(
            title = "Plot controls",
            width = 360,
            open = "open",
            accordion(
              open = c("Germination plot", "Germination speed"),
              accordion_panel(
                "Germination plot",
                icon = icon("chart-line"),
                controlSection(
                  title = "Treatment selection",
                  selectInput(
                    inputId = ns("plotColor"),
                    label = "Treatment 1 (color)",
                    choices = plotColorChoices()
                  ),
                  selectInput(
                    inputId = ns("plotShape"),
                    label = "Treatment 2 (shape, n <= 6)",
                    choices = plotShapeChoices()
                  )
                ),
                controlSection(
                  title = "Plot options",
                  checkboxInput(
                    inputId = ns("mergeTrts"),
                    label = "Rescale cumulative germination?"
                  ),
                  checkboxInput(
                    inputId = ns("showSpeeds"),
                    label = "Show speed estimates?"
                  )
                )
              ),
              accordion_panel(
                "Germination speed",
                icon = icon("gauge-high"),
                controlSection(
                  title = "Speed options",
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
                    class = "flex-btns",
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
                )
              )
            )
          ),
          panelCard(
            title = "Cumulative germination plot",
            tools = plotModeToggle(ns),
            full_screen = TRUE,
            uiOutput(ns("plotArea"))
          ),
          panelCard(
            title = "Germination speed",
            div(
              style = "overflow-x: auto",
              dataTableOutput(ns("germSpeedTable"))
            )
          )
        )
      })

      ## plot // germination curves ----
      germPlot <- reactive({
        {
          req(ready())

          df <- workingData()
          colorTrt <- input$plotColor
          shapeTrt <- input$plotShape
          req(colorTrt, shapeTrt)

          # collect treatments
          trts <- c(colorTrt, shapeTrt)
          trts <- trts[!trts == "NA"]

          # rescales cumulative fraction across retained treatments
          if (input$mergeTrts) {
            df <- df %>%
              mutate(
                MaxCumFrac = max(CumFraction),
                .by = any_of(trts)
              ) %>%
              arrange(CumTime) %>%
              summarise(
                MaxCumFrac = max(MaxCumFrac),
                FracDiff = sum(FracDiff),
                .by = c(all_of(trts), CumTime)
              ) %>%
              mutate(
                CumFraction = cumsum(FracDiff) / sum(FracDiff) * MaxCumFrac,
                .by = any_of(trts)
              )
          }

          # no color, no shape
          if (colorTrt == "NA" & shapeTrt == "NA") {
            plt <- df %>%
              ggplot(aes(
                x = CumTime,
                y = CumFraction
              )) +
              geom_point(shape = 19, size = 2.5)

            # color only
          } else if (colorTrt != "NA" & shapeTrt == "NA") {
            plt <- df %>%
              ggplot(aes(
                x = CumTime,
                y = CumFraction,
                color = as.factor(.data[[colorTrt]])
              )) +
              geom_point(shape = 19, size = 2.5) +
              labs(color = colorTrt)

            # shape only
          } else if (colorTrt == "NA" & shapeTrt != "NA") {
            plt <- df %>%
              ggplot(aes(
                x = CumTime,
                y = CumFraction,
                shape = as.factor(.data[[shapeTrt]])
              )) +
              geom_point(size = 2.5) +
              labs(shape = shapeTrt)

            # color and shape
          } else {
            plt <- df %>%
              ggplot(aes(
                x = CumTime,
                y = CumFraction,
                color = as.factor(.data[[colorTrt]]),
                shape = as.factor(.data[[shapeTrt]])
              )) +
              geom_point(size = 2.5) +
              labs(color = colorTrt, shape = shapeTrt)
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
              expand = expansion(c(0.1, 0.1))
            ) +
            scale_y_continuous(
              labels = scales::percent,
              limits = c(0, 1),
              expand = expansion(c(0, 0.05))
            ) +
            labs(
              title = "Cumulative germination",
              x = "Time",
              y = "Cumulative (%)"
            ) +
            theme_classic() +
            theme(plot.title = element_text(face = "bold", size = 14))

          # show speed fractions on plot
          lines <- rv$germSpeeds / 100
          plt <- plt +
            geom_hline(
              yintercept = lines,
              color = "grey",
              linewidth = 0.35,
              linetype = "dashed"
            )

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
        }
      })

      output$plotArea <- renderUI({
        if (identical(input$plotMode, "interactive")) {
          plotlyOutput(ns("plotly"), height = "600px")
        } else {
          plotOutput(ns("plot"), height = "560px")
        }
      })

      output$plot <- renderPlot(germPlot(), res = 100)

      output$plotly <- renderPlotly(ggplotly(germPlot()))

      ## germSpeedTable ----
      output$germSpeedTable <- renderDataTable(
        {
          req(ready())
          req(input$germSpeedType)

          # show as rate or cumulative fraction
          if (input$germSpeedType == "Rate") {
            germSpeedData() %>%
              mutate(
                Time = round(1 / Time, 6),
                Frac = paste0("GR", Frac * 100)
              ) %>%
              pivot_wider(
                names_from = "Frac",
                values_from = "Time"
              )
          } else {
            germSpeedData() %>%
              mutate(
                Frac = paste0("T", Frac * 100),
                Time = round(Time, 1)
              ) %>%
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
              text = "Copy table to clipboard"
            ),
            list(
              extend = "csv",
              text = "Download as csv"
            )
          )
        )
      )
    } # end
  )
}
