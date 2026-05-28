# ---- Shared UI components ---- #
# Reusable UI builders. Most take a `ns` (namespace) function from the calling
# model server and are invoked inside renderUI blocks.

# Basic UI blocks ----

#' @description creates a well panel with a title above
#' @param ... items to include in the well panel
#' @param title text to place above the well panel
namedWell <- function(..., title = NULL) {
  div(
    div(class = "well-title", title),
    div(class = "p-3 bg-light border rounded", ...)
  )
}

#' @description creates a dashboard box with some common options
#' @param ... items to place in the box
#' @param width grid width of the box (1-12)
#' @param title box title
primaryBox <- function(..., width = 12, title = NULL) {
  column(
    width,
    card(
      card_header(title, class = "bg-primary text-white fw-bold fst-italic"),
      card_body(...)
    )
  )
}

#' @description a shared ui component
#' @param ns namespace function from calling server
dataCleanUI <- function(ns) {
  radioButtons(
    inputId = ns("dataCleanSelect"),
    label = "Select data cleaning:",
    choices = list(
      "Original" = "original",
      "Cleaned (remove duplicates)" = "clean"
    )
  )
}

#' @description a shared ui component
#' @param ns namespace function from calling server
dataTransfUI <- function(ns) {
  radioButtons(
    inputId = ns("dataTransfSelect"),
    label = "Select dosage transformation:",
    choices = list(
      "None" = "none",
      "Logarithmic" = "log"
    )
  )
}

#' @description a shared ui component
#' @param ns namespace function from calling server
germSlidersUI <- function(ns) {
  namedWell(
    title = "Germination constraints",
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
}

#' @description a shared ui component
#' @param ns namespace function from calling server
germSpeedSliderUI <- function(ns) {
  namedWell(
    title = "Germination speed options",
    sliderInput(
      inputId = ns("germSpeedBasis"),
      label = "Germination (%) for speed calculation:",
      min = 10,
      max = 100,
      value = 50,
      step = 5
    )
  )
}

#' @description ui element showing model error if present
#' @param results object from modelResults either list or string error
#' @returns a rendered UI element or nothing
modelErrorUI <- function(results) {
  renderUI({
    if (is.list(results())) {
      return()
    }
    div(
      class = "model-error",
      sprintf(
        "Model failed under current settings: %s. Last valid model coefficients shown above.",
        results()
      )
    )
  })
}

#' @description a shared ui component
#' @param ns namespace function from calling server
#' @param results reactive model results from calling server
#' @param held reactive named list of held-param flags
#' @param paramNames character vector of fittable parameter names (these get a
#'   hold checkbox; any other row, e.g. the pseudo-R^2, does not)
modelResultsUI <- function(ns, results, held, paramNames) {
  # display labels for non-parameter result rows
  labelFor <- function(p) if (p == "PseudoR2") "Pseudo-R²" else p

  renderUI({
    namedWell(
      title = "Model results",
      renderTable(
        {
          res <- results()

          validate(need(res != "No data", "No data selected."))
          validate(
            need(
              is.list(res),
              sprintf(
                "Unable to compute model, try adjusting parameters. Reason: %s",
                res
              )
            ),
            errorClass = "model-results"
          )

          # convert list to simple data frame
          res %>%
            enframe() %>%
            unnest(value) %>%
            mutate(
              Hold = lapply(name, function(p) {
                if (p %in% paramNames) {
                  HTML(sprintf(
                    "<input type='checkbox' %s onclick=\"Shiny.setInputValue('%s-hold', this.checked, {priority: 'event'})\">",
                    ifelse(isTRUE(held()[[p]]), "checked", ""),
                    ns(p)
                  ))
                } else {
                  ""
                }
              }),
              name = vapply(name, labelFor, character(1))
            ) %>%
            rename(
              Parameter = name,
              Value = value
            )
        },
        digits = 4,
        width = "100%",
        striped = FALSE,
        hover = TRUE,
        sanitize.text.function = function(x) x,
        align = "llc"
      )
    )
  })
}

#' @description a shared ui component
#' @param ns namespace function from calling server
#' @param params vector of param names
setParamsUI <- function(ns, params) {
  namedWell(
    title = "Specify model coefficients (optional)",
    div(
      class = "flex-row",
      lapply(params, function(p) {
        numericInput(
          inputId = ns(paste0(p, "-set")),
          label = p,
          value = NA,
          step = .1,
          width = "30%"
        )
      })
    ),
    em(
      "Specify individual model coefficients, or leave blank to allow the model to find a best-fit value."
    )
  )
}


# TrtSelect module ----

#' @description a shared ui component
#' @param ns namespace function from calling server
#' @param data reactive dataset from which to pull TrtID values
trtSelectUI <- function(ns, trtCols, data) {
  trtColChoices <- list()
  for (col in trtCols) {
    trtColChoices[[col]] = sort(unique(data()[[col]]))
  }

  trtSelectServer(trtColChoices)

  renderUI({
    accordion(
      id = ns("trtFilterCollapse"),
      accordion_panel(
        title = "Additional treatment filters",
        value = "panel",
        div(
          style = "padding: 10px;",
          checkboxGroupInput(
            inputId = ns("trtFilterCols"),
            label = "Filter by:",
            choices = trtCols,
            inline = TRUE
          ),
          uiOutput(ns("trtFilters"))
        )
      ),
      open = FALSE
    )
  })
}


#' @description creates the rendered ui for treatment filters
#' @param trtChoices a named list containing the filter options
trtSelectServer <- function(trtChoices) {
  moduleServer(
    id = NULL,
    function(input, output, session) {
      ns <- session$ns

      # render the selection menus for only the trt cols picked
      output$trtFilters <- renderUI({
        collapseId <- ns("trtFilterCollapse")
        selector <- paste0("#", collapseId, " .accordion-item")
        if (is.null(input$trtFilterCols)) {
          ui <- NULL
          shinyjs::removeCssClass(selector = selector, class = "border-warning")
        } else {
          ui <- lapply(input$trtFilterCols, function(col) {
            uiOutput(ns(paste0("trtSelect-", col)))
          })
          shinyjs::addCssClass(selector = selector, class = "border-warning")
        }
        ui
      })

      # create each checkbox selection group
      lapply(names(trtChoices), function(col) {
        id <- paste0("trtSelect-", col)
        output[[id]] <- renderUI({
          checkboxGroupInput(
            inputId = ns(id),
            label = col,
            choices = trtChoices[[col]],
            selected = trtChoices[[col]],
            inline = TRUE
          )
        })
      })
    }
  )
}
