# Population-based threshold models dashboard
# Ben Bradford, UW-Madison
# Pedro Bello, UC Davis
# Kent Bradford, UC Davis

# Dependencies -----

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(bslib)
  library(stats)
  library(tidyverse)
  library(DT)
})


# Development ----

# renv::init()         # initiate renv if not already
# renv::dependencies() # show project dependencies
# renv::update()       # update project libraries
# renv::clean()        # remove unused packages
# renv::snapshot()     # save updated lock file to project
# renv::restore()      # restore versions from lockfile

# shiny::devmode(TRUE)
# shiny::devmode(FALSE)

# Load data ----

read <- function(file) {
  read_csv(file, col_types = cols(), progress = F)
}

colValidation <- read("data/column-validation.csv")
sampleTemplate <- read("data/sample-template.csv")
sampleGermData <- read("data/sample-germ-data.csv")
samplePrimingData <- read("data/sample-priming-data.csv")
sampleAgingData <- read("data/sample-aging-data.csv")
samplePromoterData <- read("data/sample-promoter-data.csv")
sampleInhibitorData <- read("data/sample-inhibitor-data.csv")


# Local vars ----

nCols <- nrow(colValidation)
factorCols <- filter(colValidation, Role == "Factor")$Column
modelNames <- colValidation |>
  select(Germination:Inhibitor) |>
  names()


# Functions ----

#' @description checks many types of objects and determines if they're truthy
#' @details Tests for real content, not logical truth. 0 is truthy (valid value).
#' @param x a value to be evaluated as `TRUE` or `FALSE`
#' @returns T/F
truthy <- function(x) {
  # non-values
  if (is.null(x) || inherits(x, "try-error")) {
    return(FALSE)
  }
  if (is.function(x)) {
    return(TRUE)
  }
  if (length(x) == 0) {
    return(FALSE)
  }

  # container types
  if (is.data.frame(x)) {
    return(nrow(x) > 0)
  }
  if (!is.atomic(x)) {
    return(TRUE)
  }

  # atomic: all-NA is false
  if (all(is.na(x))) {
    return(FALSE)
  }

  # type-specific checks
  switch(
    typeof(x),
    character = any(!is.na(x) & nzchar(x) & x != "NA"),
    logical = any(x, na.rm = TRUE),
    TRUE
  )
}

#' @description parses the comma-separated germ speed input, returns an ordered vector of numbers
#' @param x a string to parse
#' @returns a vector of parsed numbers from the string
parseSpeeds <- function(x) {
  parsed <- NULL
  suppressWarnings(
    try({
      vals <- strsplit(x, ",") |>
        unlist() |>
        parse_number() |>
        as.integer() |>
        unique() |>
        sort()
      vals <- vals[vals > 0]
      vals <- vals[vals <= 100]
      parsed <- vals
    })
  )
  return(parsed)
}


# Validation ----

#' @description checks a data column and returns whether it's valid and a message about it
#' @param col data vector to evaluate
#' @param expectedType checks that `col` is this type
#' @param minValue if `col` is numeric, checks this constraint
#' @param maxValue if `col` is numeric, checks this constraint
#' @returns boolean
validateCol <- function(col, expectedType, minValue, maxValue) {
  span_red <- \(...) span(style = "color: red;", ...)
  info <- NULL
  msg <- NULL
  valid <- FALSE

  if (anyNA(col)) {
    msg <- list(
      span_red("Warning: Missing value in data"),
      br()
    )
  }

  colType <- class(col)
  info <- list(paste("Type:", colType), br())

  # column type mismatch
  if (!is.na(expectedType) & colType != expectedType) {
    msg <- append(
      msg,
      list(
        span_red("Error: Incorrect column type, expected", expectedType),
        br()
      )
    )
  }

  # Numeric value checks
  if (colType == "numeric") {
    info <- append(
      info,
      list(
        paste0("Range: ", round(min(col), 2), " 🡒 ", round(max(col), 2)),
        br()
      )
    )

    # min check
    if (!is.na(minValue) & min(col) < minValue) {
      msg <- append(
        msg,
        list(
          span_red("Error: Min value less than", minValue),
          br()
        )
      )
    }

    # max check
    if (!is.na(maxValue) & max(col) > maxValue) {
      msg <- append(
        msg,
        list(
          span_red("Error: Max value greater than ", maxValue),
          br()
        )
      )
    }
  }

  # valid if no messages yet
  if (is.null(msg)) {
    msg <- list(span(strong("OK"), style = "color: blue;"))
    valid <- TRUE
  } else {
    msg <- head(msg, -1) # remove the last br()
  }

  list(
    valid = valid,
    ui = tagList(info, msg)
  )
}


# Helpers ----

#' @description generates a named list of unique values in a column and adds the count
#' @param df the data frame
#' @param col string specifying column name
#' @returns named list
getColChoices <- function(df, col) {
  df %>%
    count(.data[[col]]) |>
    select(n, everything()) |>
    mutate(n = sprintf("%s (n = %s)", .data[[col]], n)) |>
    deframe()
}

#' @description gets the coefficient estimates from an nls model
#' @param model the nls model
#' @returns a named list
getModelCoefs <- function(model) {
  summary(model)$coefficients |>
    as_tibble(rownames = "Param") |>
    select(1:2) |>
    deframe() |>
    round(6) |>
    as.list()
}

#' @description checks that all required columns are validated for each model
#' @param requirements boolean vector of column requirements
#' @param statuses boolean vector of current column readiness statuses
#' @returns T/F if statuses satisfy requirements
checkModelReady <- function(requirements, statuses) {
  compare <- sapply(1:nCols, function(i) {
    test <-
      (requirements[i] == TRUE & statuses[i] == TRUE) |
      (requirements[i] == FALSE)
    ifelse(length(test) == 0, FALSE, test)
  })
  all(compare)
}

#' @description sets up lists for model run in the calling environment
#' @param setParams named list of params with value or `NA`
#' @param paramRanges named list of (lower, start, upper) param constraints
#' @returns no return value, instead it directly manipulates variables in the calling environment
buildModelParams <- function(setParams, paramRanges) {
  env <- parent.frame()
  defined <- lower <- start <- upper <- list()
  for (p in names(paramRanges)) {
    paramValue <- setParams[[p]]
    if (truthy(paramValue)) {
      defined[[p]] <- paramValue
      assign(p, paramValue, envir = env)
      lower[[p]] <- paramValue
      start[[p]] <- paramValue
      upper[[p]] <- paramValue
    } else {
      if (exists(c(p))) {
        remove(list = c(p), envir = env)
      }
      ranges <- paramRanges[[p]]
      lower[[p]] <- ranges[1]
      start[[p]] <- ranges[2]
      upper[[p]] <- ranges[3]
    }
  }
  assign("defined", defined, envir = env)
  assign("lower", lower, envir = env)
  assign("start", start, envir = env)
  assign("upper", upper, envir = env)
}

#' @description builds the model results list
#' @param model the nls model object
#' @param params list of param names
#' @param defined named list of user-defined param values
#' @param CumFraction vector of cumulative fraction values to correlate the model against
#' @returns a named list of model results
buildModelResults <- function(model, params, defined, CumFraction) {
  coefs <- getModelCoefs(model)
  results <- list()
  for (p in params) {
    results[[p]] <- c(defined[[p]], coefs[[p]])[1]
  }
  results$Correlation <- cor(CumFraction, predict(model))^2
  results
}


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
    title = "Germination contraints",
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
modelResultsUI <- function(ns, results, held) {
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
            rename(
              Parameter = name,
              Value = value
            ) %>%
            mutate(
              Hold = lapply(Parameter, function(p) {
                if (p == "Correlation") {
                  ""
                } else {
                  HTML(sprintf(
                    "<input type='checkbox' %s onclick=\"Shiny.setInputValue('%s-hold', this.checked, {priority: 'event'})\">",
                    ifelse(held()[[p]], "checked", ""),
                    ns(p)
                  ))
                }
              })
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
    # ,
    # uiOutput(ns("runModel"))
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


# Plot helpers ----

#' @description plot helper
#' @param maxFrac horizontal cutoff value
#' @returns list of ggproto objects to add to ggplot
addFracToPlot <- function(maxFrac) {
  list(
    annotate(
      "rect",
      xmin = 0,
      xmax = Inf,
      ymin = maxFrac,
      ymax = 1,
      fill = "grey",
      alpha = 0.1
    ),
    geom_hline(
      yintercept = maxFrac,
      color = "darkgrey",
      linetype = "dashed"
    )
  )
}

#' @description plot helper
#' @param `gg` the ggplot object
#' @param `params` list of params to add, including math notation
#' @param `y` initial max y value to anchor the text
#' @returns updated ggplot object
addParamsToPlot <- function(gg, params, y) {
  lineheight = y / 25
  y <- y - lineheight

  gg <- gg +
    annotate(
      "text",
      x = -Inf,
      y = y,
      hjust = -0.1,
      label = "Model parameters:",
      fontface = "bold"
    )

  for (par in params) {
    y <- y - lineheight

    gg <- gg +
      annotate(
        "text",
        x = -Inf,
        y = y,
        hjust = -0.15,
        label = par,
        parse = TRUE
      )
  }

  gg
}


# Source files ----

list.files("src", "*.R", full.names = TRUE) |>
  lapply(source)
