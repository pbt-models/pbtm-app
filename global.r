# Population-based threshold models dashboard
# Ben Bradford, UW-Madison
# Pedro Bello, UC Davis
# Kent Bradford, UC Davis

# Dependencies ----

rm(list = ls())
suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(bslib)
  library(stats)
  library(tidyverse)
  library(DT)
  library(plotly)
  library(reactable)
})


# Development ----

options(shiny.fullstacktrace = FALSE)

if (FALSE) {
  shiny::runApp() + shiny::devmode()

  renv::init() # initiate renv if not already
  renv::dependencies() # show project dependencies
  renv::update() # update project libraries
  renv::clean() # remove unused packages
  renv::snapshot() # save updated lock file to project
  renv::restore() # restore versions from lockfile
}


# Setup ------------------------------------------------------------------------

read <- function(file) {
  read_csv(file, col_types = cols(), progress = FALSE)
}

# column validation requirements
colValidation <- read("data/column-validation.csv")

# sample data
sample_template <- read("data/template.csv")
sample_data <- list(
  germination = list(
    title = "Sample germination data",
    info = "This tomato dataset can be used to explore the germination model and characterize the time to different germination fractions under each of the treatment conditions.",
    data = read("data/PBTM Sample Germination Data.csv")
  ),
  thermal_time = list(
    title = "Thermal time data",
    info = "This tomato dataset can be used to explore the thermal time model and characterize the population behavior of the seeds under different temperature regimes.",
    data = read("data/PBTM Sample Thermal Time Data.csv")
  ),
  thermal_time_subpop = list(
    title = "Thermal time data (subpopulations)",
    info = "This dataset for thermal time analysis has two mixed subpopulations to demonstrate the use of the subpopulation solver.",
    data = read("data/PBTM Sample Thermal Time Subpopulation Data.csv")
  ),
  hydrotime = list(
    title = "Hydrotime data",
    info = "This tomato dataset can be used to explore the hydrotime model and characterize the population behavior of the seeds under different water potential regimes.",
    data = read("data/PBTM Sample Hydrotime Data.csv")
  ),
  hydrothermal_time = list(
    title = "Hydrothermal time data",
    info = "This dataset tracks tomato seed germination under three temperature regimes (15, 20, 25°C) and three water potential conditions (0, -0.25, -0.5 MPa). It may also be used for germination, thermal time, and hydrotime models when filtered.",
    data = read("data/PBTM Sample Hydrothermal Time Data.csv")
  ),
  hydropriming = list(
    title = "Hydropriming data",
    info = "This dataset shows the effect of priming seeds for different amounts of time at several different water potentials. Primed seeds may germinate faster than unprimed seeds.",
    data = read("data/PBTM Sample Hydropriming Data.csv")
  ),
  hydrothermal_priming = list(
    title = "Hydrothermal priming data",
    info = "This dataset evaluates germination speed after seeds were primed at different water potentials, temperatures, and for different priming durations.",
    data = read("data/PBTM Sample Hydrothermal Priming Data.csv")
  ),
  aging = list(
    title = "Aging data",
    info = "Seeds of different ages may exhibit different germination responses. This dataset of lettuce seeds allows exploration of the aging model.",
    data = read("data/PBTM Sample Aging Data.csv")
  ),
  promoter = list(
    title = "Promoter data",
    info = "Germination speeds may be increased by the application of specific hormones. This tomato dataset shows dose response to the application of gibberellin (GA).",
    data = read("data/PBTM Sample Promoter Data.csv")
  ),
  inhibitor = list(
    title = "Inhibitor data",
    info = "Germination may be inhibited by exposure to specific chemicals. This dataset shows dose response to the application of abscisic acid (ABA).",
    data = read("data/PBTM Sample Inhibitor Data.csv")
  )
)

# global vars
nCols <- nrow(colValidation)
factorCols <- filter(colValidation, Role == "Factor")$Column
modelNames <- colValidation |>
  select(Germination:last_col()) |>
  names()


# Helpers ----------------------------------------------------------------------

# message and print an object to the console for testing
echo <- function(x) {
  message(deparse(substitute(x)), " <", paste(class(x), collapse = ", "), ">")
  print(x)
}

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
    return(nrow(x) > 0 & ncol(x) > 0)
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

# Documentation ----------------------------------------------------------------

.docCache <- new.env(parent = emptyenv())

#' @description read and render a markdown file to cached HTML
#' @param path path to a .md file
#' @returns an HTML tag (or NULL if the file is missing)
renderMd <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    stop("renderMd: file not found: ", path)
  }

  if (is.null(.docCache[[path]])) {
    txt <- paste(
      readLines(path, warn = FALSE, encoding = "UTF-8"),
      collapse = "\n"
    )
    .docCache[[path]] <- shiny::markdown(txt)
  }
  .docCache[[path]]
}

# model docs
md_list <- c(
  germination = "md/1-germination.md",
  thermal_time = "md/2-thermal-time.md",
  hydrotime = "md/3-hydrotime.md",
  hydrothermal_time = "md/4-hydrothermal-time.md",
  hydropriming = "md/5-hydropriming-time.md",
  hydrothermal_priming = "md/6-hydrothermal-priming-time.md",
  aging = "md/7-aging-time.md",
  promoters = "md/8-promoters.md",
  inhibitors = "md/9-inhibitors.md",
  subpopulations = "md/10-subpopulations.md"
)

# this should also throw if the md doesn't exist
model_docs <- lapply(md_list, function(md) {
  renderMd(md)
})

#' Builds the 'More information' link that pops up the modal
#' @param md path to the markdown file with more information
#' @param title optional title attribute for the link
#' @returns HTML
build_modal_link <- function(md, title = "More information") {
  if (is.null(md)) {
    return()
  }
  onclick <- sprintf(
    "Shiny.setInputValue('show_modal', {md: '%s'}, { priority: 'event' });",
    md
  )
  shiny::HTML(
    sprintf(
      "<b><a style='cursor:pointer' title='%s' onclick=\"%s\">More information.</a></b>",
      title,
      onclick
    )
  )
}

#' Shows a markdown file in a modal popup
#' @param md markdown file to display
#' @param title optional modal title
show_modal <- function(md) {
  html <- renderMd(md)
  if (is.null(html)) {
    return(NULL)
  }
  m <- modalDialog(
    html,
    footer = modalButton("Close"),
    easyClose = TRUE,
    size = "xl"
  )
  showModal(m)
}


# Germination speed ------------------------------------------------------------

# Shared by the germination tab and the rate-based models (hydro / hydrothermal
# priming), which previously each inlined this near-identical pipeline.

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


#' @description adds a per-group incremental fraction (FracDiff) column
#' @param df data frame with CumTime, CumFraction
#' @param groups character vector of grouping columns
#' @returns df with FracDiff added, ungrouped
addFracDiff <- function(df, groups) {
  df %>%
    group_by(across(all_of(groups))) %>%
    arrange(across(all_of(groups)), CumTime, CumFraction) %>%
    mutate(FracDiff = CumFraction - lag(CumFraction, default = 0)) %>%
    ungroup()
}


#' @description rescales cumulative germination per group and interpolates the
#'   time to reach each target fraction (the germination-speed table)
#' @param df data frame that already has FracDiff (see addFracDiff)
#' @param groups character vector of grouping columns
#' @param fracs numeric vector of target germination percentages (0-100)
#' @returns long tibble with one row per group x target fraction: Frac, Time
interpolateGermSpeed <- function(df, groups, fracs) {
  df %>%
    mutate(MaxCumFrac = max(CumFraction), .by = all_of(groups)) %>%
    arrange(CumTime) %>%
    summarise(
      MaxCumFrac = max(MaxCumFrac),
      FracDiff = sum(FracDiff),
      .by = c(all_of(groups), CumTime)
    ) %>%
    mutate(
      CumFraction = cumsum(FracDiff) / sum(FracDiff) * MaxCumFrac,
      .by = all_of(groups)
    ) %>%
    group_by(across(all_of(groups))) %>%
    arrange(CumTime) %>%
    reframe({
      approx(
        CumFraction,
        CumTime,
        xout = fracs / 100,
        ties = "ordered",
        rule = 2
      ) %>%
        setNames(c("Frac", "Time")) %>%
        as_tibble() %>%
        drop_na()
    })
}

# Basic UI blocks --------------------------------------------------------------

#' @description creates a well panel with a title above
#' @param ... items to include in the well panel
#' @param title text to place above the well panel
namedWell <- function(..., title = NULL) {
  div(
    div(class = "well-title", title),
    div(class = "p-3 border rounded", ...)
  )
}

#' Quiet top-level card. `tools` renders right-aligned in the header (e.g. a toggle).
panelCard <- function(..., title = NULL, tools = NULL, full_screen = FALSE) {
  card(
    full_screen = full_screen,
    if (!is.null(title)) {
      card_header(
        class = "d-flex justify-content-between align-items-center",
        span(class = "panel-card-title", title),
        tools
      )
    },
    card_body(...)
  )
}

#' @description a flat labeled block for grouping controls (replaces nested
#'   grey wells inside a sidebar)
#' @param ... items to include in the section
#' @param title text to place above the section
controlSection <- function(..., title = NULL) {
  div(
    class = "control-section",
    if (!is.null(title)) div(class = "control-section-title", title),
    ...
  )
}

#' @description consistent tab title + subtitle + optional "About" modal link
#' @param title tab title
#' @param subtitle optional one-line subtitle
#' @param doc optional path to a markdown file shown via `build_modal_link()`
#' @param doc_label link text for the "About" modal link
tabHeader <- function(
  title,
  subtitle = NULL,
  doc = NULL,
  doc_label = "More information"
) {
  div(
    class = "tab-header",
    h2(class = "tab-header-title", title),
    if (!is.null(subtitle)) {
      div(
        class = "tab-header-subtitle",
        subtitle,
        if (!is.null(doc)) build_modal_link(doc, doc_label)
      )
    }
  )
}

#' @description Static/Interactive plot-mode toggle, extracted so it can live
#'   in a panelCard() header via the `tools` argument
#' @param ns module namespace function
plotModeToggle <- function(ns) {
  div(
    class = "plot-mode-toggle",
    radioButtons(
      ns("plotMode"),
      label = NULL,
      choices = c("Static" = "static", "Interactive" = "interactive"),
      selected = "static",
      inline = TRUE
    )
  )
}


# Source files ----

list.files("src", pattern = "\\.R$", recursive = TRUE, full.names = TRUE) |>
  lapply(source)
