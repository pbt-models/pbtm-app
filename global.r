# Population-based threshold models dashboard
# Ben Bradford, UW-Madison
# Pedro Bello, UC Davis
# Kent Bradford, UC Davis

# Dependencies ----

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(bslib)
  library(stats)
  library(tidyverse)
  library(DT)
  library(plotly)
})


# Development ----

if (FALSE) {
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
sampleTemplate <- read("data/sample-template.csv")
sampleGermData <- read("data/sample-germ-data.csv")
samplePrimingData <- read("data/sample-priming-data.csv")
sampleAgingData <- read("data/sample-aging-data.csv")
samplePromoterData <- read("data/sample-promoter-data.csv")
sampleInhibitorData <- read("data/sample-inhibitor-data.csv")

# global vars
nCols <- nrow(colValidation)
factorCols <- filter(colValidation, Role == "Factor")$Column
modelNames <- colValidation |>
  select(Germination:last_col()) |>
  names()


# Helpers ----------------------------------------------------------------------

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
    div(class = "p-3 bg-light border rounded", ...)
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
