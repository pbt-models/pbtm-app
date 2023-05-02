# ---- global ---- #

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(shinyBS)
  library(stats)
  library(tidyverse)
  library(snakecase)
  library(DT)
})


# Helper functions ----

truthy <- function(x) {
  if (is.null(x)) return(F)
  if (inherits(x, "try-error")) return(F)
  if (is.function(x)) return(T)
  if (is.data.frame(x)) return(nrow(x) > 0)
  if (length(x) == 0) return(F)
  if (all(is.na(x))) return(F)
  if (is.character(x) && !any(nzchar(stats::na.omit(x)))) return(F)
  if (is.logical(x) && !any(stats::na.omit(x))) return(F)
  if (!is.atomic(x)) return(TRUE)
  if (length(x) > 1) return(T)
  if (x %in% c(NA, "NA", "", 0, F)) return(F)
  T
}

# returns false if colStatus is false for that column
checkModelReadiness <- function(requirements, statuses) {
  compare <- sapply(1:nCols, function(i) {
    test <- (requirements[i] == T & statuses[i] == T) | (requirements[i] == F)
    if (length(test) == 0) {F} else {test}
  })
  !(F %in% compare)
}

# Load data ----

colValidation <- read_csv("data/column-validation.csv", col_types = cols(), progress = F)
sampleTemplate <- read_csv("data/sample-template.csv", col_types = cols(), progress = F)
sampleGermData <- read_csv("data/sample-germ-data.csv", col_types = cols(), progress = F)
samplePrimingData <- read_csv("data/sample-priming-data.csv", col_types = cols(), progress = F)
sampleAgingData <- read_csv("data/sample-aging-data.csv", col_types = cols(), progress = F)


# Local vars ----

nCols <- nrow(colValidation)
factorCols <- filter(colValidation, Role == "Factor")$Column
modelNames <- colValidation %>%
  select(Germination:Inhibitor) %>%
  names()
