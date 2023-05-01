# ---- global ---- #

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(tidyverse)
  library(DT)
})


# Helper functions ----

truthy <- function(val) {
  if (is.null(val)) return(F)
  if (length(val) == 0) return(F)
  if (length(val) > 1) return(T)
  if (val %in% c(NA, "NA", F, 0)) return(F)
  T
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
modelNames <- names(colValidation)[12:length(names(colValidation))]

defaultGermSpeedFracs <- c(10, 16, 50, 84, 90)

