# ---- global ---- #

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(tidyverse)
  library(snakecase)
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

defaultGermSpeedFracs <- c(10, 16, 50, 84, 90)

