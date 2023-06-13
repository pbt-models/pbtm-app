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
  library(Cairo)
})


# Load data ----

read <- function(file) {
  read_csv(file, col_types = cols(), progress = F)
}

colValidation <- read("data/column-validation.csv")
sampleTemplate <- read("data/sample-template.csv")
sampleGermData <- read("data/sample-germ-data.csv")
samplePrimingData <- read("data/sample-priming-data.csv")
sampleAgingData <- read("data/sample-aging-data.csv")


# Local vars ----

nCols <- nrow(colValidation)
factorCols <- filter(colValidation, Role == "Factor")$Column
modelNames <- colValidation %>%
  select(Germination:Inhibitor) %>%
  names()
