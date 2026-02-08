# Population-based threshold models dashboard
# Ben Bradford, UW-Madison
# Pedro Bello, UC Davis
# Kent Bradford, UC Davis

# Dependencies -----

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(shinyBS)
  library(stats)
  library(tidyverse)
  # library(snakecase)
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
modelNames <- colValidation %>%
  select(Germination:Inhibitor) %>%
  names()


# Source files ----

list.files("src", "*.R", full.names = TRUE) |>
  lapply(source)
