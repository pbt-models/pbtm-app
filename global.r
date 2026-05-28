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


# Source files ----

# Core library files are sourced first; their functions are referenced by the
# tab modules and the model factory. Sourcing only defines functions, so order
# affects definition not execution — but keeping it explicit documents intent
# and guards against name resolution surprises. Any other src/*.R files (tab
# modules, future additions) are sourced afterward automatically.

coreSrcFiles <- c(
  "helpers.R",        # truthy, parseSpeeds, validateCol, getColChoices, checkModelReady
  "fit.R",            # model fitting core
  "ui-components.R",  # shared UI builders
  "plot-helpers.R"    # ggplot helpers
)

for (f in coreSrcFiles) source(file.path("src", f))

remainingSrcFiles <- setdiff(
  list.files("src", pattern = "\\.R$", full.names = TRUE),
  file.path("src", coreSrcFiles)
)
lapply(remainingSrcFiles, source)
