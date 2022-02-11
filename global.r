# ---- global ---- #

library(tidyverse)

# Load data
columnValidation <- read_csv("data/column-validation.csv", col_types = cols())
sampleTemplate <- read_csv("data/sample-template.csv", col_types = cols())
sampleGermData <- read_csv("data/sample-germ-data.csv", col_types = cols())
samplePrimingData <- read_csv("data/sample-priming-data.csv", col_types = cols())

# local vars
nCols <- nrow(columnValidation)
factorCols <- filter(columnValidation, Role == "Factor")$Column
modelNames <- names(columnValidation)[12:length(names(columnValidation))] # confirm correct starting column for model names
nModels <- length(modelNames)
