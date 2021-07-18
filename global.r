# ---- Global ---- #

library(tidyverse)


columnDescriptions <- read_csv("data/column-types.csv", col_types = cols())
sampleGermData <- read_csv("data/sample-germ-data.csv", col_types = cols())
samplePrimingData <- read_csv("data/sample-priming-data.csv", col_types = cols())
currentData <- tibble()

