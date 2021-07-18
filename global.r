# ---- Global ---- #

library(tidyverse)


columnDescriptions <- read_csv("data/column-descriptions.csv", col_types = cols())
columnDefaults <- read_csv("data/column-defaults.csv", col_types = cols())
sampleTemplate <- read_csv("data/sample-template.csv", col_types = cols())
sampleGermData <- read_csv("data/sample-germ-data.csv", col_types = cols())
samplePrimingData <- read_csv("data/sample-priming-data.csv", col_types = cols())

