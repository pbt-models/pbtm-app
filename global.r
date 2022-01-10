# ---- Global ---- #

#library(dplyr)
library(tidyverse)

options(rsconnect.http.trace = TRUE)
options(rsconnect.http.verbose = TRUE)


columnDescriptions <- read_csv("data/column-descriptions.csv", col_types = cols())
columnValidation <- read_csv("data/column-validation.csv", col_types = cols())
sampleTemplate <- read_csv("data/sample-template.csv", col_types = cols())
sampleGermData <- read_csv("data/sample-germ-data.csv", col_types = cols())
samplePrimingData <- read_csv("data/sample-priming-data.csv", col_types = cols())

#Added because of compilation error
#columnDefaults <- read_csv("data/column-defaults.csv", col_types = cols())

# test stuff

# df <- sampleGermData
# 
# df %>%
#   group_by(Treat.ID) %>%
#   arrange(Treat.ID, CumTime, CumFraction) %>%
#   mutate(NewCumFrac = cumsum(CumFraction) / sum(CumFraction)) %>%
#   ggplot(aes(x = CumTime, y = NewCumFrac)) +
#   geom_line()
# 
# df %>%
#   arrange(CumTime, CumFraction) %>%
#   mutate(NewCumFrac = cumsum(CumFraction) / sum(CumFraction)) %>%
#   ggplot(aes(x = CumTime, y = NewCumFrac)) +
#   geom_line()
# 
# 
# df %>%
#   mutate(Germ.wp = as.factor(Germ.wp)) %>%
#   group_by(Treat.ID, Germ.wp) %>%
#   arrange(Treat.ID, CumTime, CumFraction) %>%
#   mutate(FracDiff = CumFraction - lag(CumFraction, default = 0)) %>%
#   group_by(Germ.wp, CumTime) %>%
#   summarise(FracDiff = sum(FracDiff), .groups = "drop_last") %>%
#   mutate(CumFrac = cumsum(FracDiff) / sum(FracDiff)) %>%
#   ggplot(aes(x = CumTime, y = CumFrac, color = Germ.wp)) +
#   geom_line() +
#   geom_point(shape = 19, size = 2)
