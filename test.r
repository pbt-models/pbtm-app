install.packages("Cairo")

library(tidyverse)
library(shiny)
library(shinyjs)
library(DT)
library(Cairo)

#Set working folder - Add the folder location of your data files
setwd("~/Dropbox/pbtm-app-main-3/data")

#Load Seeds data - Add the name of the data file (csv file)
mydata <- read.csv("sample-germ-data.csv", header=T)

df <- mydata

df <- df %>%
  group_by(TrtID) %>%
  arrange(TrtID, CumTime, CumFraction) #%>%
  #mutate(FracDiff = CumFraction - lag(CumFraction, default = 0)) #calc of actual fraction non-cumulative


df <- df %>%
group_by(TrtID, GermTemp, CumTime) #%>%
 #summarise(FracDiff1 = sum(FracDiff), .groups = "drop_last") %>%
 #mutate(CumFrac2 = cumsum(FracDiff1) / 1)

df <- group_by_at(df, vars(TrtID, GermTemp))

df <- group_by_at(df, vars())

df <- df %>%
      mutate(Trt2 = as.factor(df$GermTemp))

dft <- tibble(
        TrtID = as.factor(df$TrtID),
        CumTime = df$CumTime,
        CumFrac = df$CumFraction
        )

#Germination Analysis Calculation Tests  -----------------------

grmSpeedFracs <- c(50, 90)

df <- df %>%
  arrange(CumTime) %>%
  summarise(
    {
      approx(CumFraction, CumTime, xout = grmSpeedFracs / 100, ties = "ordered", rule = 2) %>%
        setNames(c("Frac", "Time")) %>%
        as_tibble() %>%
        drop_na()
    },
    .groups = "drop"
  )

df <- group_by_at(df, vars(TrtID, GermTemp))

df <- df %>%
  summarise(
    Mean = mean(Time), sd = sd(Time),
    .groups = "drop"
  )

#Thermal Time Sub-optimal Model tests  -----------------------
