# ---- app ---- #

library(shiny)

source("global.r")
source("ui.r")
source("server.r")

shinyApp(ui, server)
