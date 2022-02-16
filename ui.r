# ---- UI ---- #

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  # library(shinythemes)
  library(shinyjs)
  # library(shinyBS)
  library(DT)
})



# Header ------------------------------------------------------------------

header <- dashboardHeader(
  title = "PBTM Dashboard"
)



# Sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "intro"),
    menuItemOutput("loadMenu"),
    menuItemOutput("germMenu"),
    lapply(modelNames, function(m) {
      menuItemOutput(paste0(m, "Menu"))
    })
  )
)



# Body --------------------------------------------------------------------

intro <- list(
  h2("Population-based threshold models for seed germination analysis", class = "tab-title"),
  h3("Introduction to the models"),
  p("This app in an interactive easy-to-use implementation of the functions found in the PBTM R package (currently under development).  The models use a nonlinear least squares function (Bates and Watts, 1988; Bates and Chambers, 1992) for each model by comparing the raw data and respective treatments directly with the curves predicted by the selected model and minimizing the error."),
  br(),
  h3("Required data structure"),
  p("Proper data preparation is required to avoid issues when working with this site or the PBTM package. The use of these data templates is not required, but if you use different column names you will have to match them to the expected names after uploading your data. Columns that specify treatment information (e.g. TrtID, GermWP) need to have the value of that treatment repeated for each member of the treatment. Each row (observation) also needs to have a value for CumTime (cumulative elapsed time) and CumFraction (fraction germinated as of that time point, ranges from 0 to 1)."),
  br(),
  h4("Sample datasets:"),
  p(
    downloadButton("downloadTemplate", "Empty data template"),
    downloadButton("downloadSampleGermData", "Sample germination dataset"),
    downloadButton("downloadSamplePrimingData", "Sample priming dataset")
  ),
  br(),
  h4("Expected column names and descriptions:"),
  div(style = "overflow: auto;", tableOutput("columnDescriptions"))
)

load <- list(
  h3("Upload data", class = "tab-title"),
  p(em("Upload your own data here or select one of our sample datasets to get started.")),
  hr(),
  p(strong("Sample datasets:")),
  p(
    actionButton("loadSampleGermData", "Load germination sample data"),
    actionButton("loadSamplePrimingData", "Load priming sample data")
  ),
  br(),
  p(fileInput("userData", "Upload your own data:", accept = c(".csv"))),
  p(strong("Start over:")),
  actionButton("clearData", "Clear loaded data"),
  hr(),
  uiOutput("currentDataDisplay")
)

tabs <- append(
  list(
    tabItem("intro", intro),
    tabItem("load", load),
    tabItem("germTab", list(
      h3("Germination models", class = "tab-title"),
      uiOutput("germUI")))
  ),
  lapply(modelNames, function(m) {
    tabItem(paste0(m, "Tab"), list(
      h3(m, class = "tab-title"),
      uiOutput(paste0(m, "UI"))
    ))
  })
)

body <- dashboardBody(
  tags$head(includeHTML(("google-analytics.html"))),
  tags$head(includeHTML(("favicons.html"))),
  includeCSS("style.css"),
  useShinyjs(),
  do.call(tabItems, tabs)
)



# Footer ------------------------------------------------------------------

footer <- list(
  div(
    align = "center",
    class = "wrapper",
    style = "font-size: small; color: grey; background-color: white;",
    br(), br(),
    p("App developed by", a("Ben Bradford", href = "https://github.com/bzbradford")),
    p("Based on the", a("PBTM R package", href = "https://github.com/pedrobello/pbtm"), "developed by", a("Pedro Bello", href = "https://github.com/pedrobello"), "and", a("Ben Bradford", href = "https://github.com/bzbradford")),
    p("Seed germination models developed by", a("Kent Bradford", href = "https://www.plantsciences.ucdavis.edu/people/kent-bradford"), "and", a("Pedro Bello", href = "https://www.plantsciences.ucdavis.edu/people/pedro-bello")),
    p(a("Source code", href = "https://github.com/bzbradford/pbtm-app")),
    br()
  )
)



# Generate UI -------------------------------------------------------------

ui <- list(
  dashboardPage(header, sidebar, body),
  footer
)
