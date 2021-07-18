# ---- UI ---- #

library(shiny)
library(shinythemes)
library(shinyjs)
# library(shinyWidgets)
library(shinyBS)
library(DT)
# library(plotly)


ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  useShinyjs(),
  tags$head(tags$style(type = "text/css", ".container-fluid { max-width: 1000px; }")),
  
  fluidRow(
    column(12, h2("Seed Germination Modeling App"))
  ),
  hr(style = "margin-top: 0px;"),
  
  h3("Introduction to the models"),
  p("Population-based Threshold Models (PBTM) Calculator – version 0.2.0. This R package combines existing PBTMs calculations, parameters output and respective plots. The package uses a nonlinear least squares function (Bates and Watts, 1988; Bates and Chambers, 1992) for each model by comparing the raw data and respective treatments directly with the curves predicted by the selected model and minimizing the error."),
  
  h3("Required data structure"),
  p("Proper data preparation is required to avoid issues when working with the pbtm package. The use of these templates is not required, and users can work with their own data objects as long as the column names are modified to mirror exactly (caps included) the column names described below (Table 1) or visible in the templates. The treatment information (e.g., germination water potential (Germ.wp) needs to be repeated for every cumulative data point (columns CumTime and CumFraction) for the pertinent treatment or these data points will not be used on calculations. Data should be properly filtered to work with the desired model."),
  
  br(),
  
  bsCollapse(
    bsCollapsePanel(
      title = "Data format requirements",
      p(em("You must upload your seed germination data as a CSV file. If you do not use the default column names shown below, you will have to specify which columns correspond to each of the expected data types.")),
      br(),
      p(strong("Download sample datasets:")),
      p(
        downloadButton("downloadTemplate", "Empty data template"),
        downloadButton("downloadSampleGermData", "Sample germination dataset"),
        downloadButton("downloadSamplePrimingData", "Sample priming dataset")
      ),
      br(),
      p(strong("Dataset column names and descriptions:")),
      tableOutput("columnDescriptions")
    )
  ),
  
  bsCollapse(
    id = "data",
    open = "load",
    
    bsCollapsePanel(
      title = "Data entry",
      value = "load",
      p(em("Upload your own data here or select one of our sample datasets to get started.")),
      hr(),
      p(strong("Load sample datasets")),
      fluidRow(
        column(12,
          actionButton("loadSampleGermData", "Load germination sample data"),
          actionButton("loadSamplePrimingData", "Load priming sample data"),
          actionButton("clearData", "Clear loaded data")
        )
      ),
      hr(),
      fluidRow(
        column(12, fileInput("userData", "Upload your own data", accept = c(".csv"))),
      )
    ),
    
    bsCollapsePanel(
      title = "View current dataset",
      value = "view",
      fluidRow(
        column(10, p(em("This is a preview of the currently loaded dataset. If it looks good, click 'OK' to match your column names to the expected data values for germination modeling. Column names matching the template dataset will be matched automatically."))),
        column(2, actionButton("viewColumnMatching", "OK"), align = "right")
      ),
      hr(),
      fluidRow(
        column(12,
          p(strong("Currently loaded dataset:")),
          div(style = "overflow: auto;", uiOutput("currentDataDisplay"))
        )
      )
    ),
    
    bsCollapsePanel(
      title = "Define data columns",
      value = "cols",
      p(em("If you used the same column names as the default data template, they will be automatically matched below. Otherwise, cast your column names into the appropriate data types. Warning messages will appear if your data doesn't match the expected type or range.")),
      hr(),
      p(strong("Match required data to column names:")),
      div(
        lapply(1:nrow(columnDefaults), function(i) {
          wellPanel(
            style = "display: inline-block; vertical-align: top; width: 20em; margin: 5px;",
            uiOutput(paste0("colSelect", i)),
            uiOutput(paste0("colValidate", i))
          )
        })
      ),
      hr(),
      p(strong("Available models:")),
      uiOutput("modelStatus")
    )
  ),
  br(),

  
)
