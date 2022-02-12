# ---- UI ---- #

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(shinyBS)
library(DT)



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

headerStyle <- "margin-top: 0px; border-bottom: 1px solid rgba(0, 0, 0, 0.25);"
bodyparts <- list()

bodyparts$intro <- list(
  h2("Population-based threshold models for seed germination analysis", style = headerStyle),
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
  tableOutput("columnDescriptions")
)

bodyparts$load <- list(
  h3("Upload data", style = headerStyle),
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

body <- dashboardBody(
  useShinyjs(),
  tabItems(
    tabItem("intro", bodyparts$intro),
    tabItem("load", bodyparts$load),
    tabItem("germTab", list(
      h3("Germination models", style = headerStyle),
      uiOutput("germUI"))),
    tabItem("ThermalTimeTab", list(
      h3("ThermalTime model", style = headerStyle),
      uiOutput("ThermalTimeUI"))),
    tabItem("HydroTimeTab", list(
      h3("HydroTime model", style = headerStyle),
      uiOutput("HydroTimeUI"))),
    tabItem("HydroThermalTimeTab", list(
      h3("HydroThermalTime model", style = headerStyle),
      uiOutput("HydroThermalTimeUI"))),
    tabItem("HydroPrimingTab", list(
      h3("HydroPriming model", style = headerStyle),
      uiOutput("HydroPrimingUI"))),
    tabItem("HydroThermalPrimingTab", list(
      h3("HydroThermalPriming model", style = headerStyle),
      uiOutput("HydroThermalPrimingUI"))),
    tabItem("AgingTab", list(
      h3("Aging model", style = headerStyle),
      uiOutput("AgingUI"))),
    tabItem("PromoterTab", list(
      h3("Promoter model", style = headerStyle),
      uiOutput("PromoterUI"))),
    tabItem("InhibitorTab", list(
      h3("Inhibitor model", style = headerStyle),
      uiOutput("InhibitorUI")))
  )
)



# Footer ------------------------------------------------------------------

footer <- list(
  div(
    align = "center",
    style = "font-size:small; color:grey; border-top:2px darkgrey; margin-top: 2em;",
    p("App developed by", a("Ben Bradford", href = "https://github.com/bzbradford")),
    p(
      "Based on the", a("PBTM R package", href = "https://github.com/pedrobello/pbtm"),
      "developed by", a("Pedro Bello", href = "https://github.com/pedrobello"),
      "and", a("Ben Bradford", href = "https://github.com/bzbradford")
    ),
    p(
      "Seed germination models developed by", a("Kent Bradford", href = "https://www.plantsciences.ucdavis.edu/people/kent-bradford"),
      "and", a("Pedro Bello", href = "https://www.plantsciences.ucdavis.edu/people/pedro-bello")
    ),
    p(a("Source code", href = "https://github.com/bzbradford/pbtm-app")
    )
  )
)



# Generate UI -------------------------------------------------------------

ui <- list(
  dashboardPage(header, sidebar, body),
  footer
)


# ui <- fluidPage(
#   
#   theme = shinytheme("flatly"),
#   useShinyjs(),
#   tags$head(tags$style(type = "text/css", ".container-fluid { max-width: 1000px; }")),
#   
#   fluidRow(
#     column(12, h2("Seed Germination Modeling App"))
#   ),
#   hr(style = "margin-top: 0px;"),
#   
#   h3("Introduction to the models"),
#   p("Population-based Threshold Models (PBTM) Calculator – version 0.2.0. This R package combines existing PBTMs calculations, parameters output and respective plots. The package uses a nonlinear least squares function (Bates and Watts, 1988; Bates and Chambers, 1992) for each model by comparing the raw data and respective treatments directly with the curves predicted by the selected model and minimizing the error."),
#   
#   h3("Required data structure"),
#   p("Proper data preparation is required to avoid issues when working with the pbtm package. The use of these templates is not required, and users can work with their own data objects as long as the column names are modified to mirror exactly (caps included) the column names described below (Table 1) or visible in the templates. The treatment information (e.g., germination water potential (Germ.wp) needs to be repeated for every cumulative data point (columns CumTime and CumFraction) for the pertinent treatment or these data points will not be used on calculations. Data should be properly filtered to work with the desired model."),
#   
#   br(),
#   
#   bsCollapse(
#     id = "data",
#     multiple = T,
#     open = c("load", "view", "cols"),
#     
#     bsCollapsePanel(
#       title = "Data format requirements",
#       value = "template",
#       p(em("You must upload your seed germination data as a CSV file. If you do not use the default column names shown below, you will have to specify which columns correspond to each of the expected data types.")),
#       br(),
#       p(strong("Download sample datasets:")),
#       p(
#         downloadButton("downloadTemplate", "Empty data template"),
#         downloadButton("downloadSampleGermData", "Sample germination dataset"),
#         downloadButton("downloadSamplePrimingData", "Sample priming dataset")
#       ),
#       br(),
#       p(strong("Dataset column names and descriptions:")),
#       tableOutput("columnDescriptions")
#     ),
#     
#     bsCollapsePanel(
#       title = "Data entry",
#       value = "load",
#       p(em("Upload your own data here or select one of our sample datasets to get started.")),
#       hr(),
#       p(strong("Load sample datasets")),
#       fluidRow(
#         column(12,
#           actionButton("loadSampleGermData", "Load germination sample data"),
#           actionButton("loadSamplePrimingData", "Load priming sample data"),
#           actionButton("clearData", "Clear loaded data")
#         )
#       ),
#       hr(),
#       fluidRow(
#         column(12, fileInput("userData", "Upload your own data", accept = c(".csv"))),
#       )
#     ),
#     
#     bsCollapsePanel(
#       title = "View current dataset",
#       value = "view",
#       fluidRow(
#         column(10, p(em("This is a preview of the currently loaded dataset. If it looks good, click 'OK' to match your column names to the expected data values for germination modeling. Column names matching the template dataset will be matched automatically."))),
#         column(2, actionButton("confirmDataView", "OK"), align = "right")
#       ),
#       hr(),
#       fluidRow(
#         column(12,
#           p(strong("Currently loaded dataset:")),
#           div(style = "overflow: auto;", uiOutput("currentDataDisplay"))
#         )
#       )
#     ),
#     
#     bsCollapsePanel(
#       title = "Define data columns",
#       value = "cols",
#       fluidRow(
#         column(10, p(em("If you used the same column names as the default data template, they will be automatically matched below. Otherwise, cast your column names into the appropriate data types. Warning messages will appear if your data doesn't match the expected type or range."))),
#         column(2, actionButton("confirmColView", "OK"), align = "right")
#       ),
#       hr(),
#       p(strong("Match required data to column names:")),
#       uiOutput("colSelectUI"),
#       # div(
#       #   lapply(1:nrow(columnValidation), function(i) {
#       #     wellPanel(
#       #       style = "display: inline-block; vertical-align: top; width: 100%; margin: 5px;",
#       #       uiOutput(paste0("colSelect", i)),
#       #       uiOutput(paste0("colValidate", i))
#       #     )
#       #   })
#       # ),
#       hr(),
#       fluidRow(
#         column(10, 
#           p(strong("Available models:")),
#           uiOutput("modelStatus")
#         ),
#         column(2, actionButton("confirmColView2", "OK"), align = "right")
#       )
#     )
#   ),
#   br(),
#   
#   h3("Model outputs and analysis"),
#   # actionButton("runModels", "Run Models"),
#   tabsetPanel(
#     tabPanel(
#       title = "Germination models",
#       uiOutput("germUI")
#     )
#     
#     # tabPanel(
#     #   title = "PlotRateVsTreat",
#     #   sliderInput("GRInput", "Select Growth Rate", min = 10, max = 90, value = 50, step = 10),
#     #   plotOutput("PlotRateVsTreat")
#     # )
#   ),
#   br(),
#   hr(),
#   div(
#     align = "center",
#     style = "font-size:small; color:grey; border-top:2px darkgrey",
#     p("App developed by", a("Ben Bradford", href = "https://github.com/bzbradford")),
#     p(
#       "Based on the", a("PBTM R package", href = "https://github.com/pedrobello/pbtm"),
#       "developed by", a("Pedro Bello", href = "https://github.com/pedrobello")
#     ),
#     p(
#       "Seed germination models developed by", a("Kent Bradford", href = "https://www.plantsciences.ucdavis.edu/people/kent-bradford"),
#       "and", a("Pedro Bello", href = "https://www.plantsciences.ucdavis.edu/people/pedro-bello")
#     )
#   )
# )



