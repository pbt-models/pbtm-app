# ---- Introduction content ---- #

# UI ----

IntroUI <- function() {
  ns <- NS("intro")
  
  tagList(
    h2("Seed germination modeling app", class = "tab-title"),
    h3("Introduction to the population-based models"),
    p("This app in an interactive easy-to-use implementation of the functions found in the PBTM R package (currently under development). Population-based threshold (PBT) models describe and predict seed germination performance under most conditions (Bradford and Bello 2022). These models are fueled by germination rate data collected after seed imbibition and under varying (measured) levels of the conditions of interest. The models use a nonlinear least squares function (Bates and Watts, 1988; Bates and Chambers, 1992) for each model by comparing the raw data and respective treatments directly with the curves predicted by the selected model and minimizing the error."),
    br(),
    h3("Required data structure"),
    p("Proper data preparation is required to avoid issues when working with this site or the PBTM package. The use of these data templates is not required, but if you use different column names you will have to match them to the expected names after uploading your data. Columns that specify treatment information (e.g. TrtID, GermWP) need to have the value of that treatment repeated for each member of the treatment. Each row (observation) also needs to have a value for CumTime (cumulative elapsed time) and CumFraction (fraction germinated as of that time point, ranges from 0 to 1)."),
    br(),
    h4("Sample datasets:"),
    div(
      class = "flex-btns",
      downloadButton(ns("downloadTemplate"), "Empty data template"),
      downloadButton(ns("downloadGermData"), "Sample germination dataset"),
      downloadButton(ns("downloadPrimingData"), "Sample priming dataset"),
      downloadButton(ns("downloadAgingData"), "Sample aging dataset")
    ),
    br(),
    h4("Expected column names and descriptions:"),
    div(
      style = "overflow: auto;",
      tableOutput(ns("columnDescriptions"))
    )
  )
}


# Server ----

#' @references colValidation
#' @references sampleTemplate
#' @references sampleGermData
#' @references samplePrimingData
#' @references sampleAgingData

IntroServer <- function() {
  moduleServer(
    id = "intro",
    function(input, output, session) {
      ns <- session$ns
      
      
      # Outputs ----
      
      ## columnDescriptions ----
      output$columnDescriptions <- renderTable({
        colValidation %>%
          select(
            `Default name` = Column,
            Description = LongDescription,
            `Data type` = TypeDescription,
            Usage
          )
      },
        spacing = "s"
      )

      
      # Download handlers ----
      
      ## downloadTemplate ----
      output$downloadTemplate <- downloadHandler(
        filename = "PBTM Data Template.csv",
        content = function(file) {
          write_csv(sampleTemplate, file)
        }
      )
      
      ## downloadGermData ----
      output$downloadGermData <- downloadHandler(
        filename = "PBTM Sample Germination Data.csv",
        content = function(file) {
          write_csv(sampleGermData, file)
        }
      )
      
      ## downloadPrimingData ----
      output$downloadPrimingData <- downloadHandler(
        filename = "PBTM Sample Priming Data.csv",
        content = function(file) {
          write_csv(samplePrimingData, file)
        }
      )
      
      ## downloadAgingData ----
      output$downloadAgingData <- downloadHandler(
        filename = "PBTM Sample Aging Data.csv",
        content = function(file) {
          write_csv(sampleAgingData, file)
        }
      )
      
    } # end
  )
}
