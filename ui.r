# ---- UI ---- #

# create tab panels with the UI call for each tab
tabPanels <- lapply(
  c("Intro", "LoadData", modelNames),
  \(m) nav_panel_hidden(value = paste0(m, "Tab"), exec(paste0(m, "UI")))
)

# footer
footer <- div(
  align = "center",
  class = "wrapper",
  style = "font-size: small; color: grey; background-color: white;",
  br(),
  br(),
  p(
    "App developed by",
    a("Ben Bradford", href = "https://github.com/bzbradford"),
    "and",
    a("Pedro Bello", href = "https://github.com/pedrobello")
  ),
  p(
    "Based on the",
    a("PBTM R package", href = "https://github.com/pedrobello/pbtm"),
    "developed by",
    a("Pedro Bello", href = "https://github.com/pedrobello"),
    "and",
    a("Ben Bradford", href = "https://github.com/bzbradford")
  ),
  p(
    "Seed germination models developed by",
    a(
      "Kent Bradford",
      href = "https://www.plantsciences.ucdavis.edu/people/kent-bradford"
    ),
    "and",
    a(
      "Pedro Bello",
      href = "https://www.plantsciences.ucdavis.edu/people/pedro-bello"
    )
  ),
  p(a("Source code", href = "https://github.com/pbt-models/pbtm-app")),
  br()
)

# build ui
ui <- page_sidebar(
  title = "PBTM Dashboard",
  fillable = FALSE,
  theme = bs_theme(version = 5, primary = "#3c8dbc"),
  sidebar = sidebar(
    width = 250,
    actionLink("nav_IntroTab", "Introduction"),
    div(
      class = "nav-link-container",
      actionLink("nav_LoadDataTab", "Upload data"),
      uiOutput("LoadBadge", inline = TRUE)
    ),
    lapply(modelNames, function(m) {
      div(
        class = "nav-link-container",
        actionLink(paste0("nav_", m, "Tab"), snakecase::to_any_case(m, case = "sentence")),
        uiOutput(paste0(m, "Badge"), inline = TRUE)
      )
    })
  ),
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(
      name = "description",
      content = "Population-based threshold models for seed germination analysis."
    ),
    tags$meta(
      name = "keywords",
      content = "germination, statistics, analysis, population, threshold, model"
    ),
    includeHTML(("www/google-analytics.html")),
    includeHTML(("www/favicons.html")),
    includeCSS("www/style.css")
  ),
  useShinyjs(),
  do.call(navset_hidden, c(list(id = "mainNav"), tabPanels)),
  footer
)
