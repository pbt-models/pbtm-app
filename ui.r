# ---- UI ---- #

ui <- page(
  title = "PBTM Dashboard",
  fillable = FALSE,
  class = "main",
  theme = bs_theme(
    version = 5,
    primary = "#3c8dbc",
    "body-bg" = "#ecf0f5",
    "card-border-color" = "#d2d6de"
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

  # navbar
  tags$nav(
    class = "navbar navbar-dark bg-primary",
    tags$span(class = "navbar-brand mb-0 ms-2", "PBTM Dashboard")
  ),

  # sidebar
  layout_sidebar(
    sidebar = sidebar(
      width = 250,
      bg = "#222d32",
      fg = "#b8c7ce",

      # intro
      div(
        class = "nav-link-container",
        # no badge for introduction
        span(class = "nav-badge"),
        actionLink("nav_IntroTab", "Introduction")
      ),

      # upload
      div(
        class = "nav-link-container",
        span(class = "nav-badge", uiOutput("LoadBadge", inline = TRUE)),
        actionLink("nav_LoadDataTab", "Upload data")
      ),

      # models
      lapply(modelNames, function(m) {
        div(
          class = "nav-link-container",
          span(
            class = "nav-badge",
            uiOutput(paste0(m, "Badge"), inline = TRUE)
          ),
          actionLink(
            paste0("nav_", m, "Tab"),
            snakecase::to_any_case(m, case = "sentence")
          )
        )
      })
    ),

    # initialize main content area
    # create tab panels with the UI call for each tab
    do.call(
      navset_hidden,
      c(
        list(id = "mainNav"),
        lapply(
          c("Intro", "LoadData", modelNames),
          \(m) nav_panel_hidden(value = paste0(m, "Tab"), exec(paste0(m, "UI")))
        )
      )
    )
  ),

  # footer
  div(
    align = "center",
    class = "main",
    style = "font-size: small; color: grey; background-color: #222d32; margin: 0; padding: 1rem;",
    div(
      "App developed by",
      a("Ben Bradford", href = "https://github.com/bzbradford"),
      "and",
      a("Pedro Bello", href = "https://github.com/pedrobello"),
      "Based on the",
      a("PBTM R package", href = "https://github.com/pbt-models/pbtm"),
      br(),
      "Seed germination models developed by",
      a(
        "Kent Bradford",
        href = "https://www.plantsciences.ucdavis.edu/people/kent-bradford"
      ),
      "and Pedro Bello",
      br(),
      a("Source code", href = "https://github.com/pbt-models/pbtm-app")
    ),
  )
)
