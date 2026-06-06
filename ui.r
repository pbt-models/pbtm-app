# ---- UI ---- #

ui <- page(
  title = "PBTM Dashboard",
  fillable = FALSE,
  class = "main",
  theme = bs_theme(
    version = 5,
    # primary chosen for WCAG-AA contrast (~6:1) with white card-header text;
    # the old #3c8dbc was only ~2.9:1
    primary = "#15679c",
    "body-bg" = "#eef2f6",
    "card-border-color" = "#d2d6de",
    "border-radius" = "0.375rem"
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
      fg = "#cdd7dd",

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
    # create tab panels with the UI call for each tab. Models with a spec are
    # built by the factory (modelUI); Intro / LoadData / Germination keep their
    # bespoke {Name}UI() functions.
    do.call(
      navset_hidden,
      c(
        list(id = "mainNav"),
        lapply(
          c("Intro", "LoadData", modelNames),
          \(m) {
            nav_panel_hidden(
              value = paste0(m, "Tab"),
              if (m %in% names(modelSpecs)) {
                modelUI(modelSpecs[[m]])
              } else {
                exec(paste0(m, "UI"))
              }
            )
          }
        )
      )
    )
  ),

  # footer
  div(
    align = "center",
    class = "main app-footer",
    style = "font-size: small; color: #b8c7ce; background-color: #222d32; margin: 0; padding: 1rem;",
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
