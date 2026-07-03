# ---- UI ---- #

ui <- page(
  title = "PBTM Dashboard",

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
    includeCSS("www/style.css"),
    useShinyjs(),
  ),

  tags$header(
    div(
      div(
        class = "header-content",
        h1("PBTM Dashboard"),
        a(
          icon("github"),
          title = "Source code",
          href = "https://github.com/pbt-models/pbtm-app",
          target = "_blank"
        )
      )
    )
  ),

  tags$main(
    do.call(
      navset_pill_list,
      c(
        list(id = "mainNav", widths = c(3, 9)), # tweak widths to taste
        list(
          nav_panel(
            "About",
            renderMd("md/_introduction.md"),
            tags$hr(),
            lapply(modelSpecs, function(m) {
              tagList(
                renderMd(m$doc),
                tags$hr(),
              )
            }),
            renderMd("md/_references.md")
          ),
          nav_panel(
            title = tagList(
              span(
                class = "nav-badge",
                uiOutput("LoadBadge", inline = TRUE)
              ),
              "Upload data"
            ),
            value = "nav_loadDataTab",
            loadDataUI()
          )
        ),
        lapply(modelNames, function(m) {
          nav_panel(
            title = tagList(
              span(
                class = "nav-badge",
                uiOutput(paste0(m, "Badge"), inline = TRUE)
              ),
              snakecase::to_any_case(m, case = "sentence")
            ),
            value = paste0("nav_", m, "Tab"),
            if (m %in% names(modelSpecs)) {
              modelUI(modelSpecs[[m]])
            } else {
              exec(paste0(m, "UI"))
            }
          )
        })
      )
    )
  ),

  # footer
  tags$footer(
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
    )
  )
)
