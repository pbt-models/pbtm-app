# ---- UI ---- #

# App Theme --------------------------------------------------------------------
system_font_stack <- "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif"
app_theme <- bslib::bs_theme(
  version = 5,
  base_font = system_font_stack,
  heading_font = system_font_stack,
  primary = "#3f6fb0",
  "border-radius" = "0.5rem",
  "card-cap-bg" = "var(--bs-light)", # quiet card headers, not blue
  "card-border-color" = "rgba(0,0,0,.08)",
  font_scale = 0.95
) |>
  bslib::bs_add_variables("headings-font-weight" = "600")

# Main UI ----------------------------------------------------------------------
ui <- page(
  theme = app_theme,
  title = "PBTM Dashboard",

  #--- Head ---
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

  #--- Header ---
  tags$header(
    id = "header",
    div(
      class = "header-content",
      div(
        style = "display: inline-flex; align-items: center; gap: 1rem; justify-content: space-between; width: 100%;",
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

  #--- Main content ---
  tags$main(
    do.call(
      navset_underline,
      c(
        list(
          id = "mainNav",
          nav_panel(
            "About",
            div(
              renderMd("md/_introduction.md"),
              tags$hr(),
              lapply(modelSpecs, function(m) {
                tagList(
                  renderMd(m$doc),
                  tags$hr(),
                )
              }),
              renderMd("md/_references.md")
            )
          ),
          nav_panel(
            title = tagList(
              span(
                class = "nav-badge",
                uiOutput("badge_loadData", inline = TRUE)
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
              div(
                class = "nav-badge",
                uiOutput(paste0("badge_", m), inline = TRUE)
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

  #--- Footer ---
  tags$footer(
    id = "footer",
    div(
      class = "footer-content",
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
