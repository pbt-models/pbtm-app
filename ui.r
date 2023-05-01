# ---- UI ---- #


# Dashboard layout ----

## Header ----

header <- dashboardHeader(
  title = "PBTM Dashboard"
)


## Sidebar ----

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


## Body ----

tabs <- append(
  list(
    tabItem("intro", introTabUI()),
    tabItem("load", loadDataTabUI()),
    tabItem("germTab", list(
      h3("Germination rate analysis", class = "tab-title"),
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
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "description", content = "Population-based threshold models for seed germination analysis."),
    tags$meta(name = "keywords", content = "germination, statistics, analysis, population, threshold, model"),
    includeHTML(("www/google-analytics.html")),
    includeHTML(("www/favicons.html")),
    includeCSS("www/style.css")
    ),
  useShinyjs(),
  do.call(tabItems, tabs)
)


## Footer ----

footer <- list(
  div(
    align = "center",
    class = "wrapper",
    style = "font-size: small; color: grey; background-color: white;",
    br(), br(),
    p("App developed by", a("Ben Bradford", href = "https://github.com/bzbradford"), "and", a("Pedro Bello", href = "https://github.com/pedrobello")),
    p("Based on the", a("PBTM R package", href = "https://github.com/pedrobello/pbtm"), "developed by", a("Pedro Bello", href = "https://github.com/pedrobello"), "and", a("Ben Bradford", href = "https://github.com/bzbradford")),
    p("Seed germination models developed by", a("Kent Bradford", href = "https://www.plantsciences.ucdavis.edu/people/kent-bradford"), "and", a("Pedro Bello", href = "https://www.plantsciences.ucdavis.edu/people/pedro-bello")),
    p(a("Source code", href = "https://github.com/bzbradford/pbtm-app")),
    br()
  )
)



# Generate UI ----

ui <- tagList(
  dashboardPage(header, sidebar, body),
  footer
)
