# ---- Documentation panels ---- #
# Renders the md/*.md files into collapsible "About" panels shown at the top of
# each relevant tab. Rendering (markdown -> HTML) is cached since the files are
# static.

.docCache <- new.env(parent = emptyenv())

#' @description read and render a markdown file to cached HTML
#' @param path path to a .md file
#' @returns an HTML tag (or NULL if the file is missing)
renderMd <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  if (is.null(.docCache[[path]])) {
    txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    .docCache[[path]] <- shiny::markdown(txt)
  }
  .docCache[[path]]
}

#' @description a collapsible documentation panel for a tab
#' @param path path to the source markdown file
#' @param title accordion header text
#' @param open whether the panel starts expanded
#' @returns a bslib accordion, or NULL if the file is missing
docPanel <- function(path, title = "About this analysis", open = FALSE) {
  html <- renderMd(path)
  if (is.null(html)) return(NULL)
  accordion(
    accordion_panel(
      title = title,
      value = "doc",
      div(class = "doc-content", html)
    ),
    open = if (open) "doc" else FALSE,
    class = "doc-accordion"
  )
}
