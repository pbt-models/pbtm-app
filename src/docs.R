# ---- Documentation panels ---- #
# Renders the md/*.md files into collapsible "About" panels shown at the top of
# each relevant tab. Rendering (markdown -> HTML) is cached since the files are
# static.

.docCache <- new.env(parent = emptyenv())

#' @description read and render a markdown file to cached HTML
#' @param path path to a .md file
#' @returns an HTML tag (or NULL if the file is missing)
renderMd <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(NULL)
  }
  if (is.null(.docCache[[path]])) {
    txt <- paste(
      readLines(path, warn = FALSE, encoding = "UTF-8"),
      collapse = "\n"
    )
    .docCache[[path]] <- shiny::markdown(txt)
  }
  .docCache[[path]]
}

#' Builds the 'More information' link that pops up the modal
#' @param md path to the markdown file with more information
#' @param title optional title attribute for the link
#' @returns HTML
build_modal_link <- function(md, title = "More information") {
  if (is.null(md)) {
    return()
  }
  onclick <- sprintf(
    "Shiny.setInputValue('show_modal', {md: '%s'}, { priority: 'event' });",
    md
  )
  shiny::HTML(
    sprintf(
      "<b><a style='cursor:pointer' title='%s' onclick=\"%s\">More information.</a></b>",
      title,
      onclick
    )
  )
}

#' Shows a markdown file in a modal popup
#' @param md markdown file to display
#' @param title optional modal title
show_modal <- function(md) {
  html <- renderMd(md)
  if (is.null(html)) {
    return(NULL)
  }
  m <- modalDialog(
    html,
    footer = modalButton("Close"),
    easyClose = TRUE,
    size = "xl"
  )
  showModal(m)
}
