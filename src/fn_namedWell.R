
#' @description creates a well panel with a title above
#' @param ... items to include in the well panel
#' @param title text to place above the well panel

namedWell <- function(..., title = NULL) {
  div(
    div(class = "well-title", title),
    div(class = "well", ...)
  )
}
