
#' @description creates a dashboard box with some common options
#' @param ... items to place in the box
#' @param width grid width of the box (1-12)
#' @param title box title

primaryBox <- function(..., width = 12, title = NULL) {
  box(
    ...,
    width = width,
    title = title,
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE
  )
}
