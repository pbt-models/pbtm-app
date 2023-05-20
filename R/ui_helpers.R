
namedWell <- function(..., title = NULL) {
  div(
    div(class = "well-title", title),
    div(class = "well", ...)
  )
}

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
