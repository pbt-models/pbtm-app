or <- function(...) {
  args <- list(...)
  for (arg in args) {
    if (truthy(arg)) return(arg)
  }
  return(NULL)
}
