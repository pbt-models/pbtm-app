# ---- truthy ---- #

#' @param x a value to be evaluated as `TRUE` or `FALSE`
#' @returns boolean

truthy <- function(x) {
  if (is.null(x)) return(F)
  if (inherits(x, "try-error")) return(F)
  if (is.function(x)) return(T)
  if (is.data.frame(x)) return(nrow(x) > 0)
  if (length(x) == 0) return(F)
  if (all(is.na(x))) return(F)
  if (is.character(x) && !any(nzchar(stats::na.omit(x)))) return(F)
  if (is.logical(x) && !any(stats::na.omit(x))) return(F)
  if (!is.atomic(x)) return(TRUE)
  if (length(x) > 1) return(T)
  if (x %in% c(NA, "NA", "", 0, F)) return(F)
  T
}
